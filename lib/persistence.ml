module type Backend_store_s = sig
  val get_roles : Uuidm.t -> (Role_set.t, string) Lwt_result.t

  val get_perms : Authorizer.actor_spec -> (Authorizer.auth_rule list, string) Lwt_result.t

  val put_perm : Authorizer.auth_rule -> (unit, string) Lwt_result.t

  val delete_perm : Authorizer.auth_rule -> (unit, string) Lwt_result.t

  val grant_roles : Uuidm.t -> Role_set.t -> (unit, string) Lwt_result.t

  val create_authorizable : id:Uuidm.t -> ?owner:Uuidm.t -> Role_set.t -> (unit, string) Lwt_result.t

  val mem_authorizable : Uuidm.t -> bool Lwt.t

  val get_owner : Uuidm.t -> (Uuidm.t option, string) Lwt_result.t

  val set_owner : Uuidm.t -> owner:Uuidm.t -> (unit, string) Lwt_result.t
end

module type S = sig
  include Backend_store_s
  val get_authorizable : typ:'kind -> Uuidm.t -> ('kind Authorizable.t, string) Lwt_result.t
  val put_perms : Authorizer.auth_rule list -> (Authorizer.auth_rule list, Authorizer.auth_rule list) Lwt_result.t
  val decorate_to_authorizable : ('a -> 'kind Authorizable.t) -> 'a -> ('kind Authorizable.t, string) Lwt_result.t
  val get_checker :
    'a Authorizable.t ->
    ('b Authorizable.t -> Action.t -> bool, string) Lwt_result.t
end

let ( let* ) = Lwt_result.bind

module Make(BES : Backend_store_s) : S = struct
  include BES
  let rec get_typeless_authorizable id : (unit Authorizable.t, string) Lwt_result.t =
    if%lwt BES.mem_authorizable id
      then
        let* roles = BES.get_roles id in
        let* owner_id = BES.get_owner id in
        let* owner =
          match owner_id with
          | Some owner_id' ->
            let* x = get_typeless_authorizable owner_id' in
            Lwt.return_ok(Some x)
          | None ->
            Lwt.return_ok(None)
        in
        Lwt.return_ok(Authorizable.make ~roles ~typ:() ?owner id)
      else
        Lwt.return(Error(Printf.sprintf "Authorizable %s doesn't exist." (Uuidm.to_string id)))
  let get_authorizable ~(typ : 'kind) id =
    if%lwt BES.mem_authorizable id
    then
      let* roles = BES.get_roles id in
      let* owner_id = BES.get_owner id in
      let* owner =
        match owner_id with
        | Some owner_id' -> Lwt_result.map (Option.some) (get_typeless_authorizable owner_id')
        | None -> Lwt_result.return(None)
      in
      Lwt.return(Ok(Authorizable.make ~roles ~typ ?owner id))
    else
      Lwt.return(Error(Printf.sprintf "Authorizable %s doesn't exist." (Uuidm.to_string id)))

  (** [put_perms perms] adds all the permissions [perms] to the backend. If
      there is an error at any point, it returns a `result` containing all of
      the items that were not added. *)
  let put_perms perms =
    List.fold_left
      (fun acc x ->
         match%lwt acc with
         | Ok acc' ->
           begin match%lwt BES.put_perm x with
             | Ok() -> Lwt.return_ok(x :: acc')
             | Error _ -> Lwt.return_error[x]
           end
         | Error xs ->
           Lwt.return_error(x :: xs)
      )
      (Lwt.return_ok [])
      perms

  (** This convenience function should be used to decorate the [to_authorizable]
    * functions of authorizable modules. The newly decorated function connects
    * to the persistent backend to ensure that the authorizable's roles and ownership
    * are consistent in both spaces.
  *)
  let decorate_to_authorizable
      (to_authorizable : 'a -> 'kind Authorizable.t)
    : 'a -> ('kind Authorizable.t, string) Lwt_result.t =
    fun x ->
    let (ent : 'kind Authorizable.t) = to_authorizable x in
    let uuid = ent.uuid in
    if%lwt BES.mem_authorizable ent.uuid
    then
      let* ent' = get_authorizable ~typ:ent.typ ent.uuid in
      let roles = Role_set.union ent.roles ent'.roles in
      let* () = BES.grant_roles uuid roles in
      let* owner =
        match ent.owner, ent'.owner with
        | Some owner, None ->
          let* () = BES.set_owner ent.uuid ~owner:owner.uuid in
          Lwt.return_ok(Some owner)
        | None, Some owner ->
          Lwt.return_ok(Some owner)
        | None, None ->
          Lwt.return_ok None
        | Some x, Some y when x <> y ->
          Lwt_result.fail(
            "decorate_to_authorizable: both the database and the decorated function \
              returned distinct values for the owner of authorizable "
            ^ Uuidm.to_string ent.uuid)
        | Some x, Some _ (* when x = y *) ->
          Lwt.return_ok(Some x)
      in
      Lwt.return_ok Authorizable.{uuid; roles; owner; typ = ent.typ}
    else
      let* () = BES.create_authorizable ~id:uuid ent.roles in
      Lwt.return_ok ent

  let get_checker authorizable =
    let%lwt auth_rules =
      Role_set.elements authorizable.Authorizable.roles
      |> List.map (fun r -> `Role r)
      |> List.cons (`Uniq authorizable.Authorizable.uuid)
      |> Lwt_list.map_s BES.get_perms
    in
    let* auth_rules =
      List.fold_left
        (fun acc x ->
          let%lwt acc = acc in
          match acc, x with
          | Ok acc, Ok perms ->
            Lwt.return_ok(perms @ acc)
          | Error err, _
          | _, Error err ->
            Lwt.return_error err)
        (Lwt.return_ok [])
        auth_rules
    in
    Lwt.return_ok @@
    fun actor action ->
      let is_owner =
        match authorizable.Authorizable.owner with
        | Some x -> Authorizable.(actor.uuid = x.uuid)
        | None -> false
      in
      let is_self = Authorizable.(actor.uuid = authorizable.uuid) in
      if is_self || is_owner
      then true
      else
        let actor_roles = actor.Authorizable.roles in
        List.exists
          (fun (actor', action', _) ->
             match actor' with
             | `Uniq id ->
               actor.uuid = id && action = action'
             | `Role role ->
               Role_set.mem role actor_roles && (action = action' || action' = `Manage)
          )
          auth_rules
end
