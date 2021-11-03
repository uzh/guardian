module type Backend_store_s = sig
  val get_roles : Uuidm.t -> (Role_set.t, string) result

  val get_perms : Authorizer.actor_spec -> Authorizer.auth_rule list

  val put_perm : Authorizer.auth_rule -> (unit, string) result

  val grant_roles : Uuidm.t -> Role_set.t -> (unit, string) result

  val create_entity : ?id:Uuidm.t -> Role_set.t -> (unit, string) result

  val mem_entity : Uuidm.t -> bool

  val get_owner : Uuidm.t -> (Uuidm.t option, string) result

  val set_owner : Uuidm.t -> owner:Uuidm.t -> (unit, string) result
end

module Make(BES : Backend_store_s) = struct
  include BES
  let get_entity ~(typ : 'kind) id =
    if BES.mem_entity id
    then
      let ( let* ) = Result.bind in
      let* roles = BES.get_roles id in
      (** TODO: owner *)
      (* let* owner_id = BES.get_owner id in
         let* owner_roles = BES.get_roles owner_id in
         let* owner = get_entity ~typ:() owner_id in *)
      Ok(Entity.make ~roles ~typ id)
    else
      Error(Printf.sprintf "Entity %s doesn't exist." (Uuidm.to_string id))

  (** [put_perms perms] adds all the permissions [perms] to the backend. If
      there is an error at any point, it returns a `result` containing all of
      the items that were not added. *)
  let put_perms perms =
    List.fold_left
      (fun acc x ->
         match acc with
         | Ok acc' ->
           begin match BES.put_perm x with
             | Ok() -> Ok(x :: acc')
             | Error _ -> Error [x]
           end
         | Error xs ->
           Error(x :: xs)
      )
      (Ok [])
      perms

  (** This convenience function should be used to decorate the [to_entity]
    * functions of authorizable modules. The newly decorated function connects
    * to the persistent backend to ensure that the entity's roles and ownership
    * are consistent in both spaces.
  *)
  let decorate_to_entity
      (to_entity : 'a -> 'kind Entity.t)
    : 'a -> ('kind Entity.t, string) result =
    fun x ->
    let (ent : 'kind Entity.t) = to_entity x in
    let uuid = ent.uuid in
    let ( let* ) = Result.bind in
    if BES.mem_entity ent.uuid
    then
      let* ent' = get_entity ~typ:ent.typ ent.uuid in
      let roles = Role_set.union ent.roles ent'.roles in
      let* () = BES.grant_roles uuid roles in
      Result.Ok Entity.{uuid; roles; owner = ent.owner; typ = ent.typ}
    else
      let* () = BES.create_entity ~id:uuid ent.roles in
      Ok ent

  let get_checker entity =
    let auth_rules =
      Role_set.elements entity.Entity.roles
      |> List.map (fun r -> `Role r)
      |> List.cons (`Uniq entity.Entity.uuid)
      |> List.map BES.get_perms
      |> List.flatten
    in
    fun actor action ->
      let is_owner =
        match entity.Entity.owner with
        | Some x -> Entity.(actor.uuid = x.uuid)
        | None -> false
      in
      let is_self = Entity.(actor.uuid = entity.uuid) in
      if is_self || is_owner
      then true
      else
        let actor_roles = actor.Entity.roles in
        List.exists
          (fun (actor', action', _) ->
             match actor' with
             | `Uniq id ->
               actor.uuid = id && action = action'
             | `Role role ->
               Role_set.mem role actor_roles && action = action'
          )
          auth_rules
end
