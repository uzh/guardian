module type Backend_store_s = sig
  val get_roles : Uuidm.t -> (Role_set.t, string) result

  val get_perms : Authorizer.actor_spec -> Authorizer.auth_rule list

  val grant_roles : Uuidm.t -> Role_set.t -> (unit, string) result

  val create_entity : ?id:Uuidm.t -> Role_set.t -> (unit, string) result

  val mem_entity : Uuidm.t -> bool

  val get_owner : Uuidm.t -> (Uuidm.t option, string) result

  val set_owner : Uuidm.t -> owner:Uuidm.t -> (unit, string) result
end

module Make(BES : Backend_store_s) = struct
  include BES
  let get_entity ~(typ : 'kind) id =
    let rv: ('kind Entity.t, string) result =
      if BES.mem_entity id
      then
        let ( let* ) = Result.bind in
        let* roles = BES.get_roles id in
        (* let* owner_id = BES.get_owner id in
           let* owner_roles = BES.get_roles owner_id in
           let* owner = get_entity ~typ:() owner_id in *)
        Ok(Entity.make ~roles ~typ id)
      else
        Error(Printf.sprintf "Entity %s doesn't exist." (Uuidm.to_string id))
    in
    rv

  (** This convenience function should be used to decorate the [to_entity]
    * functions of authorizable modules.
  *)
  let decorate_to_entity (to_entity : 'a -> 'kind Entity.t) : 'a -> ('kind Entity.t, string) result =
    fun x ->
    let (ent : 'kind Entity.t) = to_entity x in
    let uuid = ent.uuid in
    let ( let* ) = Result.bind in
    if BES.mem_entity ent.uuid
    then
      let* ent' = get_entity ~typ:ent.typ ent.uuid in
      let roles = Role_set.union ent.roles ent'.roles in
      let* () = BES.grant_roles uuid roles in
      (** TODO: must save the entity if the roles or owner are different *)
      Result.Ok Entity.{uuid; roles; owner = ent.owner; typ = ent.typ}
    else
      let* () = BES.create_entity ~id:uuid ent.roles in
      Ok ent

  let get_checker entity actions : Authorizer.auth_rule list =
    Role_set.elements entity.Entity.roles
    |> List.map (fun r -> `Role r)
    |> List.cons (`Uniq entity.Entity.uuid)
    |> List.map BES.get_perms
    |> List.flatten
    |> List.filter (fun (_, action, _) -> List.mem action actions)
end
