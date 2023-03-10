module Make (Core : Role.Sig) (Id : Uuid.Sig) = struct
  type t =
    | Entity of Core.t
    | Id of Core.t * Id.t
  [@@deriving eq, show, ord, yojson]

  let value = Core.show

  let is_valid (target : t) (spec : t) =
    match target, spec with
    | Entity t_entity, Entity s_entity | Id (t_entity, _), Entity s_entity ->
      Core.equal t_entity s_entity
    | Entity _, Id (_, _) -> false
    | Id (t_entity, t_uuid), Id (s_entity, s_uuid) ->
      Core.equal t_entity s_entity && Id.equal t_uuid s_uuid
  ;;
end
