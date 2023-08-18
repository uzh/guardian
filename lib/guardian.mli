type context = (string * string) list

module type RoleSig = Role.Sig

module Utils : sig
  val with_exn
    :  ?ctx:'a
    -> (?ctx:'a -> 'b -> ('c, string) result Lwt.t)
    -> string
    -> 'b
    -> 'c Lwt.t

  val decompose_variant_string : string -> string * string list
  val failwith_invalid_role : ?msg_prefix:string -> string * string list -> 'a

  module Dynparam : sig
    type t = Pack : 'a Caqti_type.t * 'a -> t

    val empty : t
    val add : 'a Caqti_type.t -> 'a -> t -> t
  end

  val deny_message_uuid
    :  Uuid.Actor.t
    -> Permission.t
    -> Uuid.Target.t
    -> string

  val deny_message_model : Uuid.Actor.t -> Permission.t -> string -> string
end

module Contract : sig
  module Uuid = Uuid
end

module Make : functor
  (ActorModel : RoleSig)
  (Role : RoleSig)
  (TargetModel : RoleSig)
  -> sig
  module Uuid = Uuid
  module Permission = Permission

  module TargetEntity : sig
    type t =
      | Model of TargetModel.t
      | Id of Uuid.Target.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Actor : sig
    type t =
      { uuid : Uuid.Actor.t
      ; model : ActorModel.t
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val create : ActorModel.t -> Uuid.Actor.t -> t
  end

  module ActorRole : sig
    type t =
      { actor_uuid : Uuid.Actor.t
      ; role : Role.t
      ; target_uuid : Uuid.Target.t option
      }

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val create : ?target_uuid:Uuid.Target.t -> Uuid.Actor.t -> Role.t -> t
  end

  module type ActorSig = sig
    type t

    val to_authorizable : ?ctx:context -> t -> (Actor.t, string) Lwt_result.t
  end

  module Target : sig
    type t =
      { uuid : Uuid.Target.t
      ; model : TargetModel.t
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val create : TargetModel.t -> Uuid.Target.t -> t
  end

  module type TargetSig = sig
    type t

    val to_authorizable : ?ctx:context -> t -> (Target.t, string) Lwt_result.t
  end

  module RolePermission : sig
    type t =
      { role : Role.t
      ; permission : Permission.t
      ; model : TargetModel.t
      }

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val create : Role.t -> Permission.t -> TargetModel.t -> t
  end

  module ActorPermission : sig
    type t =
      { actor_uuid : Uuid.Actor.t
      ; permission : Permission.t
      ; target : TargetEntity.t
      }

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val create_for_model : Uuid.Actor.t -> Permission.t -> TargetModel.t -> t
    val create_for_id : Uuid.Actor.t -> Permission.t -> Uuid.Target.t -> t
  end

  module ValidationSet : sig
    type t =
      | And of t list
      | Or of t list
      | One of Permission.t * TargetEntity.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val and_ : t list -> t
    val or_ : t list -> t
    val one : Permission.t * TargetEntity.t -> t
    val empty : t
  end

  module type PersistenceSig =
    Persistence.Contract
      with type actor = Actor.t
       and type actor_model = ActorModel.t
       and type actor_permission = ActorPermission.t
       and type actor_role = ActorRole.t
       and type role = Role.t
       and type role_permission = RolePermission.t
       and type target = Target.t
       and type target_entity = TargetEntity.t
       and type target_model = TargetModel.t
       and type validation_set = ValidationSet.t

  module MakePersistence : functor
    (Backend : Persistence.Backend
                 with type actor = Actor.t
                  and type actor_model = ActorModel.t
                  and type actor_permission = ActorPermission.t
                  and type actor_role = ActorRole.t
                  and type role = Role.t
                  and type role_permission = RolePermission.t
                  and type target = Target.t
                  and type target_entity = TargetEntity.t
                  and type target_model = TargetModel.t
                  and type validation_set = ValidationSet.t)
    -> PersistenceSig
end [@warning "-67"]
