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
  val invalid_role : ?msg_prefix:string -> string * string list -> string

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

  val deny_message_for_str_target
    :  Uuid.Actor.t
    -> Permission.t
    -> string
    -> string

  val deny_message_validation_set : Uuid.Actor.t -> string -> string
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
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val model : TargetModel.t -> t
    val id : Uuid.Target.t -> t
    val is_id : t -> bool
    val find_id : t -> Uuid.Target.t option
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
    val sexp_of_t : t -> Sexplib0.Sexp.t
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
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val create : ?target_uuid:Uuid.Target.t -> Uuid.Actor.t -> Role.t -> t
    val role_to_human : t -> string
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
    val sexp_of_t : t -> Sexplib0.Sexp.t
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
    val sexp_of_t : t -> Sexplib0.Sexp.t
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
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val create_for_model : Uuid.Actor.t -> Permission.t -> TargetModel.t -> t
    val create_for_id : Uuid.Actor.t -> Permission.t -> Uuid.Target.t -> t
  end

  module PermissionOnTarget : sig
    type t =
      { permission : Permission.t
      ; model : TargetModel.t
      ; target_uuid : Uuid.Target.t option
      }

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val sexp_of_t : t -> Sexplib0.Sexp.t

    val create
      :  ?target_uuid:Uuid.Target.t
      -> Permission.t
      -> TargetModel.t
      -> t

    val of_tuple : Permission.t * TargetModel.t * Uuid.Target.t option -> t
    val remove_duplicates : t list -> t list

    val filter_permission_on_model
      :  Permission.t
      -> TargetModel.t
      -> t list
      -> t list

    val validate : ?any_id:bool -> t -> t list -> bool

    val permission_of_model
      :  Permission.t
      -> TargetModel.t
      -> t list
      -> bool * Uuid.Target.t list
  end

  module ValidationSet : sig
    type t =
      | And of t list
      | Or of t list
      | One of PermissionOnTarget.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val and_ : t list -> t
    val or_ : t list -> t
    val one : PermissionOnTarget.t -> t
    val one_of_tuple : Permission.t * TargetModel.t * Uuid.Target.t option -> t
    val empty : t
  end

  module type PersistenceSig =
    Persistence.Contract
      with type actor = Actor.t
       and type actor_model = ActorModel.t
       and type actor_permission = ActorPermission.t
       and type actor_role = ActorRole.t
       and type permission_on_target = PermissionOnTarget.t
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
                    and type permission_on_target = PermissionOnTarget.t
                    and type role = Role.t
                    and type role_permission = RolePermission.t
                    and type target = Target.t
                    and type target_entity = TargetEntity.t
                    and type target_model = TargetModel.t
                    and type validation_set = ValidationSet.t)
      -> PersistenceSig
end [@warning "-67"]
