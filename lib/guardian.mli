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
end

module Contract : sig
  module Uuid = Uuid
end

module Make : functor (ActorRoles : RoleSig) (TargetRoles : RoleSig) -> sig
  module Uuid = Uuid
  module Action = Action
  module RoleSet : Role_set.Core with type elt = ActorRoles.t

  module ActorSpec : sig
    type t =
      | Entity of ActorRoles.t
      | Id of ActorRoles.t * Uuid.Actor.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val value : ActorRoles.t -> string
    val is_valid : t -> t -> bool
  end

  module TargetSpec : sig
    type t =
      | Entity of TargetRoles.t
      | Id of TargetRoles.t * Uuid.Target.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val value : TargetRoles.t -> string
    val is_valid : t -> t -> bool
  end

  module Rule : sig
    type t = ActorSpec.t * Action.t * TargetSpec.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or

    module Set : CCSet.S with type elt = t
  end

  module Effect : sig
    type t = Action.t * TargetSpec.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val create : 'a -> 'b -> 'a * 'b
    val is_valid : t -> t -> bool
  end

  module EffectSet : sig
    type t =
      | And of t list
      | Or of t list
      | One of Effect.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val and_ : t list -> t
    val or_ : t list -> t
    val one : Effect.t -> t
  end

  module Actor : sig
    type 'a t

    val id : 'a t -> Uuid.Actor.t
    val owner : 'a t -> Uuid.Actor.t option
    val roles : 'a t -> RoleSet.t
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t

    val of_yojson
      :  (Yojson.Safe.t -> 'a Ppx_deriving_yojson_runtime.error_or)
      -> Yojson.Safe.t
      -> 'a t Ppx_deriving_yojson_runtime.error_or

    val show : 'a t -> string
    val pp : Format.formatter -> 'a t -> unit
    val make : ?owner:Uuid.Actor.t -> RoleSet.t -> 'a -> Uuid.Actor.t -> 'a t
    val a_owns_b : 'a t -> 'b t -> bool
    val has_role : 'a t -> ActorRoles.t -> bool
  end

  module type ActorSig = sig
    type t

    val to_authorizable
      :  ?ctx:context
      -> t
      -> (ActorRoles.t Actor.t, string) Lwt_result.t
  end

  module Target : sig
    type 'a t

    val id : 'a t -> Uuid.Target.t
    val owner : 'a t -> Uuid.Actor.t option
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t

    val of_yojson
      :  (Yojson.Safe.t -> 'a Ppx_deriving_yojson_runtime.error_or)
      -> Yojson.Safe.t
      -> 'a t Ppx_deriving_yojson_runtime.error_or

    val show : 'a t -> string
    val pp : Format.formatter -> 'a t -> unit
    val make : ?owner:Uuid.Actor.t -> 'a -> Uuid.Target.t -> 'a t
  end

  module type TargetSig = sig
    type t

    val to_authorizable
      :  ?ctx:context
      -> t
      -> (TargetRoles.t Target.t, string) Lwt_result.t
  end

  module Authorizer : sig
    val check_effect
      :  ?tags:Logs.Tag.set
      -> Rule.t list
      -> 'a Actor.t
      -> Effect.t
      -> (unit, string) result

    val actor_in_rule : 'a Actor.t -> Rule.t -> bool
    val actor_in_rule_res : 'a Actor.t -> Rule.t -> (unit, string) result

    val can_for_rules
      :  ?any_of:bool
      -> Rule.t list
      -> 'a Actor.t
      -> (unit, string) result
  end

  module type PersistenceSig =
    Persistence.Contract
      with type 'a actor = 'a Actor.t
       and type 'b target = 'b Target.t
       and type actor_spec = ActorSpec.t
       and type effect = Effect.t
       and type effect_set = EffectSet.t
       and type kind = TargetRoles.t
       and type role_set = RoleSet.t
       and type roles = ActorRoles.t
       and type rule = Rule.t
       and type target_spec = TargetSpec.t

  module MakePersistence : functor
    (Backend : Persistence.Backend
                 with type 'a actor = 'a Actor.t
                  and type 'b target = 'b Target.t
                  and type actor_spec = ActorSpec.t
                  and type effect = Effect.t
                  and type effect_set = EffectSet.t
                  and type kind = TargetRoles.t
                  and type role_set = RoleSet.t
                  and type roles = ActorRoles.t
                  and type rule = Rule.t
                  and type target_spec = TargetSpec.t)
    -> PersistenceSig
end [@warning "-67"]
