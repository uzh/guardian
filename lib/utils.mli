val hide_typ : Format.formatter -> 'a -> unit

val with_exn
  :  ?ctx:'a
  -> (?ctx:'a -> 'b -> ('c, string) result Lwt.t)
  -> string
  -> 'b
  -> 'c Lwt.t

val decompose_variant_string : string -> string * string list
val invalid_role : ?msg_prefix:string -> string * string list -> string
val failwith_invalid_role : ?msg_prefix:string -> string * string list -> 'a

module Dynparam : sig
  type t = Pack : 'a Caqti_type.t * 'a -> t

  val empty : t
  val add : 'a Caqti_type.t -> 'a -> t -> t
end

val deny_message_uuid : Uuid.Actor.t -> Permission.t -> Uuid.Target.t -> string

val deny_message_for_str_target
  :  Uuid.Actor.t
  -> Permission.t
  -> string
  -> string

val deny_message_validation_set : Uuid.Actor.t -> string -> string
