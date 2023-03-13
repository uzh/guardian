exception Exception of string

type connection_type =
  | SinglePool of string
  | MultiPools of (string * string) list

module type ConfigSig = sig
  val database : connection_type
  val database_pool_size : int
end

module DefaultConfig : ConfigSig

module type Sig = sig
  val initialize : unit -> unit

  val fetch_pool
    :  ?ctx:(string * string) list
    -> unit
    -> (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t

  val add_pool : ?pool_size:int -> string -> string -> unit

  val find
    :  ?ctx:(string * string) list
    -> ('a, 'b, [< `One ]) Caqti_request.t
    -> 'a
    -> 'b Lwt.t

  val find_opt
    :  ?ctx:(string * string) list
    -> ('a, 'b, [< `One | `Zero ]) Caqti_request.t
    -> 'a
    -> 'b option Lwt.t

  val collect
    :  ?ctx:(string * string) list
    -> ('a, 'b, [< `Many | `One | `Zero ]) Caqti_request.t
    -> 'a
    -> 'b list Lwt.t

  val exec
    :  ?ctx:(string * string) list
    -> ('a, unit, [< `Zero ]) Caqti_request.t
    -> 'a
    -> unit Lwt.t
end

module Make : functor (Config : ConfigSig) -> sig
  include Sig

  val transaction
    :  ?ctx:(string * string) list
    -> (Caqti_lwt.connection -> 'a)
    -> 'a Lwt.t

  val transaction'
    :  ?ctx:(string * string) list
    -> (Caqti_lwt.connection -> ('a, [< Caqti_error.t ]) result)
    -> 'a Lwt.t

  val query
    :  ?ctx:(string * string) list
    -> (Caqti_lwt.connection -> 'a Lwt.t)
    -> 'a Lwt.t

  val query'
    :  ?ctx:(string * string) list
    -> (Caqti_lwt.connection -> ('a, [< Caqti_error.t ]) result Lwt.t)
    -> 'a Lwt.t
end
[@@warning "-67"]
