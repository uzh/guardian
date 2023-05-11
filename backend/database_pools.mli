exception Exception of string

type connection_type =
  | SinglePool of string
  | MultiPools of (string * string) list

module type ConfigSig = sig
  val database : connection_type
  val database_pool_size : int
end

module DefaultConfig : ConfigSig

module type Sig = Database_pools_sig.Sig

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
