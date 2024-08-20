exception Exception of string

module type ConfigSig = sig
  val database : string * string
  val database_pool_size : int
  val expected_databases : int
end

module DefaultConfig : ConfigSig

module type Sig = Database_pools_sig.Sig

module Make : functor (Config : ConfigSig) -> sig
  include Sig

  val query
    :  ?ctx:(string * string) list
    -> (Caqti_lwt.connection -> ('a, Caqti_error.t) result Lwt.t)
    -> 'a Lwt.t
end
[@@warning "-67"]
