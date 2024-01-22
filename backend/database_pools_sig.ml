module type Sig = sig
  val initialize : unit -> unit

  val fetch_pool
    :  ?ctx:(string * string) list
    -> unit
    -> (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t

  val add_pool : ?pool_size:int -> string -> string -> unit
  val drop_pool : string -> unit Lwt.t

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

  val transaction
    :  ?ctx:(string * string) list
    -> (Caqti_lwt.connection -> 'a)
    -> 'a Lwt.t

  val exec_with_connection
    :  ('a, unit, [< `Zero ]) Caqti_request.t
    -> 'a
    -> (module Caqti_lwt.CONNECTION)
    -> unit Lwt.t

  val populate
    :  ?ctx:(string * string) list
    -> string
    -> string list
    -> 'a Caqti_type.t
    -> 'a list
    -> unit Lwt.t
end
