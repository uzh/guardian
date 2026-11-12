(* The rights guardian's statements need. Grant them to the roles guardian
   runs as (see {!Guardian_backend.Capabilities} in the interface). *)

let read = Veins.Capability.create "guardian_read"
let write = Veins.Capability.create "guardian_write"
let schema = Veins.Capability.create "guardian_schema"
let all = [ read; write; schema ]
