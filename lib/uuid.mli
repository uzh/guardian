module Actor : Sig.Uuid
module Target : Sig.Uuid

val actor_of_target : Target.t -> Actor.t
val target_of_actor : Actor.t -> Target.t
