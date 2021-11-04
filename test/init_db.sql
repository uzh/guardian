CREATE TABLE entities (
    id TEXT UNIQUE NOT NULL,
    roles TEXT NOT NULL,
    parent TEXT
);

CREATE TABLE rules (
    actor_id TEXT,
    actor_role TEXT,
    act TEXT NOT NULL,
    target_id TEXT,
    target_role TEXT,
    CONSTRAINT only_one_actor
        CHECK(
            (actor_id IS NULL OR actor_role IS NULL) 
            and (actor_id IS NOT NULL OR actor_role IS NOT NULL)
        ),
    CONSTRAINT only_one_target
        CHECK(
            (target_id IS NULL OR target_role IS NULL) 
            and (target_id IS NOT NULL OR target_role IS NOT NULL)
        ),
    UNIQUE(actor_role, act, target_role)
);
