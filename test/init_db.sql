CREATE TABLE entities (
    id TEXT UNIQUE NOT NULL,
    roles TEXT NOT NULL,
    parent TEXT
);

CREATE TABLE role_rules (
    actor_role TEXT NOT NULL,
    act TEXT NOT NULL,
    target_role TEXT NOT NULL
);

CREATE TABLE individual_rules (
    actor_id TEXT NOT NULL,
    act TEXT NOT NULL,
    target_id TEXT NOT NULL
);
