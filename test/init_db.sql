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
    target_role TEXT
);
