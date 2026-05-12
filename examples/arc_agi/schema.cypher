// KuzuDB schema for ARC-AGI-3 worldview graph.
// Created idempotently by worldview.Worldview.init_schema().

CREATE NODE TABLE IF NOT EXISTS Observation (
    id STRING,
    episode INT64,
    frame_n INT64,
    grid_hash STRING,
    score INT64,
    state STRING,
    ts TIMESTAMP,
    PRIMARY KEY (id)
);

CREATE NODE TABLE IF NOT EXISTS Entity (
    id STRING,
    color INT64,
    bbox_r0 INT64, bbox_c0 INT64, bbox_r1 INT64, bbox_c1 INT64,
    centroid_y DOUBLE, centroid_x DOUBLE,
    area INT64,
    shape_hash STRING,
    PRIMARY KEY (id)
);

CREATE NODE TABLE IF NOT EXISTS Action (
    id STRING,
    code INT64,
    x INT64,
    y INT64,
    PRIMARY KEY (id)
);

CREATE NODE TABLE IF NOT EXISTS Hypothesis (
    id STRING,
    dsl STRING,
    action_code INT64,
    support INT64,
    refute INT64,
    status STRING,        // 'candidate' | 'promoted' | 'rejected'
    created_at TIMESTAMP,
    PRIMARY KEY (id)
);

CREATE NODE TABLE IF NOT EXISTS Rule (
    id STRING,
    prolog_src STRING,
    promoted_at TIMESTAMP,
    game_id STRING,
    PRIMARY KEY (id)
);

CREATE REL TABLE IF NOT EXISTS CONTAINS (FROM Observation TO Entity);

CREATE REL TABLE IF NOT EXISTS TRANSITION (
    FROM Observation TO Observation,
    action_id STRING,
    action_code INT64,
    reward INT64
);

CREATE REL TABLE IF NOT EXISTS SUPPORTS (FROM Hypothesis TO Observation);

CREATE REL TABLE IF NOT EXISTS REFUTES (FROM Hypothesis TO Observation);

CREATE REL TABLE IF NOT EXISTS PROMOTED_TO (FROM Hypothesis TO Rule);

// ---------------------------------------------------------------------------
// Kernel ontology layer.
// Concepts are universal across games. Edges below let the reasoner ask
// "which entities in the current frame have affordance X?" deterministically,
// regardless of which game/episode produced the evidence.
// ---------------------------------------------------------------------------

CREATE NODE TABLE IF NOT EXISTS Concept (
    id STRING,
    name STRING,
    category STRING,    // 'spatial' | 'affordance' | 'goal' | 'causal' | 'temporal'
    is_kernel BOOLEAN,
    PRIMARY KEY (id)
);

CREATE NODE TABLE IF NOT EXISTS Provenance (
    id STRING,
    kind STRING,        // 'sensor' | 'induction' | 'llm'
    confidence DOUBLE,
    evidence_count INT64,
    created_at TIMESTAMP,
    PRIMARY KEY (id)
);

CREATE NODE TABLE IF NOT EXISTS GoalCandidate (
    id STRING,
    description STRING,
    support INT64,
    PRIMARY KEY (id)
);

// concept-to-concept taxonomy edge ("MoveUp is a kind of Movable")
CREATE REL TABLE IF NOT EXISTS KIND_OF (FROM Concept TO Concept);

// affordance assertion: this entity exhibits this concept
CREATE REL TABLE IF NOT EXISTS IS_A (FROM Entity TO Concept);

// classification: a hypothesis/rule is a concrete form of a concept
CREATE REL TABLE IF NOT EXISTS HYP_INSTANCE_OF (FROM Hypothesis TO Concept);
CREATE REL TABLE IF NOT EXISTS RULE_INSTANCE_OF (FROM Rule TO Concept);

CREATE REL TABLE IF NOT EXISTS HAS_PROVENANCE (FROM Rule TO Provenance);

CREATE REL TABLE IF NOT EXISTS GOAL_KIND (FROM GoalCandidate TO Concept);

CREATE REL TABLE IF NOT EXISTS GOAL_DERIVED_FROM (FROM GoalCandidate TO Hypothesis);

// ---------------------------------------------------------------------------
// Cluster layer — synthetic entities that aggregate ColorBlobs by various
// criteria. Lets the induction layer reason about avatars, regions, palette
// classes, and rigid-body rotations instead of fragmented color-components.
// ---------------------------------------------------------------------------

CREATE NODE TABLE IF NOT EXISTS Cluster (
    id STRING,
    kind STRING,          // 'motion' | 'proximity' | 'color_group' | 'rotation' | 'physics' | ...
    subkind STRING,       // for kind='physics': 'gravity' | 'collision' | 'destruction' | ...
    signature STRING,     // stable hash across frames (for re-identification)
    n_members INT64,
    bbox_r0 INT64, bbox_c0 INT64, bbox_r1 INT64, bbox_c1 INT64,
    centroid_y DOUBLE, centroid_x DOUBLE,
    velocity_dy INT64, velocity_dx INT64,
    rotation_deg INT64,
    discovered_at_obs STRING,
    PRIMARY KEY (id)
);

CREATE REL TABLE IF NOT EXISTS PART_OF (FROM Entity TO Cluster);

CREATE REL TABLE IF NOT EXISTS OBSERVED_AS (FROM Observation TO Cluster);

CREATE REL TABLE IF NOT EXISTS CLUSTER_INSTANCE_OF (FROM Cluster TO Concept);

// Temporal identity edge: links a cluster in frame t to its successor in t+1.
// change_kind ∈ {persisted, translated, transformed}. Absence of any outgoing
// NEXT_CLUSTER edge from a cluster = it vanished. Absence of any incoming edge
// for a cluster = it appeared.
CREATE REL TABLE IF NOT EXISTS NEXT_CLUSTER (
    FROM Cluster TO Cluster,
    action_code INT64,
    change_kind STRING,
    dy DOUBLE,
    dx DOUBLE
);

// Physics-cluster causal/explanatory edges: a Mark cluster is EXPLAINED_BY the
// MotionCluster that produced it; a Collision is EXPLAINED_BY the two motions
// that converged.
CREATE REL TABLE IF NOT EXISTS EXPLAINED_BY (
    FROM Cluster TO Cluster,
    role STRING
);

// ---------------------------------------------------------------------------
// Shape layer — one node per distinct shape_hash, cached across all frames.
// Holds the actual bitmap so the LLM (later) can name the shape, and a slot
// for the inferred semantic role (Agent / Goal / Collectible / Obstacle ...).
// ---------------------------------------------------------------------------

CREATE NODE TABLE IF NOT EXISTS Shape (
    shape_hash STRING,
    bitmap_ascii STRING,    // multi-line text of '1' (filled) / '0' (empty)
    height INT64,
    width INT64,
    area INT64,
    n_observations INT64,
    first_seen_at TIMESTAMP,
    llm_name STRING,
    llm_confidence DOUBLE,
    role_concept STRING,    // populated by Name → Concept classifier
    PRIMARY KEY (shape_hash)
);

CREATE REL TABLE IF NOT EXISTS HAS_SHAPE (FROM Entity TO Shape);

// ---------------------------------------------------------------------------
// GoalAttempt — persists which candidate goals have been tried and whether
// they produced a reward, so Prolog backtracking can skip already-failed ones
// across replans and episodes.
// ---------------------------------------------------------------------------

CREATE NODE TABLE IF NOT EXISTS GoalAttempt (
    id STRING,
    goal_signature STRING,    // stable hash: source + rounded coords
    centroid_y DOUBLE,
    centroid_x DOUBLE,
    source STRING,
    attempted_at TIMESTAMP,
    frames_invested INT64,
    reward_delta INT64,
    status STRING,            // 'pending' | 'no_reward' | 'success'
    PRIMARY KEY (id)
);
