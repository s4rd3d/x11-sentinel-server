BEGIN;

CREATE SCHEMA xss;

GRANT USAGE ON SCHEMA xss TO xss;

CREATE TABLE xss.users (
    user_id VARCHAR NOT NULL PRIMARY KEY,
    event_count INTEGER NOT NULL,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL,
    deleted_at TIMESTAMP NULL
);

CREATE TABLE xss.sessions (
    session_id VARCHAR NOT NULL PRIMARY KEY,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL,
    deleted_at TIMESTAMP NULL
);

CREATE TABLE xss.users_sessions (
    user_id VARCHAR NOT NULL REFERENCES xss.users(user_id) ON UPDATE CASCADE,
    session_id VARCHAR NOT NULL REFERENCES xss.sessions(session_id) ON UPDATE CASCADE,
    CONSTRAINT users_sessions_id PRIMARY KEY(user_id, session_id)
);

CREATE TABLE xss.streams (
    stream_id VARCHAR NOT NULL PRIMARY KEY,
    session_id VARCHAR NOT NULL,
    user_id VARCHAR NOT NULL,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL,
    deleted_at TIMESTAMP NULL,

    FOREIGN KEY(user_id) REFERENCES xss.users(user_id),
    FOREIGN KEY(session_id) REFERENCES xss.sessions(session_id)
);

CREATE TABLE xss.chunks (
    stream_id VARCHAR NOT NULL,
    sequence_number INT NOT NULL,
    session_id VARCHAR NOT NULL,
    user_id VARCHAR NOT NULL,
    epoch_unit VARCHAR NOT NULL,
    epoch_value INTEGER NOT NULL,
    submitted_at TIMESTAMP NOT NULL,
    real_ip_address INET NOT NULL,
    peer_ip_address INET NOT NULL,
    referer VARCHAR NOT NULL,
    chunk JSONB NOT NULL,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL,
    deleted_at TIMESTAMP NULL,

    FOREIGN KEY(user_id) REFERENCES xss.users(user_id),
    FOREIGN KEY(session_id) REFERENCES xss.sessions(session_id),
    FOREIGN KEY(stream_id) REFERENCES xss.streams(stream_id),

    CONSTRAINT chunk_id PRIMARY KEY(session_id, sequence_number)
);

GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA xss TO xss;

COMMIT;
