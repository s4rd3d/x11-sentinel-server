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
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL,
    deleted_at TIMESTAMP NULL,

    FOREIGN KEY(session_id) REFERENCES xss.sessions(session_id)
);

CREATE TABLE xss.users_streams (
    user_id VARCHAR NOT NULL REFERENCES xss.users(user_id) ON UPDATE CASCADE,
    stream_id VARCHAR NOT NULL REFERENCES xss.streams(stream_id) ON UPDATE CASCADE,
    CONSTRAINT users_streams_id PRIMARY KEY(user_id, stream_id)
);

CREATE TABLE xss.chunks (
    stream_id VARCHAR NOT NULL,
    sequence_number INT NOT NULL,
    session_id VARCHAR NOT NULL,
    user_id VARCHAR NOT NULL,
    epoch_unit VARCHAR NOT NULL,
    epoch_value BIGINT NOT NULL,
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

    CONSTRAINT chunk_id PRIMARY KEY(stream_id, sequence_number)
);

CREATE TABLE xss.profiles (
    profile_id VARCHAR NOT NULL PRIMARY KEY,
    user_id VARCHAR NOT NULL,
    profile_data BYTEA NULL,
    succeeded_at TIMESTAMP NULL,
    failed_at TIMESTAMP NULL,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL,
    deleted_at TIMESTAMP NULL,

    FOREIGN KEY(user_id) REFERENCES xss.users(user_id)
);

CREATE TABLE xss.verifications (
    verification_id VARCHAR NOT NULL PRIMARY KEY,
    profile_id VARCHAR NOT NULL,
    stream_id VARCHAR NOT NULL,
    last_chunk INT NOT NULL,
    chunk_count INT NOT NULL,
    result DOUBLE PRECISION NULL,
    succeeded_at TIMESTAMP NULL,
    failed_at TIMESTAMP NULL,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL,
    deleted_at TIMESTAMP NULL,

    FOREIGN KEY(profile_id) REFERENCES xss.profiles(profile_id),
    FOREIGN KEY(stream_id) REFERENCES xss.streams(stream_id)
);

GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA xss TO xss;

COMMIT;
