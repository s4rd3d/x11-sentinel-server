-- :select_user_by_user_id
SELECT
  user_id,
  event_count,
  created_at,
  updated_at
FROM xss.users
WHERE (user_id = $1 AND
       deleted_at IS NULL)

-- :insert_user
INSERT INTO xss.users
  (user_id,
  event_count,
  created_at,
  updated_at)
VALUES ($1, $2, $3, $4)

-- :update_user_event_count
UPDATE xss.users
SET event_count = $1,
    updated_at = $2
WHERE(user_id = $3 AND
      deleted_at IS NULL)

-- :soft_delete_user_by_user_id
UPDATE xss.users
SET updated_at = $1,
    deleted_at = $2
WHERE (user_id = $3 AND
       deleted_at IS NULL)

-- :select_stream_by_stream_id
SELECT
  stream_id,
  session_id,
  user_id,
  created_at,
  updated_at
FROM xss.streams
WHERE (stream_id = $1 AND
       deleted_at IS NULL)

-- :insert_stream
INSERT INTO xss.streams
  (stream_id,
   session_id,
   user_id,
   created_at,
   updated_at)
VALUES ($1, $2, $3, $4, $5)

-- :soft_delete_stream_by_stream_id
UPDATE xss.streams
SET updated_at = $1,
    deleted_at = $2
WHERE (stream_id = $3 AND
       deleted_at IS NULL)

-- :select_session_by_session_id
SELECT
  session_id,
  created_at,
  updated_at
FROM xss.sessions
WHERE (session_id = $1 AND
       deleted_at IS NULL)

-- :insert_session
INSERT INTO xss.sessions
  (session_id,
   created_at,
   updated_at)
VALUES ($1, $2, $3)

-- :soft_delete_session_by_session_id
UPDATE xss.sessions
SET updated_at = $1,
    deleted_at = $2
WHERE (session_id = $3 AND
       deleted_at IS NULL)

-- :select_chunk_by_stream_id_and_sequence_number
SELECT
  stream_id,
  sequence_number,
  session_id,
  user_id,
  epoch_unit,
  epoch_value,
  submitted_at,
  real_ip_address,
  peer_ip_address,
  referer,
  chunk,
  created_at,
  updated_at
FROM xss.chunks
WHERE (stream_id = $1 AND
       sequence_number = $2 AND
       deleted_at IS NULL)

-- :insert_chunk
INSERT INTO xss.chunks
  (stream_id,
  sequence_number,
  session_id,
  user_id,
  epoch_unit,
  epoch_value,
  submitted_at,
  real_ip_address,
  peer_ip_address,
  referer,
  chunk,
  created_at,
  updated_at)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)

-- :soft_delete_chunk_by_stream_id_and_sequence_number
UPDATE xss.chunks
SET updated_at = $1,
    deleted_at = $2
WHERE (stream_id = $3 AND
       sequence_number = $4 AND
       deleted_at IS NULL)

-- :select_user_session_by_user_id_and_session_id
SELECT
  user_id,
  session_id
FROM xss.users_sessions
WHERE (user_id = $1 AND
       session_ID = $2)

-- :insert_user_session
INSERT INTO xss.users_sessions
  (user_id,
   session_id)
VALUES ($1, $2)
