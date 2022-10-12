-- :select_user_by_user_id
SELECT
  user_id,
  event_count,
  created_at,
  updated_at
FROM xss.users
WHERE (user_id = $1 AND
       deleted_at IS NULL)

-- :select_users
SELECT
  user_id,
  event_count,
  created_at,
  updated_at
FROM xss.users
WHERE (deleted_at IS NULL)

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
  created_at,
  updated_at
FROM xss.streams
WHERE (stream_id = $1 AND
       deleted_at IS NULL)

-- :select_latest_stream_by_user_id
SELECT
  s.stream_id,
  s.session_id,
  s.created_at,
  s.updated_at
FROM xss.streams s
     INNER JOIN xss.users_streams us
     ON s.stream_id = us.stream_id
WHERE (us.user_id = $1 AND
       s.deleted_at IS NULL)
ORDER BY s.created_at DESC LIMIT 1

-- :insert_stream
INSERT INTO xss.streams
  (stream_id,
   session_id,
   created_at,
   updated_at)
VALUES ($1, $2, $3, $4)

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

-- :select_chunks_by_user_id
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
WHERE (user_id = $1 AND
       deleted_at IS NULL)

-- :select_sequence_numbers_by_stream_id
SELECT
  sequence_number
FROM xss.chunks
WHERE (stream_id = $1 AND
       deleted_at IS NULL)
ORDER BY sequence_number DESC

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
       session_id = $2)

-- :insert_user_session
INSERT INTO xss.users_sessions
  (user_id,
   session_id)
VALUES ($1, $2)

-- :select_user_stream_by_user_id_and_stream_id
SELECT
  user_id,
  stream_id
FROM xss.users_streams
WHERE (user_id = $1 AND
       stream_id = $2)

-- :insert_user_stream
INSERT INTO xss.users_streams
  (user_id,
   stream_id)
VALUES ($1, $2)

-- :select_profile_by_profile_id
SELECT
  profile_id,
  user_id,
  profile_data,
  succeeded_at,
  failed_at,
  created_at,
  updated_at
FROM xss.profiles
WHERE (profile_id = $1 AND
       deleted_at IS NULL)

-- :select_latest_profile_by_user_id
SELECT
  profile_id,
  user_id,
  profile_data,
  succeeded_at,
  failed_at,
  created_at,
  updated_at
FROM xss.profiles
WHERE (user_id = $1 AND
       deleted_at IS NULL)
ORDER BY created_at DESC LIMIT 1

-- :insert_profile
INSERT INTO xss.profiles
  (profile_id,
   user_id,
   created_at,
   updated_at)
VALUES ($1, $2, $3, $4)

-- :update_profile_success
UPDATE xss.profiles
SET profile_data = $1,
    succeeded_at = $2,
    updated_at = $3
WHERE (profile_id = $4 AND
       deleted_at IS NULL)

-- :update_profile_failure
UPDATE xss.profiles
SET failed_at = $1,
    updated_at = $2
WHERE (profile_id = $3 AND
       deleted_at IS NULL)

-- :cleanup_profiles
UPDATE xss.profiles
SET failed_at = $1,
    updated_at = $2
WHERE (succeeded_at IS NULL AND
       failed_at IS NULL)

-- :soft_delete_profile_by_profile_id
UPDATE xss.profiles
SET updated_at = $1,
    deleted_at = $2
WHERE (profile_id = $3 AND
       deleted_at IS NULL)

-- :select_verification_by_verification_id
SELECT
  verification_id,
  profile_id,
  stream_id,
  last_chunk,
  chunk_count,
  result,
  succeeded_at,
  failed_at,
  created_at,
  updated_at
FROM xss.verifications
WHERE (verification_id = $1 AND
       deleted_at IS NULL)

-- :select_verifications_by_profile_id
SELECT
  verification_id,
  profile_id,
  stream_id,
  last_chunk,
  chunk_count,
  result,
  succeeded_at,
  failed_at,
  created_at,
  updated_at
FROM xss.verifications
WHERE (profile_id = $1 AND
       deleted_at IS NULL)

-- :select_latest_succeeded_verification_by_user_id
SELECT
  v.verification_id,
  v.profile_id,
  v.stream_id,
  v.last_chunk,
  v.chunk_count,
  v.result,
  v.succeeded_at,
  v.failed_at,
  v.created_at,
  v.updated_at
FROM
    xss.profiles p
    INNER JOIN xss.verifications v
    ON p.profile_id = v.profile_id
WHERE (p.user_id = $1 AND
       p.deleted_at IS NULL AND
       v.deleted_at IS NULL AND
       v.succeeded_at IS NOT NULL)
ORDER BY v.succeeded_at DESC
LIMIT 1

-- :select_verifications_by_user_id_and_threshold
SELECT
  v.verification_id,
  v.result,
  v.succeeded_at,
  p.user_id
FROM
    xss.profiles p
    INNER JOIN xss.verifications v
    ON p.profile_id = v.profile_id
WHERE (p.user_id = $1 AND
       p.deleted_at IS NULL AND
       v.deleted_at IS NULL AND
       v.succeeded_at IS NOT NULL AND
       v.result <= $2)

-- :select_verifications_and_user_id_by_threshold
SELECT
  v.verification_id,
  v.result,
  v.succeeded_at,
  p.user_id
FROM
    xss.profiles p
    INNER JOIN xss.verifications v
    ON p.profile_id = v.profile_id
WHERE (p.deleted_at IS NULL AND
       v.deleted_at IS NULL AND
       v.succeeded_at IS NOT NULL AND
       v.result <= $1)

-- :insert_verification
INSERT INTO xss.verifications
  (verification_id,
   profile_id,
   stream_id,
   last_chunk,
   chunk_count,
   created_at,
   updated_at)
VALUES ($1, $2, $3, $4, $5, $6, $7)

-- :update_verification_success
UPDATE xss.verifications
SET result = $1,
    succeeded_at = $2,
    updated_at = $3
WHERE (verification_id = $4 AND
       deleted_at IS NULL)

-- :update_verification_failure
UPDATE xss.verifications
SET failed_at = $1,
    updated_at = $2
WHERE (verification_id = $3 AND
       deleted_at IS NULL)

-- :cleanup_verifications
UPDATE xss.verifications
SET failed_at = $1,
    updated_at = $2
WHERE (succeeded_at IS NULL AND
       failed_at IS NULL)

-- :soft_delete_verification_by_verification_id
UPDATE xss.verifications
SET updated_at = $1,
    deleted_at = $2
WHERE (verification_id = $3 AND
       deleted_at IS NULL)

-- :select_events_aggregated_by_day
SELECT
  date_trunc('day', submitted_at) "day",
  sum(jsonb_array_length(chunk))
FROM  xss.chunks
WHERE deleted_at IS NULL
GROUP BY "day"
ORDER BY "day" DESC

-- :select_events_by_user_id_aggregated_by_day
SELECT
  date_trunc('day', submitted_at) "day",
  sum(jsonb_array_length(chunk))
FROM
  xss.chunks
WHERE (user_id = $1 AND
       deleted_at IS NULL)
GROUP BY "day"
ORDER BY "day" DESC

-- :select_event_count_by_stream_id
SELECT
  sum(jsonb_array_length(chunk))
FROM
  xss.chunks
WHERE (stream_id = $1 AND
       deleted_at IS NULL)
