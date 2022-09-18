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
