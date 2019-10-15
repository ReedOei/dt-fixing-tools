insert into flaky_test
(
  name,
  intended_id,
  revealed_id,
  commit_sha,
  class_test_name
)
values
(
  ?,
  ?,
  ?,
  ?,
  ?
)