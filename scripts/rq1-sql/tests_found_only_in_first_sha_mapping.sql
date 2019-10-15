  select distinct fps.idf_test_name, fps.idf_sha, fps.idf_module, fps.first_test_name, fps.first_sha,fps.first_module
  from fs_prior_sha_to_idf_sha fps
  join fs_test_commit_order ftco on ftco.commit_sha = fps.first_sha and fps.idf_test_name = ftco.test_name;