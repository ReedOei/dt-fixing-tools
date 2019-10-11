select distinct ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha,ftf.failures,ftf.rounds
FROM fs_rq1_tests_with_first_sha ftf;
