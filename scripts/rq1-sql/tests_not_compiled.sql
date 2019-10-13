select distinct ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha,ftf.failures,ftf.rounds,ftf.perc_fail
from fs_rq1_tests_did_not_compile ftf;
