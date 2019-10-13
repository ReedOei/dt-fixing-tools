select distinct ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha,ftf.perc_fail
from fs_rq1_tests_found_in_first_sha_details ftf;
