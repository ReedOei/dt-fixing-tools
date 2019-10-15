select ftf.subject_name, ftf.test_name, ftf.round_type, ftf.flaky_type, ftf.failures, ftf.rounds, ftf.commit_sha
from fs_rq1_first_sha_flaky_info ftf;