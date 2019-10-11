  select ftf.subject_name, ftf.test_name, ftf.round_type, ftf.flaky_type, ftf.failures, ftf.rounds, ftf.commit_sha
    from flaky_test_failures ftf
      join fs_test_commit_order ftco on ftco.commit_sha = ftf.commit_sha
        where ftco.order_num > -1;