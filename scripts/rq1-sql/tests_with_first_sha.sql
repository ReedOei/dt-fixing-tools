  select distinct ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha,SUM(ftf.failures),SUM(ftf.rounds)
    FROM fs_idflakies_vers_results fivr
      JOIN fs_test_commit_order ftco on ftco.test_name = fivr.test_name
        JOIN flaky_test_failures ftf on ftf.test_name = fivr.test_name and fivr.commit_sha = ftf.commit_sha
	  WHERE ftco.order_num > -1
	    group by ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha;
