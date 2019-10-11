  select distinct ftco.commit_sha,fttut.module
    from fs_experiment fe
      join fs_test_commit_order ftco on ftco.short_sha = fe.short_sha
        join fs_idflakies_vers_results fivr on fivr.test_name = ftco.test_name
	  join fs_test_to_uniq_test fttut on fttut.commit_sha = ftco.commit_sha and fttut.orig_test_name = ftco.test_name
	    where ftco.order_num > -1 and fe.test_file_is_empty > 0;