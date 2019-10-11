  select distinct fttut.commit_sha,fttut.module
    from fs_test_to_uniq_test fttut
      join fs_test_commit_order ftco on ftco.commit_sha = fttut.commit_sha
        join fs_idflakies_vers_results fivr on fivr.test_name = fttut.orig_test_name
	  where ftco.order_num > -1;
