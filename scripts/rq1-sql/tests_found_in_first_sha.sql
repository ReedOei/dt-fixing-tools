  select distinct ftf.test_name
    from flaky_test_failures ftf
      join fs_test_commit_order ftco on ftf.commit_sha = ftco.commit_sha
        join (
	    select distinct ft.name as test_name
	        from flaky_test ft
		    join (
		          select distinct ft.class_test_name,ft.name
			        from (
				        select distinct ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha,SUM(ftf.failures),SUM(ftf.rounds)
					        from fs_test_to_uniq_test fttut
						        join fs_experiment fe on fe.test_name = fttut.uniq_test_name
							        JOIN fs_test_commit_order ftco on ftco.commit_sha = fttut.commit_sha
								        join fs_idflakies_vers_results fivr on fivr.test_name = fttut.orig_test_name
									        JOIN flaky_test_failures ftf on ftf.test_name = fivr.test_name and fivr.commit_sha = ftf.commit_sha
										        where ftco.order_num > -1 and fe.test_file_is_empty > 0
											        group by ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha
												      ) p
												            join flaky_test ft on ft.name = p.test_name
													        ) p on p.class_test_name = ft.class_test_name
														  ) p on p.test_name = ftf.test_name
														    where ftco.order_num > -1;
