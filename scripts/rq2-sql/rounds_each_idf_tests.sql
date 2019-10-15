select idf_rounds,count(idf_rounds) from fs_tests_found_only_in_first_sha_mapping_failures group by idf_rounds order by idf_rounds ;
-- 1|1
-- 17|1
-- 22|2
-- 100|69
-- 101|24
