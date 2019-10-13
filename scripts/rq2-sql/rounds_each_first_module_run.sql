select first_rounds,count(distinct first_module) from fs_tests_found_only_in_first_sha_mapping_failures group by first_rounds order by first_rounds ;

-- 1|1
-- 2|1
-- 6|1
-- 9|1
-- 10|1
-- 96|1
-- 100|16
-- 101|11
