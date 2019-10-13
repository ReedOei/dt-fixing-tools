select first_rounds,count(first_rounds) from fs_tests_found_only_in_first_sha_mapping_failures group by first_rounds order by first_rounds;

-- 1|1
-- 2|2
-- 6|3
-- 9|3
-- 10|1
-- 96|20
-- 100|51
-- 101|16
