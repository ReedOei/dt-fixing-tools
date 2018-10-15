select i.slug, i.sha, i.loc_code, i.loc_test, i.test_count, i.flaky_n, i.rand_n
from
(
  select s.slug, substr(sr.sha, 1, 8) as sha, ' ' as loc_code, ' ' as loc_test,
       sum(si.test_count) as test_count,
       sum(ifnull(flaky.number, 0)) as flaky_n,
       sum(ifnull(rand.number, 0)) as rand_n
  from subject_info si
  inner join subject s on si.name = s.name
  inner join subject_raw sr on s.slug = sr.slug
  left join flaky_test_counts flaky on flaky.flaky_type = 'flaky' and flaky.subject_name = s.name
  left join flaky_test_counts rand on rand.flaky_type = 'random' and rand.subject_name = s.name
  group by s.slug, sr.sha
) i
where flaky_n > 0 or rand_n > 0