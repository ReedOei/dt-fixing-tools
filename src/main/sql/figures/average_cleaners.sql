select round(avg(t.total), 1)
from
(
  select test_name, max(cleaner_count) as total
  from cleaner_info
  group by test_name
) t
where t.total >= ?;
