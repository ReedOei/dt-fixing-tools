select round(avg(total), 1)
from
(
  select test_name, max(dep_count) as total
  from dependency_info
  group by test_name
) t
where t.total >= ?;
