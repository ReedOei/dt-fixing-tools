select distinct test_name
from
(
  select test_name, min(dep_count) as total
  from dependency_info
  group by test_name
) t
where total >= ? and total <= ?;
