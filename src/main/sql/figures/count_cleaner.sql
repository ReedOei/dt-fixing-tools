select distinct test_name
from
(
  select test_name, min(cleaner_count) as total
  from cleaner_info
  group by test_name
) t
where total >= ? and total <= ?;
