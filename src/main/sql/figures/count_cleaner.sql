select count(distinct test_name)
from
(
  select test_name, max(cleaner_count) as total
  from cleaner_info
  group by test_name
) t
where total >= ? and total <= ?;
