select distinct test_name
from
(
  select ci.test_name, min(cleaner_count) as total
  from cleaner_info ci
  inner join od_classification odc on ci.test_name = odc.test_name
  where odc.od_type = 'victim'
  group by ci.test_name
) t
where total >= ? and total <= ?;
