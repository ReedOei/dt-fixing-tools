select distinct test_name
from
(
  select di.test_name, min(di.dep_count) as total
  from dependency_info di
  inner join od_classification as odc on di.test_name = odc.test_name
  where odc.od_type = 'victim' and di.expected_result <> 'PASS'
  group by di.test_name
) t
where total >= ? and total <= ?;
