select count(*)
from
(
  select dg.polluter_data_id, count(cg.test_name) as num
  from dependency_groups dg
  inner join od_classification odc on odc.test_name = dg.test_name
  left join cleaner_groups cg on dg.polluter_data_id = cg.polluter_data_id
  where odc.od_type = 'victim' and ifnull(cleaner_count, 0) >= ? and ifnull(cleaner_count, 0) <= ?
  group by dg.polluter_data_id
  having num >= ? and num <= ?
) t;
