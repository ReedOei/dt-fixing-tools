select count(*)
from
(
  select dg.polluter_data_id, count(cg.test_name) as num
  from dependency_groups dg
  left join cleaner_groups cg on dg.polluter_data_id = cg.polluter_data_id
  where ifnull(cleaner_count, 0) >= ? and ifnull(cleaner_count, 0) <= ?
  group by dg.polluter_data_id
  having num >= ? and num <= ?
) t;
