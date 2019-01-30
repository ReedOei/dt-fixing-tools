select distinct odc.test_name
from od_classification odc
left join
(
  select distinct test_name
  from test_patch
  where succeeded = 1
) tp on tp.test_name = odc.test_name
left join
(
  select id, test_name, min(cleaner_count) as total
  from cleaner_info
  group by test_name
) c on c.test_name = odc.test_name
where (case when tp.test_name is null then 0 else 1 end = ?) and
      odc.od_type like ? and
      (case when c.id is not null then 'has_cleaner' else 'no_cleaner' end like ?);
