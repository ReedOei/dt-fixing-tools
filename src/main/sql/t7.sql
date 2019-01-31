select tp.test_name
from test_patch tp
inner join od_classification odc on tp.test_name = odc.test_name
left join
(
  select id, test_name, min(cleaner_count) as total
  from cleaner_info
  group by test_name
) c on c.test_name = tp.test_name
where tp.succeeded = 0;
