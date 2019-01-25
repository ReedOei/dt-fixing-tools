select ft.test_name
from fixable_tests ft
inner join od_classification odc on ft.test_name = odc.test_name
left join
(
  select id, test_name, min(cleaner_count) as total
  from cleaner_info
  group by test_name
) c on c.test_name = ft.test_name
where odc.od_type like ? and
      (case when c.id is not null then 'has_cleaner' else 'no_cleaner' end like ?);
