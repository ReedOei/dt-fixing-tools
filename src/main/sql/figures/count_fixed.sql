-- Count how many of the tests that have cleaners or setters succeeded
select distinct ft.test_name
from fixable_tests ft
inner join od_classification odc on ft.test_name = odc.test_name
left join fixed_tests fixt on fixt.test_name = ft.test_name
left join
(
  select id, test_name, min(cleaner_count) as total
  from cleaner_info
  group by test_name
) c on c.test_name = ft.test_name
where (case when fixt.test_name is null then 0 else 1 end = ?) and
      odc.od_type like ? and
      (case when c.id is not null then 'has_cleaner' else 'no_cleaner' end like ?);
