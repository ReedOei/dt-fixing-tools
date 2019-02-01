select s.slug, s.name, count(ft.test_name) as n
from od_classification odc
inner join subject s on s.name = odc.subject_name
left join fixed_tests ft on ft.test_name = odc.test_name
group by s.slug, s.name;

