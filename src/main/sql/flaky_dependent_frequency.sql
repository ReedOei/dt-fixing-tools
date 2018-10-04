select s.name,
	     s.slug,
	     dr.round_type,
	     count(*) as n
from subject as s
inner join detection_round as dr on dr.subject_name = s.name
inner join flaky_test_list as ftl on ftl.flaky_test_list_id = dr.filtered_id
inner join flaky_test as ft on ftl.flaky_test_id = ft.id
group by s.name, s.slug, dr.round_type;
