select s.name,
	     s.slug,
	     dr.round_type,
		 count(ft.name) as n
from subject as s
inner join detection_round as dr on dr.subject_name = s.name
left join flaky_test_list as ftl on ftl.flaky_test_list_id = dr.filtered_id
left join flaky_test as ft on ftl.flaky_test_id = ft.id
group by s.name, dr.round_type
