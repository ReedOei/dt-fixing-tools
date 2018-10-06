select dr.subject_name as subject_name,
       dr.round_type as flaky_type,
       ft.name as test_name,
	     count(*) as number
from detection_round as dr
inner join flaky_test_list as ftl on dr.unfiltered_id = ftl.flaky_test_list_id
inner join flaky_test as ft on ftl.flaky_test_id = ft.id
group by dr.subject_name, dr.round_type, ft.name;