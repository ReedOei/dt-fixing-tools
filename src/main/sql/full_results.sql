select s.name,
       max(trr.test_count),
	     flaky_rn.round_number as flaky_rounds,
	     random_rn.round_number as round_rounds,
	     flaky.detected_num as flaky_num,
	     rand.detected_num as random_num
from subject as s
inner join test_run_result as trr on trr.subject_name = s.name
-- Count the number of flaky rounds
inner join
(
	select dr.subject_name, count(*) as round_number
	from detection_round as dr
	where dr.round_type = 'flaky'
	group by dr.subject_name
) as flaky_rn on flaky_rn.subject_name = s.name
-- Count the number of random rounds
inner join
(
	select dr.subject_name, count(*) as round_number
	from detection_round as dr
	where dr.round_type = 'random'
	group by dr.subject_name
) as random_rn on random_rn.subject_name = s.name
-- Count the number of flaky tests
inner join
(
	select s.name,
		   count(ft.name) as detected_num
	from subject as s
	inner join detection_round as dr on dr.subject_name = s.name
	left join flaky_test_list as ftl on ftl.flaky_test_list_id = dr.filtered_id
	left join flaky_test as ft on ftl.flaky_test_id = ft.id
	where dr.round_type = 'flaky'
	group by s.name
) as flaky on flaky.name = s.name
-- Count the number of dependent tests
inner join
(
	select s.name,
		   count(ft.name) as detected_num
	from subject as s
	inner join detection_round as dr on dr.subject_name = s.name
	left join flaky_test_list as ftl on ftl.flaky_test_list_id = dr.filtered_id
	left join flaky_test as ft on ftl.flaky_test_id = ft.id
	where dr.round_type = 'random'
	group by s.name
) as rand on rand.name = s.name
group by s.name
