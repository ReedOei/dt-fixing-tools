select si.name,
       si.test_count,
	   count(distinct flaky_rounds.id) as flaky_round_num,
	   count(distinct random_rounds.id) as random_round_num,
	   ifnull(max(flaky.number), 0) as flaky_num,
	   ifnull(max(random.number), 0) as random_num
from subject_info as si
inner join detection_round as flaky_rounds on flaky_rounds.subject_name = si.name and flaky_rounds.round_type = 'flaky'
inner join detection_round as random_rounds on random_rounds.subject_name = si.name and random_rounds.round_type = 'random'
left join flaky_test_counts as flaky on flaky.subject_name = si.name and flaky.flaky_type = 'flaky'
left join flaky_test_counts as random on random.subject_name = si.name and random.flaky_type = 'random'
group by si.name;
