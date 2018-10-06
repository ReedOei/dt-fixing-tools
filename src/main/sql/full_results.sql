select si.name,
       si.test_count,
	     flaky.round_number as flaky_rounds,
	     random.round_number as round_rounds,
	     flaky.number as flaky_num,
	     random.number as random_num
from subject_info as si
inner join flaky_test_counts as flaky on flaky.subject_name = si.name and flaky.flaky_type = 'flaky'
inner join flaky_test_counts as random on random.subject_name = si.name and random.flaky_type = 'random'
group by si.name
