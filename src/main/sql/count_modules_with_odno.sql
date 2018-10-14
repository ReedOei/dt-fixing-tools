select count(distinct flaky.subject_name)
from flaky_test_counts flaky
inner join flaky_test_counts random on flaky.subject_name = random.subject_name and random.flaky_type = 'random'
where flaky.flaky_type = 'flaky' and flaky.number > 0 and random.number > 0
