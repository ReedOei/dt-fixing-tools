select count(distinct s.slug)
from flaky_test_counts flaky
inner join flaky_test_counts random on flaky.subject_name = random.subject_name and random.flaky_type = 'random'
inner join subject on s.name = ftc.subject_name
where flaky.flaky_type = 'flaky' and flaky.number > 0 and random.number > 0
