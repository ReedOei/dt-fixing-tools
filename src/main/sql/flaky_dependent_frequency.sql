select subject.name,
	   subject.slug,
	   detection_round.round_type,
	   count(*) as n
from subject
inner join detection_round on detection_round.subject_name = subject.name
inner join flaky_test_list on flaky_test_list.flaky_test_list_id = detection_round.filtered_id
inner join flaky_test on flaky_test_list.flaky_test_id = flaky_test.id
group by subject.name, subject.slug, detection_round.round_type;
