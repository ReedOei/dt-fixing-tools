select ftc.test_name, ftc.flaky_type, 
	   sum(case 
		when ftc.flaky_type = 'flaky' then 
			case
				when cr.passing_result <> cr.passing_expected_result or
				     cr.failing_result <> cr.failing_expected_result then 1
				else 0
			end
		else 
			case
				when cr.passing_result = cr.passing_expected_result and
				     cr.failing_result = cr.failing_expected_result then 1
				else 0
			end
	   end) as confirmed_runs, 
	   count(*) as total_runs
from flaky_test_classification as ftc
inner join confirmation_runs as cr on ftc.test_name = cr.test_name
-- where cr.round_type like '%conf%'
group by ftc.test_name
