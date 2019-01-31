select mtr.test_name, tr.name, case when mtr.test_name = tr.name then 1 else 0 end
from minimize_test_result mtr
join test_result tr on tr.test_run_result_str_id = mtr.expected_run_str_id
where mtr.expected_result <> 'PASS' and tr.result <> 'PASS'
order by mtr.test_name, tr.order_index;

-- 26 minimized that were not the first failure
-- doesn't look like we ran wildfly

