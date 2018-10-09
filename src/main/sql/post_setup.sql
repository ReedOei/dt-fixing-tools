insert into confirmation_results
select vr.name,
       vr.test_run_result_str_id,
       vr.round_type,
       vr.round_number,
       vr.verify_round_number,
       vr.expected_result,
       tr.result
from verify_round vr
inner join test_result tr on vr.name = tr.name and
                             vr.test_run_result_str_id = tr.test_run_result_str_id;

insert into confirmation_runs
select p.test_name,
       p.round_type,
       p.round_number,
       p.verify_round_number,
       p.expected_result,
       f.expected_result,
       p.result,
       f.result
from confirmation_results p
inner join confirmation_results f on p.test_name = f.test_name and
                                     p.round_number = f.round_number and
                                     p.verify_round_number = f.verify_round_number
where p.expected_result = 'PASS' and
      f.expected_result <> 'PASS';
