select s.name,
       max(trr.test_count)
from subject as s
inner join test_run_result as trr on trr.subject_name = s.name
group by s.name
