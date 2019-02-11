select subject_name, count(test_name) as tCount
from pr_tests 
where pr_status like ? 
group by subject_name;
