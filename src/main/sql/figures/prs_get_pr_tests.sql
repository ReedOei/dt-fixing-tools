select pt.subject_name, count(pt.test_name) as tCount
from pr_tests pt
join od_classification oc on oc.subject_name = pt.subject_name
where pt.pr_status like ? 
group by pt.subject_name;
