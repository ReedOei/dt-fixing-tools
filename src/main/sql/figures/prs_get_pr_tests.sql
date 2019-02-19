select pt.subject_name, count(distinct pt.test_name) as tCount
from pr_tests pt
where pt.pr_status like ?
and pt.subject_name IN
    (select subject_name
    from od_classification)
group by pt.subject_name;
