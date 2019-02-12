select pr.subject_name, count(pr.subject_name) as pCount
from 
  (select pt.subject_name, pt.pr_link 
   from pr_tests pt 
   where pt.pr_status like ? 
   group by pt.subject_name, pt.pr_link) pr
join od_classification oc on oc.subject_name = pr.subject_name
group by pr.subject_name;
