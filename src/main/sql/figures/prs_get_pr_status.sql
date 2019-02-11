select subject_name, count(subject_name) as pCount
from 
  (select pt.subject_name, pt.pr_link 
   from pr_tests pt 
   where pt.pr_status like ? 
   group by pt.subject_name, pt.pr_link) 
group by subject_name;
