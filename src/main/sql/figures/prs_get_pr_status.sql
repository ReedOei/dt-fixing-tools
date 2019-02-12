select pr.subject_name, count(pr.subject_name) as pCount
from
  (select pt.subject_name, pt.pr_link
   from pr_tests pt
   where pt.pr_status like ?
   group by pt.subject_name, pt.pr_link) pr
where pr.subject_name IN
      (select subject_name
      from od_classification)
group by pr.subject_name;
