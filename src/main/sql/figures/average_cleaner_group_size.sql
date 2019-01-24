select avg(num)
from
(
  select cg.id, count(*) as num
  from cleaner_group cg
  inner join cleaner_test ct on cg.id = ct.cleaner_group_id
  group by cg.id
  having num >= ? and num <= ?
) t;
