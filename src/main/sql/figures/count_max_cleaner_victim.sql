select max(num) 
from 
  (select odc.test_name, count(cg.test_name) as num
  from od_classification odc
  left join cleaner_groups cg on odc.test_name = cg.test_name
  where od_type = 'victim' and ifnull(cleaner_count, 0) >= 0 and ifnull(cleaner_count, 0) <= 100000
  group by odc.test_name
  having num >= 2 and num <= 100000);
