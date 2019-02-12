select max(num)
from 
  (select odc.test_name, count(distinct dg.deps) as num
  from od_classification odc
  inner join dependency_groups dg on dg.test_name = odc.test_name
  where od_type like ? and
        dg.dep_count >= 0 and
        dg.dep_count <= ?
  group by odc.test_name
  having num >= ? and num <= ?);
