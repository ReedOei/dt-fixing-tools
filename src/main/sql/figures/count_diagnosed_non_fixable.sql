select distinct odc.test_name
from od_classification odc
inner join diagnosed_tests dt on dt.test_name = odc.test_name
left join dependency_groups dg on dg.test_name = odc.test_name
left join cleaner_groups cg on cg.test_name = odc.test_name
where odc.od_type like ? and
      ((odc.od_type = 'brittle' and dg.test_name is null) or
       (odc.od_type = 'victim' and dg.test_name is not null and cg.test_name is null));
