select distinct odc.test_name
from od_classification odc
inner join diagnosed_tests dt on dt.test_name = odc.test_name
left join test_patch tp on tp.test_name = odc.test_name
where odc.od_type like ? and tp.test_name is null;
