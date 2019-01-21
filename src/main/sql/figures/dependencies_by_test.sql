select odc.test_name, group_concat(d.test_name) as tests
from od_classification odc
inner join minimize_test_result mtr on mtr.test_name = odc.test_name
inner join polluter_data pd on mtr.id = pd.minimized_id
inner join dependency d on pd.id = d.polluter_data_id
group by odc.test_name;
