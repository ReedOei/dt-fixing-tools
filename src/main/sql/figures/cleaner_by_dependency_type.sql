select pd.id, ct.test_name, group_concat(d.test_name) as tests
from od_classification odc
inner join minimize_test_result mtr on odc.test_name = mtr.test_name
inner join polluter_data pd on mtr.id = pd.minimized_id
inner join dependency d on d.polluter_data_id = pd.id
inner join cleaner_data cd on cd.polluter_data_id = pd.id
inner join cleaner_group cg on cg.cleaner_data_id = cd.id
inner join cleaner_test ct on ct.cleaner_group_id = cg.id
where 'any' = ? or odc.od_type = ?
group by pd.id, ct.test_name;
