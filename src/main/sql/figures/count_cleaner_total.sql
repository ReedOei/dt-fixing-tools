select count(*)
from polluter_data pd
inner join minimize_test_result mtr on mtr.id = pd.minimized_id
inner join od_classification odc on odc.test_name = mtr.test_name
inner join cleaner_data cd on cd.polluter_data_id = pd.id
inner join cleaner_group cg on cg.cleaner_data_id = cd.id
inner join cleaner_test ct on ct.cleaner_group_id = cg.id
where odc.od_type like ?;
