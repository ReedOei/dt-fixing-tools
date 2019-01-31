-- Get the "first" (arbitrary) dependency/cleaner (if exists) for each dependent test)

select odc.test_name, odc.od_type, group_concat(d.test_name) as dependency, group_concat(ct.test_name) as cleaner
from od_classification odc
inner join minimize_test_result mtr on odc.test_name = mtr.test_name
inner join polluter_data pd on mtr.id = pd.minimized_id
inner join dependency d on d.polluter_data_id = pd.id
left join cleaner_data cd on cd.polluter_data_id = pd.id
left join cleaner_group cg on cg.cleaner_data_id = cd.id
left join cleaner_test ct on ct.cleaner_group_id = cg.id
group by odc.test_name, odc.od_type, pd.id, cg.id;

