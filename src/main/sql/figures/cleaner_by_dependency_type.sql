select cg.cleaners, dg.deps
from dependency_groups dg
inner join od_classification odc on odc.test_name = dg.test_name
inner join cleaner_groups cg on dg.polluter_data_id = cg.polluter_data_id
where odc.od_type like ?;

