select count(*)
from polluter_data pd
inner join minimize_test_result mtr on mtr.id = pd.minimized_id
inner join od_classification odc on odc.test_name = mtr.test_name
where odc.od_type like ?;