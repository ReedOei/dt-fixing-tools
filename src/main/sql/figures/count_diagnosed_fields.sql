select odc.test_name
from od_classification odc
left join diagnosis_info di on di.test_name = odc.test_name
where odc.od_type like ? and ifnull(fields, 0) >= ? and ifnull(fields, 0) <= ?;
