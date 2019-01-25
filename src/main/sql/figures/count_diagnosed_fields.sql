select odc.test_name
from diagnosis_info di
inner join od_classification odc on di.test_name = odc.test_name
where odc.od_type like ? and fields >= ? and fields <= ?;
