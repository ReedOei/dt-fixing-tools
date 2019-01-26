select distinct odc.test_name
from od_classification odc
left join
(
  select test_name, min(fields) as fields
  from diagnosis_info
  group by test_name
) di on di.test_name = odc.test_name
where odc.od_type like ? and ifnull(fields, 0) >= ? and ifnull(fields, 0) <= ?;
