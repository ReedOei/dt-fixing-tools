select count(distinct test_name)
from od_classification
where od_type = ?;

