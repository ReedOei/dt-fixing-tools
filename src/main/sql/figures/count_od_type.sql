select distinct test_name
from od_classification
where 'any' = ? or od_type = ?;

