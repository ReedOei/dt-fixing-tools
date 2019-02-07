select distinct test_name
from od_classification
where ('any' = ? or od_type = ?)
and test_name NOT IN
    (SELECT test_name
     FROM original_order
     WHERE fix_method_order != 0);
