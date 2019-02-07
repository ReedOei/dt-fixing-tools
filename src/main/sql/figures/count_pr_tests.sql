select distinct pt.test_name
from pr_tests pt
join od_classification oc on pt.test_name = oc.test_name;
