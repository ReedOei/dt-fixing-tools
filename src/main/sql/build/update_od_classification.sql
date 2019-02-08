create view od_classification as
select pdc.test_name,
       pdc.subject_name,
       case
        when passing_count > 0 then 'brittle'
        when failing_count > 0 then 'victim'
       end as od_type
from polluter_data_count pdc
left join no_test n on n.test_name = pdc.test_name
where n.test_name is null and (passing_count > 0 or failing_count > 0)
and pdc.test_name NOT IN
    (SELECT test_name
     FROM incompatible_tests)
and pdc.test_name NOT IN
    (SELECT test_name
     FROM separate_jvm_tests)
and pdc.test_name NOT IN
    (SELECT test_name
     FROM unfinished_tests)
and pdc.test_name NOT IN
    (SELECT test_name
     FROM original_order
     WHERE fix_method_order != 0);
