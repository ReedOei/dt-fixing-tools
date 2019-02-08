create view minimized_tests_fmo as
select distinct subject_name, test_name
from minimize_test_result;

create view polluter_data_count_fmo as
select mt.subject_name,
       mt.test_name,
       count(passing.num) as passing_count,
       count(failing.num) as failing_count
from minimized_tests_fmo mt
left join
(
    select mtr.test_name, mtr.expected_result, count(d.test_name) as num
    from minimize_test_result mtr
    left join polluter_data pd on mtr.id = pd.minimized_id
    left join dependency d on d.polluter_data_id = pd.id
    group by mtr.test_name, mtr.expected_result
    having num > 0
) passing on passing.test_name = mt.test_name and passing.expected_result = 'PASS'
left join
(
    select mtr.test_name, mtr.expected_result, count(d.test_name) as num
    from minimize_test_result mtr
    left join polluter_data pd on mtr.id = pd.minimized_id
    left join dependency d on d.polluter_data_id = pd.id
    group by mtr.test_name, mtr.expected_result
    having num > 0
) failing on failing.test_name = mt.test_name and failing.expected_result <> 'PASS'
group by mt.subject_name, mt.test_name;

create view od_classification_w_fmo as
select pdc.test_name,
       pdc.subject_name,
       case
        when passing_count > 0 then 'brittle'
        when failing_count > 0 then 'victim'
      end as od_type
from polluter_data_count_fmo pdc
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
     FROM unfinished_tests);

create view fixable_tests_fmo as
select odc.subject_name, odc.test_name, odc.od_type
from od_classification_w_fmo odc
left join tests_with_cleaner c on c.test_name = odc.test_name
left join tests_with_setter s on s.test_name = odc.test_name
where c.test_name is not null or s.test_name is not null;