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

create view cleaner_info_fmo as
select mtr.subject_name,
       mtr.test_name,
       mtr.expected_result,
       cg.id,
       count(ct.test_name) as cleaner_count
from od_classification_w_fmo odc
inner join minimize_test_result mtr on mtr.test_name = odc.test_name
left join polluter_data pd on mtr.id = pd.minimized_id
left join cleaner_data cd on cd.polluter_data_id = pd.id
left join cleaner_group cg on cg.cleaner_data_id = cd.id
left join cleaner_test ct on ct.cleaner_group_id = cg.id
group by mtr.subject_name, mtr.test_name, mtr.expected_result, cg.id;

create view tests_with_cleaner_fmo as
select test_name, max(cleaner_count) as total
from cleaner_info_fmo
group by test_name
having total > 0;

create view dependency_info_fmo as
select mtr.subject_name,
       mtr.test_name,
       mtr.expected_result,
       pd.id,
       count(d.test_name) as dep_count
from od_classification_w_fmo odc
inner join minimize_test_result mtr on mtr.test_name = odc.test_name
left join polluter_data pd on mtr.id = pd.minimized_id
left join dependency d on pd.id = d.polluter_data_id
group by mtr.subject_name, mtr.test_name, mtr.expected_result, pd.id
having dep_count > 0;

create view tests_with_setter_fmo as
select di.test_name, max(dep_count) as total
from dependency_info_fmo di
inner join od_classification_w_fmo as odc on di.test_name = odc.test_name
where odc.od_type = 'brittle' and di.expected_result = 'PASS'
group by di.test_name
having total > 0;

create view fixable_tests_fmo as
select odc.subject_name, odc.test_name, odc.od_type
from od_classification_w_fmo odc
left join tests_with_cleaner_fmo c on c.test_name = odc.test_name
left join tests_with_setter_fmo s on s.test_name = odc.test_name
where c.test_name is not null or s.test_name is not null;