insert into flaky_test_classification
select info.subject_name,
       info.test_name,
       case
         when info.flaky_runs > 0 then 'flaky' -- If it was EVER flaky, then we should consider it a flaky test
        else 'random'
      end as flaky_type
from
(
  select fti.subject_name,
         fti.test_name,
  sum(case when fti.round_type = 'random' then 1 else 0 end) as random_rounds,
  sum(case when fti.round_type = 'random-class' then 1 else 0 end) as random_class_rounds,
  sum(ifnull(cbt.total_runs, 0)) as all_confirmation_rounds,
  sum(case
    -- If total_runs is null, there were never any confirmation rounds (so it must be a flaky test)
    when ifnull(cbt.total_runs, 0) > 0 and cbt.confirmed_runs = cbt.total_runs then 0
    else 1
    end) as flaky_runs,
  count(*) as total_runs
  from flaky_test_info as fti
  left join confirmation_by_test as cbt on fti.test_name = cbt.test_name
  group by fti.subject_name, fti.test_name
) as info
inner join original_order o on info.subject_name = o.subject_name and info.test_name = o.test_name;

insert into num_rounds
select subject_name, round_type, count(*) as number
from detection_round
group by subject_name, round_type;

insert into confirmation_runs
select p.test_name,
       p.round_type,
       p.round_number,
       p.verify_round_number,
       p.expected_result,
       p.result,
       f.expected_result,
       f.result
from verify_round p
inner join verify_round f on p.test_name = f.test_name and
                             p.round_number = f.round_number and
                             p.verify_round_number = f.verify_round_number
where p.expected_result = 'PASS' and
      f.expected_result <> 'PASS';

create temporary table temp
(
  subject_name,
  round_type,
  test_name,
  detection_round_id
);

insert into temp
select fti.subject_name, dr.round_type, fti.test_name, dr.id
from flaky_test_info fti
inner join detection_round dr on fti.detection_round_id = dr.id;

create temporary table temp2
(
  subject_name,
  round_type,
  flaky_type,
  test_name,
  detection_round_id
);

insert into temp2
select t.subject_name, t.round_type, ftc.flaky_type, t.test_name, t.detection_round_id
from temp t
inner join flaky_test_classification ftc on t.test_name = ftc.test_name;

create temporary table temp3
(
  subject_name,
  round_type,
  n
);

insert into temp3
select subject_name, round_type, count(*) as n
from detection_round
group by subject_name, round_type;

insert into flaky_test_failures
select i.subject_name, i.test_name, i.round_type, i.flaky_type, failures, rounds
from
(
  select subject_name, t2.test_name, t2.round_type, t2.flaky_type, count(distinct detection_round_id) as failures
  from temp2 t2
  group by t2.test_name, t2.round_type, t2.flaky_type
) i
inner join
(
  select subject_name, round_type, sum(n) as rounds
  from temp3 t3
  group by subject_name, round_type
) t on i.round_type = t.round_type and i.subject_name = t.subject_name;

insert into detection_round_failures
select dr.id, dr.round_type, count(distinct ft.name) as failures
from detection_round dr
  inner join flaky_test_list ftl on dr.unfiltered_id = ftl.flaky_test_list_id
  inner join flaky_test ft on ftl.flaky_test_id = ft.id
group by dr.id, dr.round_type;
