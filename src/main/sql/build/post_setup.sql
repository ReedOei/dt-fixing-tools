create view subject_info as
select s.name as name,
       max(trr.test_count) as test_count
from subject as s
inner join test_run_result as trr on trr.subject_name = s.name
group by s.name;

create view unfiltered_flaky_tests as
select dr.id as detection_round_id,
       dr.subject_name,
       case
        when dr.round_type = 'original' then 'NO'
        else 'OD'
       end as flaky_type,
       ft.id as flaky_test_id,
       ft.name as test_name
from flaky_test ft
inner join flaky_test_list as ftl on ftl.flaky_test_id = ft.id
inner join detection_round as dr on dr.unfiltered_id = ftl.flaky_test_list_id;

create view filtered_flaky_tests as
select dr.id as detection_round_id,
       dr.subject_name,
       case
        when dr.round_type = 'original' then 'NO'
        else 'OD'
       end as flaky_type,
       ft.id as flaky_test_id,
       ft.name as test_name
from flaky_test ft
inner join flaky_test_list as ftl on ftl.flaky_test_id = ft.id
inner join detection_round as dr on dr.filtered_id = ftl.flaky_test_list_id;

create view confirmation_by_test as
select cr.test_name,
       sum(case
            when cr.passing_result = cr.passing_expected_result and
                 cr.failing_result = cr.failing_expected_result then 1
            else 0
           end) as confirmed_runs,
       count(*) as total_runs
from confirmation_runs as cr
group by cr.test_name;

create view flaky_test_info as
select distinct uft.detection_round_id,
                uft.subject_name,
                case
                  -- this means that it was filtered out for being flaky and is NOT dependent
                  when fft.test_name is null then 'NO'
                  else uft.flaky_type
                end as flaky_type,
                uft.flaky_test_id,
                uft.test_name
from unfiltered_flaky_tests as uft
left join filtered_flaky_tests as fft on uft.test_name = fft.test_name and uft.subject_name = fft.subject_name;

create view flaky_test_counts as
select subject_name, flaky_type, count(distinct test_name) as number
from flaky_test_classification
group by subject_name, flaky_type;

create view subject_overview as
select si.name,
       si.test_count,
	     count(distinct no_rounds.id) as no_round_num,
	     count(distinct od_rounds.id) as od_round_num,
	     ifnull(max(flaky.number), 0) as flaky_num,
	     ifnull(max(random.number), 0) as random_num
from subject_info as si
left join detection_round as no_rounds on no_rounds.subject_name = si.name and no_rounds.round_type = 'original'
left join detection_round as od_rounds on od_rounds.subject_name = si.name and od_rounds.round_type <> 'original'
left join flaky_test_counts as nonorder on nonorder.subject_name = si.name and nonorder.flaky_type = 'NO'
left join flaky_test_counts as orderdep on orderdep.subject_name = si.name and orderdep.flaky_type = 'OD'
group by si.name;

create view confirmation_effectiveness as
select ftc.test_name, ftc.flaky_type,
       cr.round_type,
	   sum(case
		      when ftc.flaky_type = 'NO' then
            case
              when cr.passing_result <> cr.passing_expected_result or
                   cr.failing_result <> cr.failing_expected_result then 1
              else 0
            end
          else
            case
              when cr.passing_result = cr.passing_expected_result and
                   cr.failing_result = cr.failing_expected_result then 1
              else 0
            end
           end) as confirmed_runs,
	   count(*) as total_runs
from flaky_test_classification as ftc
inner join confirmation_runs as cr on ftc.test_name = cr.test_name
group by ftc.test_name, ftc.flaky_type, cr.round_type;

create view polluter_data_count as
select mtr.test_name,
       passing.num as passing_count,
       failing.num as failing_count
from minimize_test_result mtr
inner join
(
    select mtr.test_name, mtr.expected_result, count(pd.id) as num
    from minimize_test_result mtr
    left join polluter_data pd on mtr.id = pd.minimized_id
    group by mtr.test_name, mtr.expected_result
) passing on passing.test_name = mtr.test_name and passing.expected_result = 'PASS'
inner join
(
    select mtr.test_name, mtr.expected_result, count(pd.id) as num
    from minimize_test_result mtr
    left join polluter_data pd on mtr.id = pd.minimized_id
    group by mtr.test_name, mtr.expected_result
) failing on failing.test_name = mtr.test_name and failing.expected_result <> 'PASS';

create view od_classification as
select pdc.test_name,
       oo.subject_name,
       case
        when passing_count > 0 then 'brittle'
        when failing_count > 0 then 'victim'
        else 'both'
      end as od_type
from polluter_data_count pdc
inner join original_order oo on pdc.test_name = oo.test_name;

create view dependency_info as
select oo.subject_name,
       mtr.test_name,
       mtr.expected_result,
       pd.id,
       count(d.test_name) as dep_count
from minimize_test_result mtr
inner join original_order oo on mtr.test_name = oo.test_name
left join polluter_data pd on mtr.id = pd.minimized_id
left join dependency d on pd.id = d.polluter_data_id
group by oo.subject_name, mtr.test_name, mtr.expected_result, pd.id;

create view cleaner_info as
select oo.subject_name,
       mtr.test_name,
       mtr.expected_result,
       cg.id,
       count(ct.test_name) as cleaner_count
from minimize_test_result mtr
inner join original_order oo on mtr.test_name = oo.test_name
left join polluter_data pd on mtr.id = pd.minimized_id
left join cleaner_data cd on cd.polluter_data_id = pd.id
left join cleaner_group cg on cg.cleaner_data_id = cd.id
left join cleaner_test ct on ct.cleaner_group_id = cg.id
group by oo.subject_name, mtr.test_name, mtr.expected_result, cg.id;

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
where p.expected_result = 'PASS' and f.expected_result <> 'PASS';

insert into flaky_test_classification
select info.subject_name,
       info.test_name,
       case
         when info.flaky_runs > 0 then 'NO' -- If it was EVER NO, then we should consider it an NO test
        else 'OD'
      end as flaky_type
from
(
  select fti.subject_name,
         fti.test_name,
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
select dr.id, dr.round_type,
       sum(case when ftc.flaky_type = 'NO' then 1 else 0 end),
       sum(case when ftc.flaky_type = 'OD' then 1 else 0 end)
from detection_round dr
left join flaky_test_list ftl on dr.unfiltered_id = ftl.flaky_test_list_id
left join flaky_test ft on ftl.flaky_test_id = ft.id
left join flaky_test_classification ftc on ft.name = ftc.test_name
group by dr.id, dr.round_type

