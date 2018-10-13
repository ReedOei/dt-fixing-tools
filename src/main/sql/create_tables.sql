create table subject_raw
(
  slug text primary key,
  source text not null,
  url text not null,
  sha text not null
);

create table subject
(
  name text primary key,
  slug text not null,

  foreign key(slug) references subject_raw(slug)
);

create table test_run_result
(
  str_id text primary key,
  subject_name text not null,
  test_count integer not null,

  foreign key(subject_name) references subject(name)
);

create table flaky_test
(
  id integer primary key,
  name text not null,
  intended_id text not null,
  revealed_id text not null,

  foreign key(intended_id) references test_run_result(str_id),
  foreign key(revealed_id) references test_run_result(str_id)
);

create table flaky_test_list
(
  id integer primary key,
  flaky_test_list_id integer not null,
  flaky_test_id integer not null,

  foreign key(flaky_test_id) references flaky_test(id)
);

create table detection_round
(
  id integer primary key,
  subject_name text not null,
  unfiltered_id integer not null,
  filtered_id integer not null,
  round_type text not null,
  round_number integer not null,
  round_time real not null,

  foreign key(subject_name) references subject(name),
  foreign key(unfiltered_id) references flaky_test_list(flaky_test_list_id),
  foreign key(filtered_id) references flaky_test_list(flaky_test_list_id)
);

create table detection_round_test_runs
(
  id integer primary key,
  detection_round_id integer not null,
  test_run_result_id text not null,

  foreign key(detection_round_id) references detection_round(id),
  foreign key(test_run_result_id) references test_run_result(str_id)
);

create table test_result
(
  id integer primary key,
  test_run_result_str_id text not null,
  order_index integer not null,
  name text not null,
  run_time real not null,
  result text not null,

  foreign key(test_run_result_str_id) references test_run_result(str_id)
);

create table verify_round
(
  id integer primary key,
  subject_name text not null,
  round_number integer not null,
  test_run_result_str_id text not null,
  round_type text not null,
  verify_round_number integer not null,
  test_name text not null,
  expected_result text not null,
  result text not null,

  foreign key(subject_name) references subject(name),
  foreign key(round_number) references detection_round(round_number),
  foreign key(test_run_result_str_id) references test_run_result(str_id)
);

create table confirmation_runs
(
  test_name text,
  round_type text,
  round_number integer,
  verify_round_number integer,
  passing_expected_result text not null,
  passing_result text not null,
  failing_expected_result text not null,
  failing_result text not null
);

create table module_test_time
(
  coordinates text not null,
  groupId text not null,
  artifactId text not null,
  version text not null,
  test_time real not null
);

create view subject_info as
select s.name as name,
       max(trr.test_count) as test_count
from subject as s
inner join test_run_result as trr on trr.subject_name = s.name
group by s.name;

create view unfiltered_flaky_tests as
select dr.id as detection_round_id,
       dr.subject_name,
       dr.round_type as flaky_type,
       ft.id as flaky_test_id,
       ft.name as test_name
from flaky_test ft
inner join flaky_test_list as ftl on ftl.flaky_test_id = ft.id
inner join detection_round as dr on dr.unfiltered_id = ftl.flaky_test_list_id;

create view filtered_flaky_tests as
select dr.id as detection_round_id,
       dr.subject_name,
       dr.round_type as flaky_type,
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
                uft.flaky_type as round_type, -- This is really just the type of the round
                case
                  -- this means that it was filtered out for being flaky and is NOT dependent
                  when fft.test_name is null then 'flaky'
                  else uft.flaky_type
                end as flaky_type,
                uft.flaky_test_id,
                uft.test_name
from unfiltered_flaky_tests as uft
left join filtered_flaky_tests as fft on uft.test_name = fft.test_name and uft.subject_name = fft.subject_name;

create view flaky_test_classification as
select subject_name,
       test_name,
       case
        when all_confirmation_rounds = 0 then 'flaky_detector'

        -- If we never found it with the method detector, then we must have found it in the class detector, and vice versa
        when random_class_rounds = 0 then 'random_method_detector'
        when random_rounds = 0 then 'random_class_detector'

        -- But if we found it with both, then we can't definitively say it was found by one or the other
        else 'random_detector'
       end as classification_source,
       case
        when flaky_runs > 0 then 'flaky' -- If it was EVER flaky, then we should consider it a flaky test
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
) as info;

create view flaky_test_counts as
select subject_name, flaky_type, count(distinct test_name) as number
from flaky_test_classification
group by subject_name, flaky_type;

create view subject_overview as
select si.name,
       si.test_count,
	     count(distinct flaky_rounds.id) as flaky_round_num,
	     count(distinct random_rounds.id) as random_round_num,
	     ifnull(max(flaky.number), 0) as flaky_num,
	     ifnull(max(random.number), 0) as random_num
from subject_info as si
left join detection_round as flaky_rounds on flaky_rounds.subject_name = si.name and flaky_rounds.round_type = 'flaky'
left join detection_round as random_rounds on random_rounds.subject_name = si.name and random_rounds.round_type = 'random'
left join flaky_test_counts as flaky on flaky.subject_name = si.name and flaky.flaky_type = 'flaky'
left join flaky_test_counts as random on random.subject_name = si.name and random.flaky_type = 'random'
group by si.name;

create view confirmation_effectiveness as
select ftc.test_name, ftc.flaky_type,
       cr.round_type,
	   sum(case
		      when ftc.flaky_type = 'flaky' then
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
