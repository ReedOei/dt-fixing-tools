create table subject
(
  name text primary key,
  slug text not null
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
  name text not null,
  expected_result text not null,

  foreign key(subject_name) references subject(name),
  foreign key(round_number) references detection_round(round_number),
  foreign key(test_run_result_str_id) references test_run_result(str_id)
);

create view subject_info as
select s.name as name,
       max(trr.test_count) as test_count
from subject as s
inner join test_run_result as trr on trr.subject_name = s.name
group by s.name;

create view flaky_test_info as
select dr.id as detection_round_id,
       dr.subject_name as subject_name,
       dr.round_type as flaky_type,
       dr.round_number as round_number,
       ft.id as flaky_test_id,
       ft.name as test_name
from detection_round as dr
inner join flaky_test_list as ftl on dr.filtered_id = ftl.flaky_test_list_id
inner join flaky_test as ft on ftl.flaky_test_id = ft.id;

create view flaky_test_counts as
select dr.subject_name as subject_name,
       dr.round_type as flaky_type,
       count(distinct ft.name) as number,
       count(distinct dr.round_number) as round_number
from detection_round as dr
left join flaky_test_list as ftl on dr.filtered_id = ftl.flaky_test_list_id
left join flaky_test as ft on ftl.flaky_test_id = ft.id
group by dr.subject_name, dr.round_type;
