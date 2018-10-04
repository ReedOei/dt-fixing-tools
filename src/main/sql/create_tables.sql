create table subject
(
  name text primary key,
  slug text not null
);

create table test_run_result
(
  str_id text primary key,
  subject_name text not null,

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
  foreign key(unfiltered_id) references flaky_test_list(id),
  foreign key(filtered_id) references flaky_test_list(id)
);

create table test_result
(
  id integer primary key,
  test_run_result_str_id text not null,
  order_index integer not null,
  name text not null,
  run_time real not null,
  result text not null,
  stack_trace text not null,

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
