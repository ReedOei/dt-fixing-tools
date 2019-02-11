create table subject_raw
(
  slug text primary key,
  url text not null,
  sha text not null,
  loc integer not null,
  test_loc integer not null
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

create table no_test
(
  id integer primary key,
  test_name text not null
);

create table pr_tests
(
  id integer primary key,
  subject_name text not null,
  test_name text not null,
  pr_status text not null,
  pr_link text not null
);

create table unfinished_tests
(
  id integer primary key,
  test_name text not null
);

create table separate_jvm_tests
(
  id integer primary key,
  test_name text not null
);

create table incompatible_tests
(
  id integer primary key,
  test_name text not null
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
  failing_result text not null,

  foreign key(test_name) references original_order(test_name)
);

create table module_test_time
(
  id integer primary key,
  coordinates text not null,
  groupId text not null,
  artifactId text not null,
  version text not null,
  test_time real not null
);

create table original_order
(
  id integer primary key,
  subject_name text not null,
  test_name text not null,
  test_class text not null,
  test_package text not null,
  order_index integer not null,
  fix_method_order integer not null default 0,

  foreign key(subject_name) references subject(name)
);

create table num_rounds
(
  name text not null,
  round_type text not null,
  number integer not null,

  foreign key(name) references subject(name)
);

create table flaky_test_classification
(
  subject_name text not null,
  test_name text not null,
  flaky_type text not null check(flaky_type in ('NO', 'OD')),

  foreign key(subject_name) references subject(name),
  foreign key(test_name) references original_order(test_name)
);

create table flaky_test_failures
(
  subject_name text not null,
  test_name text not null,
  round_type text not null,
  flaky_type text not null check(flaky_type in ('NO', 'OD')),
  failures integer not null,
  rounds integer not null,

  foreign key(subject_name) references subject(name),
  foreign key(test_name) references original_order(test_name)
);

create table detection_round_failures
(
  detection_round_id integer not null,
  round_type text not null,
  no_found integer not null,
  od_found integer not null,

  foreign key(detection_round_id) references detection_round(id)
);

create table operation_time
(
  id integer primary key,
  start_time integer not null,
  end_time integer not null,
  elapsed_time real not null
);

create table polluter_data
(
  id integer primary key,
  minimized_id integer not null,

  foreign key(minimized_id) references minimize_test_result(id)
);

create table dependency
(
  id integer primary key,
  polluter_data_id integer not null,
  test_name text not null,
  order_index integer not null,

  foreign key(polluter_data_id) references polluter_data(id),
  foreign key(test_name) references original_order
);

create table minimize_test_result
(
  id integer primary key,
  subject_name text not null,
  test_name text not null,
  expected_run_str_id text not null,
  expected_result text not null,
  order_hash text not null,
  operation_time_id integer not null,

  foreign key(operation_time_id) references operation_time(id),
  foreign key(test_name) references original_order(test_name)
);

create table time_manager
(
  id integer primary key,
  relative_time integer not null,
  absolute_time integer not null,

  foreign key(absolute_time) references operation_time(id),
  foreign key(relative_time) references operation_time(id)
);

create table cleaner_data
(
  id integer primary key,
  polluter_data_id integer not null,
  isolation_result text not null,
  expected_result text not null,
  operation_time_id integer not null,

  foreign key(polluter_data_id) references polluter_data(id),
  foreign key(operation_time_id) references operation_time(id)
);

create table cleaner_group
(
  id integer primary key,
  original_size integer not null,
  minimal_size integer not null,
  cleaner_data_id integer not null,

  foreign key(cleaner_data_id) references cleaner_data(id)
);

create table cleaner_test
(
  id integer primary key,
  cleaner_group_id integer not null,
  test_name text not null,
  order_index integer not null,

  foreign key(cleaner_group_id) references cleaner_group(id)
);

create table static_field_info
(
  id integer primary key,
  test_name text not null,
  order_hash text not null,
  mode text not null,

  foreign key(test_name) references original_order(test_name)
);

create table static_field_info_field
(
  id integer primary key,
  static_field_info_id integer not null,
  field_name text not null,

  foreign key(static_field_info_id) references static_field_info(id)
);

create table polluted_field
(
  id integer primary key,
  field_name text not null,
  test_name text not null,
  expected_result text not null,

  -- Intentionally nullable
  without_deps_val text,
  with_deps_val text,

  foreign key(test_name) references original_order(test_name)
);

create table rewrite_target
(
  id integer primary key,
  static_field_name text not null,
  field_name text not null,
  polluted_field_id integer not null,
  rewrite_result_id integer not null,

  foreign key(polluted_field_id) references polluted_field(id),
  foreign key(rewrite_result_id) references rewrite_result(id)
);

create table rewrite_result
(
  id integer primary key,
  test_run_result_str_id text not null,
  actual_result text not null,
  expected_result text not null,
  polluter_diagnosis_id integer not null,

  foreign key(polluter_diagnosis_id) references polluter_diagnosis(id)
);

create table polluter_diagnosis
(
  id integer primary key,
  polluter_data_id integer not null,
  diagnosis_result_id integer not null,

  foreign key(polluter_data_id) references polluter_data(id),
  foreign key(diagnosis_result_id) references diagnosis_result(id)
);

create table diagnosis_result
(
  id integer primary key,
  test_name text not null,
  hash text not null,
  expected_result text not null,

  foreign key(test_name) references original_order(test_name)
);

create table field_diff
(
  id integer primary key,
  field_name text not null,
  xpath text not null,
  inner_field_name text not null,

  -- Intentionally nullable
  without_deps_val text,
  with_deps_val text
);

create table test_patch
(
  id integer primary key,
  subject_name text not null,
  test_name text not null,
  cleaner_name text not null,
  polluter_name text not null,
  modified_test_name text not null,
  status text not null,
  succeeded integer not null,
  patch_line_count integer not null,

  foreign key(subject_name) references subject(name),
  foreign key(test_name) references original_order(test_name)
);
