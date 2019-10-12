-- We insert results for every single subject into every subject, so delete the ones that don't match
delete from test_patch
where id in
-- apparently this is how you do it in sqlite, as we can't join in deletes
(
  select tp.id
  from test_patch tp
  inner join original_order oo on oo.test_name = tp.test_name
  where oo.subject_name <> tp.subject_name
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
       case
        when dr.round_type = 'original' then 'NO'
        else 'OD'
       end as flaky_type,
       ft.id as flaky_test_id,
       ft.name as test_name,
       ft.commit_sha as commit_sha
from flaky_test ft
inner join flaky_test_list as ftl on ftl.flaky_test_id = ft.id and ftl.commit_sha = ft.commit_sha
inner join detection_round as dr on dr.unfiltered_id = ftl.flaky_test_list_id and ft.commit_sha = dr.commit_sha;

create view filtered_flaky_tests as
select dr.id as detection_round_id,
       dr.subject_name,
       case
        when dr.round_type = 'original' then 'NO'
        else 'OD'
       end as flaky_type,
       ft.id as flaky_test_id,
       ft.name as test_name,
       ft.commit_sha as commit_sha
from flaky_test ft
inner join flaky_test_list as ftl on ftl.flaky_test_id = ft.id and ftl.commit_sha = ft.commit_sha
inner join detection_round as dr on dr.filtered_id = ftl.flaky_test_list_id and ft.commit_sha = dr.commit_sha;

create view confirmation_by_test as
select cr.test_name,
       sum(case
            when cr.passing_result = cr.passing_expected_result and
                 cr.failing_result = cr.failing_expected_result then 1
            else 0
           end) as confirmed_runs,
       count(*) as total_runs,
       cr.commit_sha
from confirmation_runs as cr
group by cr.test_name,cr.commit_sha;

create view flaky_test_info as
select distinct uft.detection_round_id,
                uft.subject_name,
                case
                  -- this means that it was filtered out for being flaky and is NOT dependent
                  when fft.test_name is null then 'NO'
                  else uft.flaky_type
                end as flaky_type,
                uft.flaky_test_id,
                uft.test_name,
                uft.commit_sha as commit_sha
from unfiltered_flaky_tests as uft
left join filtered_flaky_tests as fft on uft.test_name = fft.test_name and uft.subject_name = fft.subject_name and uft.commit_sha = fft.commit_sha;

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
     count(*) as total_runs,
     ftc.commit_sha
from flaky_test_classification as ftc
inner join confirmation_runs as cr on ftc.test_name = cr.test_name and ftc.commit_sha = cr.commit_sha
group by ftc.test_name, ftc.flaky_type, cr.round_type, ftc.commit_sha;

create view minimized_tests as
select distinct subject_name, test_name
from minimize_test_result;

create view polluter_data_count as
select mt.subject_name,
       mt.test_name,
       count(passing.num) as passing_count,
       count(failing.num) as failing_count
from minimized_tests mt
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

create view all_no_test as
select distinct test_name
from
(
  select mtr.test_name
  from minimize_test_result mtr
  left join od_classification odc on mtr.test_name = odc.test_name
  where odc.test_name is null

  union

  select test_name
  from no_test
) t
where test_name NOT IN
    (SELECT test_name
     FROM incompatible_tests)
and test_name NOT IN
    (SELECT test_name
     FROM separate_jvm_tests)
and test_name NOT IN
    (SELECT test_name
     FROM unfinished_tests)
and test_name NOT IN
    (SELECT test_name
     FROM original_order
     WHERE fix_method_order != 0);

insert into test_patch
(
  subject_name,
  test_name,
  cleaner_name,
  polluter_name,
  modified_test_name,
  status,
  succeeded,
  patch_line_count
)
select odc.subject_name,
       odc.test_name,
       'N/A',
       'N/A',
       'N/A',
       'UNKNOWN ERROR',
       0,
       -1
from od_classification odc
left join test_patch tp on tp.test_name = odc.test_name
where tp.test_name is null;

create view subject_with_od as
select distinct subject_name
from od_classification;

create view dependency_info as
select mtr.subject_name,
       mtr.test_name,
       mtr.expected_result,
       pd.id,
       count(d.test_name) as dep_count
from od_classification odc
inner join minimize_test_result mtr on mtr.test_name = odc.test_name
left join polluter_data pd on mtr.id = pd.minimized_id
left join dependency d on pd.id = d.polluter_data_id
group by mtr.subject_name, mtr.test_name, mtr.expected_result, pd.id
having dep_count > 0;

create view cleaner_info as
select mtr.subject_name,
       mtr.test_name,
       mtr.expected_result,
       cg.id,
       count(ct.test_name) as cleaner_count
from od_classification odc
inner join minimize_test_result mtr on mtr.test_name = odc.test_name
left join polluter_data pd on mtr.id = pd.minimized_id
left join cleaner_data cd on cd.polluter_data_id = pd.id
left join cleaner_group cg on cg.cleaner_data_id = cd.id
left join cleaner_test ct on ct.cleaner_group_id = cg.id
group by mtr.subject_name, mtr.test_name, mtr.expected_result, cg.id;

create view tests_with_cleaner as
select test_name, max(cleaner_count) as total
from cleaner_info
group by test_name
having total > 0;

create view tests_with_setter as
select di.test_name, max(dep_count) as total
from dependency_info di
inner join od_classification as odc on di.test_name = odc.test_name
where odc.od_type = 'brittle' and di.expected_result = 'PASS'
group by di.test_name
having total > 0;

create view tests_with_polluter as
select di.test_name, max(di.dep_count) as total
from dependency_info di
inner join od_classification as odc on di.test_name = odc.test_name
where odc.od_type = 'victim' and di.expected_result <> 'PASS'
group by di.test_name
having total > 0;

create view fixable_tests as
select odc.subject_name, odc.test_name, odc.od_type
from od_classification odc
left join tests_with_cleaner c on c.test_name = odc.test_name
left join tests_with_setter s on s.test_name = odc.test_name
where c.test_name is not null or s.test_name is not null;

create view diagnosis_info as
select odc.subject_name, odc.test_name,
       pdi.polluter_data_id as polluter_data_id,
       count(distinct rt.field_name) as fields
from od_classification odc
inner join diagnosis_result dr on odc.test_name = dr.test_name
inner join polluter_diagnosis pdi on pdi.diagnosis_result_id = dr.id
inner join rewrite_result rr on rr.polluter_diagnosis_id = pdi.id
inner join rewrite_target rt on rt.rewrite_result_id = rr.id
where rr.actual_result <> rr.expected_result
group by odc.subject_name, odc.test_name, pdi.polluter_data_id;

create view diagnosed_tests as
select distinct odc.test_name
from od_classification odc
left join
(
  select test_name, min(fields) as fields
  from diagnosis_info
  group by test_name
) di on di.test_name = odc.test_name
where ifnull(fields, 0) > 0;

create view dependency_groups as
select pd.id as polluter_data_id,
       mtr.subject_name,
       mtr.test_name,
       group_concat(d.test_name) as deps,
       count(*) as dep_count
from od_classification odc
inner join minimize_test_result mtr on mtr.test_name = odc.test_name
inner join polluter_data pd on mtr.id = pd.minimized_id
inner join dependency d on d.polluter_data_id = pd.id
group by pd.id, mtr.subject_name, mtr.test_name;

create view cleaner_groups as
select cg.id as cleaner_group_id,
       dg.polluter_data_id,
       dg.subject_name,
       dg.test_name,
       group_concat(ct.test_name) as cleaners,
       count(*) as cleaner_count
from dependency_groups dg
inner join cleaner_data cd on cd.polluter_data_id = dg.polluter_data_id
inner join cleaner_group cg on cg.cleaner_data_id = cd.id
inner join cleaner_test ct on ct.cleaner_group_id = cg.id
group by cg.id, dg.polluter_data_id, dg.subject_name, dg.test_name;

create view fixed_tests as
select distinct ft.test_name
from fixable_tests ft
inner join test_patch tp on tp.test_name = ft.test_name
where tp.succeeded = 1;

insert into confirmation_runs
select p.test_name,
       p.round_type,
       p.round_number,
       p.verify_round_number,
       p.expected_result,
       p.result,
       f.expected_result,
       f.result,
       p.commit_sha
from verify_round p
inner join verify_round f on p.test_name = f.test_name and
                             p.round_number = f.round_number and
                             p.verify_round_number = f.verify_round_number and
                             p.commit_sha = f.commit_sha
where p.expected_result = 'PASS' and f.expected_result <> 'PASS';

insert into flaky_test_classification
select info.subject_name,
       info.test_name,
       case
         when info.flaky_runs > 0 then 'NO' -- If it was EVER NO, then we should consider it an NO test
        else 'OD'
      end as flaky_type,
      info.commit_sha
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
  count(*) as total_runs,
  fti.commit_sha as commit_sha
  from flaky_test_info as fti
  left join confirmation_by_test as cbt on fti.test_name = cbt.test_name and fti.commit_sha = cbt.commit_sha
  group by fti.subject_name, fti.test_name, fti.commit_sha
) as info;
--inner join original_order o on info.subject_name = o.subject_name and info.test_name = o.test_name and o.commit_sha = info.commit_sha;

insert into num_rounds
select subject_name, round_type, count(*) as number
from detection_round
group by subject_name, round_type;

create temporary table temp
(
  subject_name,
  round_type,
  test_name,
  detection_round_id,
  commit_sha
);

insert into temp
select fti.subject_name, dr.round_type, fti.test_name, dr.id, fti.commit_sha
from flaky_test_info fti
inner join detection_round dr on fti.detection_round_id = dr.id and fti.commit_sha = dr.commit_sha;

create temporary table temp2
(
  subject_name,
  round_type,
  flaky_type,
  test_name,
  detection_round_id,
  commit_sha
);

insert into temp2
select t.subject_name, t.round_type, ftc.flaky_type, t.test_name, t.detection_round_id, t.commit_sha
from temp t
inner join flaky_test_classification ftc on t.test_name = ftc.test_name and ftc.commit_sha = t.commit_sha;

create temporary table temp3
(
  subject_name,
  round_type,
  commit_sha,
  n
);

insert into temp3
select subject_name, round_type, commit_sha, count(*) as n
from detection_round
group by subject_name, round_type, commit_sha;

insert into flaky_test_failures
select i.subject_name, i.test_name, i.round_type, i.flaky_type, failures, rounds, t.commit_sha
from
(
  select subject_name, t2.test_name, t2.round_type, t2.flaky_type, count(distinct detection_round_id) as failures, t2.commit_sha
  from temp2 t2
  group by t2.test_name, t2.round_type, t2.flaky_type, t2.commit_sha
) i
inner join
(
  select subject_name, round_type, commit_sha, sum(n) as rounds
  from temp3 t3
  group by subject_name, round_type, commit_sha
) t on i.round_type = t.round_type and i.subject_name = t.subject_name and t.commit_sha = i.commit_sha;

insert into detection_round_failures
select dr.id, dr.round_type,
       sum(case when ftc.flaky_type = 'NO' then 1 else 0 end),
       sum(case when ftc.flaky_type = 'OD' then 1 else 0 end),
       dr.commit_sha
from detection_round dr
left join flaky_test_list ftl on dr.unfiltered_id = ftl.flaky_test_list_id and dr.commit_sha = ftl.commit_sha
left join flaky_test ft on ftl.flaky_test_id = ft.id and ft.commit_sha = dr.commit_sha
left join flaky_test_classification ftc on ft.name = ftc.test_name and ftc.commit_sha = dr.commit_sha
group by dr.id, dr.round_type, dr.commit_sha;

create view fs_rq1_tests_found_in_first_sha_details as 
select distinct ftf.subject_name as slug,ftf.test_name,ftf.flaky_type,ftf.commit_sha
from flaky_test_failures ftf
join fs_test_commit_order ftco on ftf.commit_sha = ftco.commit_sha
join (
  select distinct ft.name as test_name
  from flaky_test ft
  join (
    select distinct ft.class_test_name,ft.name
    from (
      select distinct ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha,SUM(ftf.failures),SUM(ftf.rounds)
      from fs_test_to_uniq_test fttut 
      join fs_experiment fe on fe.test_name = fttut.uniq_test_name
      JOIN fs_test_commit_order ftco on ftco.commit_sha = fttut.commit_sha
      join fs_idflakies_vers_results fivr on fivr.test_name = fttut.orig_test_name
      JOIN flaky_test_failures ftf on ftf.test_name = fivr.test_name and fivr.commit_sha = ftf.commit_sha
      where ftco.order_num > -1 and fe.test_file_is_empty > 0
      group by ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha
    ) p
    join flaky_test ft on ft.name = p.test_name
  ) p on p.class_test_name = ft.class_test_name
) p on p.test_name = ftf.test_name
where ftco.order_num > -1
group by ftf.subject_name,ftf.test_name,ftf.flaky_type;

create view fs_rq1_first_sha_flaky_info as
select ftf.subject_name, ftf.test_name, ftf.round_type, ftf.flaky_type, ftf.failures, ftf.rounds, ftf.commit_sha
from flaky_test_failures ftf
join fs_test_commit_order ftco on ftco.commit_sha = ftf.commit_sha
where ftco.order_num > -1;

create view fs_rq1_tests_compiled as
select distinct ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha,SUM(ftf.failures) as failures,SUM(ftf.rounds) as rounds
from fs_test_to_uniq_test fttut 
join fs_experiment fe on fe.test_name = fttut.uniq_test_name
JOIN fs_test_commit_order ftco on ftco.commit_sha = fttut.commit_sha
join fs_idflakies_vers_results fivr on fivr.test_name = fttut.orig_test_name
JOIN flaky_test_failures ftf on ftf.test_name = fivr.test_name and fivr.commit_sha = ftf.commit_sha
where ftco.order_num > -1 and fe.test_file_is_empty > 0
group by ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha;

create view fs_rq1_modules_compiled as
select distinct ftco.commit_sha,fttut.module 
from fs_experiment fe 
join fs_test_commit_order ftco on ftco.short_sha = fe.short_sha 
join fs_idflakies_vers_results fivr on fivr.test_name = ftco.test_name
join fs_test_to_uniq_test fttut on fttut.commit_sha = ftco.commit_sha and fttut.orig_test_name = ftco.test_name
where ftco.order_num > -1 and fe.test_file_is_empty > 0;

create view fs_rq1_modules_tried_compiling as
select distinct ftco.commit_sha,fttut.module 
from fs_experiment fe 
join fs_test_commit_order ftco on ftco.short_sha = fe.short_sha 
join fs_idflakies_vers_results fivr on fivr.test_name = ftco.test_name 
join fs_test_to_uniq_test fttut on fttut.commit_sha = ftco.commit_sha and fttut.orig_test_name = ftco.test_name
where ftco.order_num > -1;

create view fs_rq1_tests_tried_compiling as
select distinct ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha,SUM(ftf.failures) as failures,SUM(ftf.rounds) as rounds
from fs_test_to_uniq_test fttut 
join fs_experiment fe on fe.test_name = fttut.uniq_test_name
JOIN fs_test_commit_order ftco on ftco.commit_sha = fttut.commit_sha
join fs_idflakies_vers_results fivr on fivr.test_name = fttut.orig_test_name
JOIN flaky_test_failures ftf on ftf.test_name = fivr.test_name and fivr.commit_sha = ftf.commit_sha
where ftco.order_num > -1
group by ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha;

create view fs_rq1_modules_with_first_sha as
select distinct fttut.commit_sha,fttut.module 
from fs_test_to_uniq_test fttut 
join fs_test_commit_order ftco on ftco.commit_sha = fttut.commit_sha
join fs_idflakies_vers_results fivr on fivr.test_name = fttut.orig_test_name 
where ftco.order_num > -1;

create view fs_rq1_tests_with_first_sha as
select distinct ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha,SUM(ftf.failures) as failures,SUM(ftf.rounds) as rounds
FROM fs_idflakies_vers_results fivr 
JOIN fs_test_commit_order ftco on ftco.test_name = fivr.test_name
JOIN flaky_test_failures ftf on ftf.test_name = fivr.test_name and fivr.commit_sha = ftf.commit_sha
WHERE ftco.order_num > -1
group by ftf.subject_name,ftf.test_name,ftf.flaky_type,ftf.commit_sha;

create view fs_idf_to_first_test_name as
select p.new_test_name,p.orig_test_name
from (
  select distinct ft.name as new_test_name, p.name as orig_test_name
  from flaky_test ft
  join (
    select distinct ft.class_test_name,ft.name
    from fs_rq1_tests_compiled p
    join flaky_test ft on ft.name = p.test_name
  ) p on p.class_test_name = ft.class_test_name
) p;

create view fs_sha_mod_map as
select distinct fivr.commit_sha as idflakies_sha, fivr.module as idflakies_module, ftco.commit_sha as first_sha, ftf.subject_name as first_sha_module
from fs_idflakies_vers_results fivr
join fs_test_commit_order ftco on fivr.test_name = ftco.test_name
join flaky_test_failures ftf on ftf.commit_sha = ftco.commit_sha
where ftco.order_num > -1;

create view fs_prior_sha_to_idf_sha as
select distinct fivr.test_name as idf_test_name, fivr.commit_sha as idf_sha, fivr.module as idf_module, ftf.test_name as first_test_name, ftf.commit_sha as first_sha, ftf.subject_name as first_module
from fs_idflakies_vers_results fivr
join fs_sha_mod_map fsmm on fsmm.idflakies_sha = fivr.commit_sha and fsmm.idflakies_module = fivr.module
join fs_idf_to_first_test_name fit on fit.orig_test_name = fivr.test_name
join flaky_test_failures ftf on ftf.test_name = fit.new_test_name and fsmm.first_sha = ftf.commit_sha and fsmm.first_sha_module = ftf.subject_name
join fs_test_commit_order ftco on ftco.test_name = fivr.test_name
where ftco.order_num > -1;

create view fs_first_sha_to_idf_sha as
select distinct t.idf_test_name,t.idf_sha,t.idf_module,t.first_test_name,t.first_sha,t.first_module
from fs_prior_sha_to_idf_sha t
join fs_test_commit_order ftco on ftco.test_name = t.idf_test_name and ftco.commit_sha = t.first_sha;
-- where idf_test_name != first_test_name
-- where first_test_name like '%TestEntityWithIndicesForJSON.should_query_using_collection_index_fromJSON%'

create view fs_idflakies_vers_results as
select distinct fstr.slug as slug,fstr.commit_sha as commit_sha,ftf.test_name as test_name,ftf.subject_name as module from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha;

create view fs_idflakies_vers_all_results as
select distinct fstr.slug as slug,fstr.commit_sha as commit_sha,ftf.test_name as test_name, fstr.module from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha
UNION
select slug,commit_sha,test_name,module from fs_subj_test_raw;

-- real only in idflakies rerun tests
-- select count(distinct ftf.test_name) as test_name from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha where ftf.test_name NOT IN (select fstr.test_name from fs_subj_test_raw fstr);

create view fs_test_to_uniq_test as
SELECT ftco.test_name as orig_test_name,ufv.commit_sha,ufv.module,ufv.test_name as uniq_test_name
FROM fs_test_commit_order ftco
JOIN
  (SELECT ftco.commit_sha,fivr.module,ftco.test_name
    FROM fs_idflakies_vers_results fivr
    JOIN fs_test_commit_order ftco ON fivr.test_name = ftco.test_name
    WHERE ftco.order_num > -1
    GROUP BY ftco.commit_sha,fivr.module
    ORDER BY ftco.commit_sha) ufv ON ftco.commit_sha = ufv.commit_sha
JOIN fs_idflakies_vers_results fivr ON fivr.test_name = ftco.test_name AND fivr.module = ufv.module;
