create temporary table temp as
select odc.subject_name as subject_name, odc.test_name as test_name, odc.od_type as od_type,
       avg(otd.elapsed_time - otc.elapsed_time) as dep_time,
       avg(otc.elapsed_time) as clean_time,
       avg(otd.elapsed_time) as tot_time
from od_classification odc
inner join minimize_test_result mtr on odc.test_name = mtr.test_name
left join polluter_data pd on mtr.id = pd.minimized_id
left join cleaner_data cd on cd.polluter_data_id = pd.id
left join operation_time otd on otd.id = mtr.operation_time_id
left join operation_time otc on otc.id = cd.operation_time_id
group by odc.test_name;

create temporary table tdata as
select slug,
       ifnull(round(avg(vic.dep_time),1), 'N/A') as pol_time,
       /* ifnull(round(avg(vic.clean_time),1), 'N/A') as clean_time, */
       /* ifnull(round(avg(vic.tot_time),1), 'N/A') as vic_time, */
       ifnull(round(avg(brit.dep_time),1), 'N/A') as set_time
       /* ifnull(avg(brit.clean_time), 'N/A'), */
       /* ifnull(avg(brit.tot_time), 'N/A'), */
from subject_with_od swo
inner join subject s on s.name = swo.subject_name
left join temp vic on vic.subject_name = swo.subject_name and vic.od_type = 'victim'
left join temp brit on brit.subject_name = swo.subject_name and brit.od_type = 'brittle'
group by swo.subject_name;

create temporary table t as
select swo.subject_name as subject_name,
       vic.dep_time as vic_time,
       brit.dep_time as brit_time
from subject_with_od swo
left join temp vic on vic.subject_name = swo.subject_name and vic.od_type = 'victim'
left join temp brit on brit.subject_name = swo.subject_name and brit.od_type = 'brittle';

select '\newcommand{\minVictimTime}{' || min(vic_time) || '}'
from t;
select '\newcommand{\minBrittleTime}{' || min(brit_time) || '}'
from t;
select '\newcommand{\maxVictimTime}{' || max(vic_time) || '}'
from t;
select '\newcommand{\maxBrittleTime}{' || max(brit_time) || '}'
from t;

select *
from tdata
union all
select 'Total',
       ifnull(round(avg(vic_time),1), 'N/A') as pol_time,
       /* ifnull(round(avg(vic.clean_time),1), 'N/A') as clean_time, */
       /* ifnull(round(avg(vic.tot_time),1), 'N/A') as vic_time, */
       ifnull(round(avg(brit_time),1), 'N/A') as set_time
       /* ifnull(avg(brit.clean_time), 'N/A'), */
       /* ifnull(avg(brit.tot_time), 'N/A'), */
from t;

