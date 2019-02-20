select subject_name as subject_name, test_count, victim_count + brittle_count as totalCount, victim_count, brittle_count,
       case when victim_count = 0 then '\na' else polluter_count end as polluter_count,
       case when polluter_count = 0 then '\na' else cleaner_count end as cleaner_count,
       case when brittle_count = 0 then '\na' else setter_count end as setter_count,
       case when victim_count = 0 then '\na' else vic_with_clean_count end as vic_with_clean_count,
       ifnull(round(cast(polluter_count as float) / victim_count, 1), '\na') as polluter_per_victim,
       ifnull(round(cast(setter_count as float) / brittle_count, 1), '\na') as setter_per_brittle,
       ifnull(round(cast(cleaner_count as float) / polluter_count, 1), '\na') as cleaner_per_polluter,
       ifnull(round(cast(cleaner_count as float) / victim_count, 1), '\na') as cleaner_per_victim
from
(select otn.subject_name as subject_name,
       ifnull(tn.n, 0) as test_count,
       otn.v_n as victim_count,
       otn.b_n as brittle_count,
       dn.pn as polluter_count,
       ifnull(cn.n, 0) as cleaner_count,
       dn.sn as setter_count,
       ifnull(vcn.n, 0) as vic_with_clean_count
from
  (select subject_name as subject_name,
   sum(case when od_type = 'victim' then 1 else 0 end) as v_n,
   sum(case when od_type = 'brittle' then 1 else 0 end) as b_n
   from od_classification
   group by subject_name
  ) otn
left join
     (select subject_name as subject_name, count(*) as n
      from original_order
      group by subject_name
     ) tn on tn.subject_name = otn.subject_name
inner join
      (select otn.subject_name as subject_name, ifnull(p.n, 0) as pn, ifnull(s.n, 0) as sn
       from
          (select subject_name as subject_name,
           sum(case when od_type = 'victim' then 1 else 0 end) as v_n,
           sum(case when od_type = 'brittle' then 1 else 0 end) as b_n
           from od_classification
           group by subject_name
          ) otn
       left join
            (select dg.subject_name, count(*) as n
             from dependency_groups dg
             inner join od_classification odc on odc.test_name = dg.test_name
             where odc.od_type = 'victim'
             group by dg.subject_name
            ) p on p.subject_name = otn.subject_name
      left join
           (select dg.subject_name, count(*) as n
            from dependency_groups dg
            inner join od_classification odc on odc.test_name = dg.test_name
            where odc.od_type = 'brittle'
            group by dg.subject_name
           ) s on s.subject_name = otn.subject_name
      ) dn on dn.subject_name = otn.subject_name
left join
     (select subject_name as subject_name, count(*) as n
      from cleaner_groups
      group by subject_name
     ) cn on cn.subject_name = otn.subject_name
left join
     (select subject_name as subject_name, count(*) as n
      from
          (select distinct oc.subject_name, oc.test_name
           from od_classification oc
           join cleaner_groups cg on cg.subject_name = oc.subject_name
           where oc.od_type = 'victim'
           and oc.test_name = cg.test_name)
      group by subject_name
     ) vcn on vcn.subject_name = otn.subject_name
order by otn.subject_name)
inner join subject s on s.name = subject_name
union all
select 'Total' as subject_name, sum(test_count) as test_count, sum(victim_count + brittle_count) as totalCount, sum(victim_count) as victim_count, sum(brittle_count) as brittle_count, sum(polluter_count) as polluter_count, sum(cleaner_count) as cleaner_count, sum(setter_count) as setter_count, sum(vic_with_clean_count) as vic_with_clean_count,
       round(cast(sum(polluter_count) as float) / sum(victim_count), 1) as polluter_per_victim,
       round(cast(sum(setter_count) as float) / sum(brittle_count), 1) as setter_per_brittle,
       round(cast(sum(cleaner_count) as float) / sum(polluter_count), 1) as cleaner_per_polluter,
       round(cast(sum(cleaner_count) as float) / sum(victim_count), 1) as cleaner_per_victim
from
(select otn.subject_name as subject_name,
       ifnull(tn.n, 0) as test_count,
       otn.v_n as victim_count,
       otn.b_n as brittle_count,
       dn.pn as polluter_count,
       ifnull(cn.n, 0) as cleaner_count,
       dn.sn as setter_count,
       ifnull(vcn.n, 0) as vic_with_clean_count
from
  (select subject_name as subject_name,
   sum(case when od_type = 'victim' then 1 else 0 end) as v_n,
   sum(case when od_type = 'brittle' then 1 else 0 end) as b_n
   from od_classification
   group by subject_name
  ) otn
left join
     (select subject_name as subject_name, count(*) as n
      from original_order
      group by subject_name
     ) tn on tn.subject_name = otn.subject_name
inner join
      (select otn.subject_name as subject_name, ifnull(p.n, 0) as pn, ifnull(s.n, 0) as sn
       from
          (select subject_name as subject_name,
           sum(case when od_type = 'victim' then 1 else 0 end) as v_n,
           sum(case when od_type = 'brittle' then 1 else 0 end) as b_n
           from od_classification
           group by subject_name
          ) otn
       left join
            (select dg.subject_name, count(*) as n
             from dependency_groups dg
             inner join od_classification odc on odc.test_name = dg.test_name
             where odc.od_type = 'victim'
             group by dg.subject_name
            ) p on p.subject_name = otn.subject_name
      left join
           (select dg.subject_name, count(*) as n
            from dependency_groups dg
            inner join od_classification odc on odc.test_name = dg.test_name
            where odc.od_type = 'brittle'
            group by dg.subject_name
           ) s on s.subject_name = otn.subject_name
      ) dn on dn.subject_name = otn.subject_name
left join
     (select subject_name as subject_name, count(*) as n
      from cleaner_groups
      group by subject_name
     ) cn on cn.subject_name = otn.subject_name
left join
     (select subject_name as subject_name, count(*) as n
      from
          (select distinct oc.subject_name, oc.test_name
           from od_classification oc
           join cleaner_groups cg on cg.subject_name = oc.subject_name
           where oc.od_type = 'victim'
           and oc.test_name = cg.test_name)
      group by subject_name
     ) vcn on vcn.subject_name = otn.subject_name
order by otn.subject_name
);
