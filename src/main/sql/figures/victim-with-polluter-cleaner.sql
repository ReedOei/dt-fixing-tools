select distinct pd.victim_name
from (
select pt.test_name as victim_name, dg.polluter_data_id, dg.deps as polluters
from
(
select odc.test_name, count(distinct dg.deps) as num
from od_classification odc
inner join dependency_groups dg on dg.test_name = odc.test_name
where od_type like 'victim' and
dg.dep_count >= 0 and
dg.dep_count <= 10000000
group by odc.test_name
having num >= 2 and num <= 10000000) pt
join dependency_groups dg on dg.test_name = pt.test_name) pd
join cleaner_groups cd on cd.polluter_data_id = pd.polluter_data_id
order by pd.victim_name, pd.polluters;

