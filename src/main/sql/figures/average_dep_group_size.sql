select ifnull(avg(dg.dep_count), 0)
from dependency_groups dg
inner join od_classification odc on odc.test_name = dg.test_name
where od_type like ? and
      dg.dep_count >= ? and
      dg.dep_count <= ?;
