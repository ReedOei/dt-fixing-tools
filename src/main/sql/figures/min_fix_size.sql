select ifnull(min(patch_line_count - 2), 0)
from test_patch tp
inner join od_classification odc on tp.test_name = odc.test_name
left join
(
  select id, test_name, min(cleaner_count) as total
  from cleaner_info
  group by test_name
) c on c.test_name = tp.test_name
where succeeded = 1 and patch_line_count > 0 and
      odc.od_type like ? and
      (case when c.id is not null then 'has_cleaner' else 'no_cleaner' end like ?);
