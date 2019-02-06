SELECT distinct pt.test_name
FROM pr_tests pt
JOIN
(
       select distinct ft.test_name
       from fixable_tests ft
       inner join od_classification odc on ft.test_name = odc.test_name
       left join fixed_tests fixt on fixt.test_name = ft.test_name
       left join
            (select id, test_name, min(cleaner_count) as total
             from cleaner_info
             group by test_name
            ) c on c.test_name = ft.test_name
       where (case when fixt.test_name is null then 0 else 1 end = ?) and
       odc.od_type like ? and
       (case when c.id is not null then 'has_cleaner' else 'no_cleaner' end like ?)
) pa on pa.test_name = pt.test_name;
