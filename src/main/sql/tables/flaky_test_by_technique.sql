select i.slug,
       case when ifnull(orig.number, 0) = 0 then '-' else i.flaky_original end,
       case when ifnull(random.number, 0) = 0 then '-' else i.flaky_random end,
       case when ifnull(random.number, 0) = 0 then '-' else i.random_random end,
       case when ifnull(rc.number, 0) = 0 then '-' else i.flaky_random_class end,
       case when ifnull(rc.number, 0) = 0 then '-' else i.random_random_class end,
       case when ifnull(rev.number, 0) = 0 then '-' else i.flaky_rev end,
       case when ifnull(rev.number, 0) = 0 then '-' else i.random_rev end,
       case when ifnull(revc.number, 0) = 0 then '-' else i.flaky_rev_class end,
       case when ifnull(revc.number, 0) = 0 then '-' else i.random_rev_class end,
	   ifnull(fcount.n, 0) as no_n,
	   ifnull(rcount.n, 0) as od_n,
	   ifnull(fcount.n, 0) + ifnull(rcount.n, 0) as all_n
from
(
	select s.slug,
		   sum(ifnull(f.original_found, 0)) as flaky_original,
		   sum(ifnull(f.rand_found, 0)) as flaky_random,
		   sum(ifnull(r.rand_found, 0)) as random_random,
		   sum(ifnull(f.rand_class_found, 0)) as flaky_random_class,
		   sum(ifnull(r.rand_class_found, 0)) as random_random_class,
		   sum(ifnull(f.rev_found, 0)) as flaky_rev,
		   sum(ifnull(r.rev_found, 0)) as random_rev,
		   sum(ifnull(f.rev_class_found, 0)) as flaky_rev_class,
		   sum(ifnull(r.rev_class_found, 0)) as random_rev_class
	from flaky_test_classification ftc
	inner join subject s on ftc.subject_name = s.name
	left join
	(
	  select ftc.subject_name, ftc.test_name, ftc.flaky_type,
		   case when flaky.test_name is null then 0 else 1 end as original_found,
		   case when rand.test_name is null then 0 else 1 end as rand_found,
		   case when rand_class.test_name is null then 0 else 1 end as rand_class_found,
		   case when rev.test_name is null then 0 else 1 end as rev_found,
		   case when rev_class.test_name is null then 0 else 1 end as rev_class_found
	  from flaky_test_classification ftc
	  left join flaky_test_failures flaky on ftc.test_name = flaky.test_name and flaky.round_type = 'original'
	  left join flaky_test_failures rand on ftc.test_name = rand.test_name and rand.round_type = 'random'
	  left join flaky_test_failures rand_class on ftc.test_name = rand_class.test_name and rand_class.round_type = 'random-class'
	  left join flaky_test_failures rev on ftc.test_name = rev.test_name and rev.round_type = 'reverse'
	  left join flaky_test_failures rev_class on ftc.test_name = rev_class.test_name and rev_class.round_type = 'reverse-class'
	) f on f.subject_name = ftc.subject_name and f.test_name = ftc.test_name and f.flaky_type = 'NO'
	left join
	(
	  select ftc.subject_name, ftc.test_name, ftc.flaky_type,
		   case when flaky.test_name is null then 0 else 1 end as original_found,
		   case when rand.test_name is null then 0 else 1 end as rand_found,
		   case when rand_class.test_name is null then 0 else 1 end as rand_class_found,
		   case when rev.test_name is null then 0 else 1 end as rev_found,
		   case when rev_class.test_name is null then 0 else 1 end as rev_class_found
	  from flaky_test_classification ftc
	  left join flaky_test_failures flaky on ftc.test_name = flaky.test_name and flaky.round_type = 'original'
	  left join flaky_test_failures rand on ftc.test_name = rand.test_name and rand.round_type = 'random'
	  left join flaky_test_failures rand_class on ftc.test_name = rand_class.test_name and rand_class.round_type = 'random-class'
	  left join flaky_test_failures rev on ftc.test_name = rev.test_name and rev.round_type = 'reverse'
	  left join flaky_test_failures rev_class on ftc.test_name = rev_class.test_name and rev_class.round_type = 'reverse-class'
	) r on r.subject_name = ftc.subject_name and r.test_name = ftc.test_name and r.flaky_type = 'OD'
	group by s.slug
) i
left join
(
	select slug, flaky_type, sum(number) as n
	from flaky_test_counts ftc
	inner join subject s on s.name = ftc.subject_name
	group by slug, flaky_type
) fcount on lower(fcount.slug) = lower(i.slug) and fcount.flaky_type = 'NO'
left join
(
	select slug, flaky_type, sum(number) as n
	from flaky_test_counts ftc
	inner join subject s on s.name = ftc.subject_name
	group by slug, flaky_type
) rcount on lower(rcount.slug) = lower(i.slug) and rcount.flaky_type = 'OD'
left join
(
    select s.slug, sum(nr.number) as number
    from num_rounds nr
    inner join subject s on nr.name = s.name
    where nr.round_type = 'original'
    group by s.slug
) orig on orig.slug = i.slug
left join
(
    select s.slug, sum(nr.number) as number
    from num_rounds nr
    inner join subject s on nr.name = s.name
    where nr.round_type = 'random'
    group by s.slug
) random on random.slug = i.slug
left join
(
    select s.slug, sum(nr.number) as number
    from num_rounds nr
    inner join subject s on nr.name = s.name
    where nr.round_type = 'random-class'
    group by s.slug
) rc on rc.slug = i.slug
left join
(
    select s.slug, sum(nr.number) as number
    from num_rounds nr
    inner join subject s on nr.name = s.name
    where nr.round_type = 'reverse'
    group by s.slug
) rev on rev.slug = i.slug
left join
(
    select s.slug, sum(nr.number) as number
    from num_rounds nr
    inner join subject s on nr.name = s.name
    where nr.round_type = 'reverse-class'
    group by s.slug
) revc on revc.slug = i.slug
order by i.slug;

