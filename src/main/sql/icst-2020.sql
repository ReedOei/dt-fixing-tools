-- iDFlakies only
SELECT DISTINCT iv.subject_name,iv.commit_sha,fv.test_name,iv.test_name 
FROM 
  (SELECT DISTINCT ftf.subject_name,ftf.commit_sha,ftf.test_name 
    FROM flaky_test_failures ftf 
    JOIN fs_test_commit_order ftco on ftco.test_name = ftf.test_name and ftco.commit_sha = ftf.commit_sha 
    WHERE ftco.order_num = -1) iv 
LEFT JOIN 
  (SELECT DISTINCT ftf.subject_name,ftf.commit_sha,ftf.test_name 
    FROM flaky_test_failures ftf 
    JOIN fs_test_commit_order ftco on ftco.test_name = ftf.test_name and ftco.commit_sha = ftf.commit_sha where ftco.order_num != -1) fv 
  ON iv.test_name = fv.test_name
JOIN
  (SELECT fstr.commit_sha 
    FROM fs_subj_test_raw fstr
    WHERE fstr.dataset is 'comprehensive') fstr
  on iv.commit_sha = fstr.commit_sha
WHERE fv.test_name is NULL;

SELECT DISTINCT iv.subject_name,iv.commit_sha,iv.test_name,fv.test_name 
FROM 
  (SELECT DISTINCT ftf.subject_name,ftf.commit_sha,ftf.test_name 
    FROM flaky_test_failures ftf 
    JOIN fs_test_commit_order ftco on ftco.test_name = ftf.test_name and ftco.commit_sha = ftf.commit_sha 
    WHERE ftco.order_num != -1) iv 
LEFT JOIN 
  (SELECT DISTINCT ftf.subject_name,ftf.commit_sha,ftf.test_name 
    FROM flaky_test_failures ftf 
    JOIN fs_test_commit_order ftco on ftco.test_name = ftf.test_name and ftco.commit_sha = ftf.commit_sha where ftco.order_num = -1) fv 
  ON iv.test_name = fv.test_name
JOIN
  (SELECT fstr.test_name 
    FROM fs_subj_test_raw fstr
    WHERE fstr.dataset is 'comprehensive') fstr
  on iv.test_name = fstr.test_name
WHERE fv.test_name is NULL;

-- names of slugs that haven't been flaky on iDFlakies sha yet
SELECT DISTINCT fstr.slug 
FROM fs_subj_test_raw fstr 
LEFT JOIN fs_file_loc ffl ON ffl.commit_sha = fstr.commit_sha 
WHERE ffl.commit_sha is NULL AND fstr.dataset is 'comprehensive';

-- names of tests that haven't been flaky once on first sha
SELECT COUNT(DISTINCT fstr.test_name)
FROM fs_subj_test_raw fstr 
JOIN fs_test_commit_order ftco on ftco.test_name = fstr.test_name  
LEFT JOIN flaky_test_failures ftf on ftco.commit_sha = ftf.commit_sha AND ftco.test_name = ftf.test_name
WHERE fstr.dataset is 'comprehensive' AND ftco.order_num > -1 AND ftf.commit_sha is NOT NULL;

SELECT DISTINCT fstr.test_name
FROM fs_subj_test_raw fstr 
JOIN fs_test_commit_order ftco on ftco.test_name = fstr.test_name  
LEFT JOIN fs_experiment fe on fe.short_sha = ftco.short_sha AND ftco.test_name = fe.test_name
WHERE fstr.dataset is 'comprehensive' AND ftco.order_num > -1 AND fe.short_sha is NULL;

-- get the first shas of tests that belong to more than one module
SELECT commit_sha,module,count()
FROM  
  (SELECT DISTINCT ftco.commit_sha, fstr.module
    FROM fs_test_commit_order ftco 
    JOIN fs_subj_test_raw fstr ON fstr.test_name = ftco.test_name 
    WHERE ftco.order_num > -1 
    GROUP BY ftco.commit_sha,fstr.module 
    ORDER BY ftco.commit_sha) 
GROUP BY commit_sha 
HAVING count() > 1;

SELECT iDT.commit_sha,iDT.module,iDT.test_name
FROM  
  (SELECT DISTINCT ftco.commit_sha, fstr.module, ftco.test_name
    FROM fs_test_commit_order ftco 
    JOIN fs_subj_test_raw fstr ON fstr.test_name = ftco.test_name 
    WHERE ftco.order_num = -1 
    GROUP BY ftco.commit_sha,fstr.module 
    ORDER BY ftco.commit_sha) iDT
JOIN fs_experiment fe on fe.test_name = iDT.test_name
where fe.test_file_is_empty != 0;



-- First SHA tests that do not have iDFlakies version ran yet
SELECT DISTINCT p.slug,p.commit_sha,p.test_name
FROM (
  SELECT fe.test_name,fstr.slug,fstr.commit_sha,fstr.module,ftco.short_sha
  FROM fs_experiment fe
  JOIN fs_subj_test_raw fstr ON fstr.test_name = fe.test_name
  JOIN fs_test_commit_order ftco ON fstr.commit_sha = ftco.commit_sha
  WHERE fe.test_file_is_empty != 0) p
LEFT JOIN fs_experiment fe on lower(p.slug) = lower(fe.slug) AND p.short_sha = fe.short_sha
WHERE fe.short_sha is NULL
GROUP BY p.commit_sha,p.module
ORDER BY p.slug;

-- CSVs to find first working SHA that we didn't already have a working SHA for
SELECT DISTINCT fstr.slug,p.commit_sha,fstr.commit_sha
FROM fs_subj_test_raw fstr
JOIN (
  SELECT ftco.test_name,ftco.commit_sha
  FROM fs_test_commit_order ftco
  JOIN fs_experiment fe ON fe.test_name = ftco.test_name 
  WHERE ftco.order_num > -1 AND fe.test_file_is_empty = 0
) p ON p.test_name = fstr.test_name
ORDER BY fstr.slug;


-- Going from non-unique shas to unique shas
SELECT count()
FROM (
SELECT ftco.test_name,ftco.commit_sha,ftco.order_num 
FROM fs_test_commit_order ftco 
JOIN fs_subj_test_raw fstr ON fstr.test_name = ftco.test_name 
WHERE ftco.order_num > -1 AND fstr.dataset is 'comprehensive'
GROUP BY ftco.commit_sha,fstr.module
ORDER BY ftco.test_name
);


-- # of flaky tests with first sha
select count(distinct ftf.test_name) from flaky_test_failures ftf join fs_test_commit_order ftco on ftco.commit_sha = ftf.commit_sha where ftco.order_num > -1;

-- # of idflakies tests with first sha - 365
select count() from fs_test_to_uniq_test;


-- # of idflakies tests that we have a first sha for AND has ran through the pipeline - 346
select count(distinct fttut.orig_test_name) from fs_test_to_uniq_test fttut join fs_experiment fe on fe.test_name = fttut.uniq_test_name  JOIN fs_test_commit_order ftco on ftco.commit_sha = fttut.commit_sha where ftco.order_num > -1;

-- # of idflakies tests that we have a first sha for AND has ran through the pipeline AND did compile - 265
select count(distinct fttut.orig_test_name) from fs_test_to_uniq_test fttut join fs_experiment fe on fe.test_name = fttut.uniq_test_name  JOIN fs_test_commit_order ftco on ftco.commit_sha = fttut.commit_sha where ftco.order_num > -1 and fe.test_file_is_empty > 0;
 
-- # of idflakies tests that we have a first sha for AND has ran through the pipeline AND did compile AND flaky test is found - 89 (unreliable due to fully qualified name vs. class/test name)
select count()  from (select distinct fttut.orig_test_name as test_name, fttut.commit_sha from fs_test_to_uniq_test fttut join fs_experiment fe on fe.test_name = fttut.uniq_test_name  JOIN fs_test_commit_order ftco on ftco.commit_sha = fttut.commit_sha where ftco.order_num > -1 and fe.test_file_is_empty > 0) p join flaky_test_failures ftf on ftf.commit_sha = p.commit_sha and ftf.test_name = p.test_name;

-- Unique first sha that has run - 162
select count() 
FROM (
  SELECT distinct fe.short_sha,fe.test_name from fs_experiment fe join fs_test_commit_order ftco on ftco.short_sha = fe.short_sha where ftco.order_num > -1
);


-- Unique first sha that still needs to be run to have run all 175 first shas - 13
select distinct fstr.slug,fttut.commit_sha,fttut.uniq_test_name from fs_test_to_uniq_test fttut 
join fs_subj_test_raw fstr on fttut.uniq_test_name = fstr.test_name
where (fttut.commit_sha,fttut.uniq_test_name) NOT IN (
  SELECT distinct ftco.commit_sha,fe.test_name from fs_experiment fe join fs_test_commit_order ftco on ftco.short_sha = fe.short_sha where ftco.order_num > -1
) 

-- Generates 922-missing-first-sha-exp.csv
select count()
FROM (
  select distinct fivr.slug,fttut.commit_sha,fttut.uniq_test_name
  from fs_test_to_uniq_test fttut 
  join fs_test_commit_order ftco on ftco.commit_sha = fttut.commit_sha
  join fs_idflakies_vers_results fivr on fivr.test_name = fttut.orig_test_name 
  where ftco.order_num > -1
  and (fttut.commit_sha,lower(fttut.module)) NOT IN (
    select distinct ftco.commit_sha,lower(fttut.module) 
    from fs_experiment fe 
    join fs_test_commit_order ftco on ftco.short_sha = fe.short_sha 
    join fs_idflakies_vers_results fivr on fivr.test_name = ftco.test_name 
    join fs_test_to_uniq_test fttut on fttut.commit_sha = ftco.commit_sha and fttut.orig_test_name = ftco.test_name
    where ftco.order_num > -1
  )
  group by fttut.commit_sha,fttut.module
);


-- tests_found_in_first_sha.csv - 118 
comm -12 <( cat first-sha-found-tests-982.csv | rev | cut -d'.' -f-2 | rev | sort -u ) <( cat tests_compiled-295.csv | rev | cut -d'.' -f-2 | rev | sort -u ) | wc -l

-- 65 tests that were found from iDFlakies rerun and is flaky again in first sha
comm -12 <( cat uniq-flaky-tests-from-idflakies-rerun.csv | rev | cut -d'.' -f-2 | rev | sort -u ) <(sort -u tests_found_in_first_sha-118.csv | tr -d '\r' ) | wc -l

-- 129 tests that were found from iDFlakies and compiles
comm -12 uniq-flaky-tests-from-idflakies-rerun.csv <(sort -u tests_compiled-295.csv | tr -d '\r' ) | wc -l

-- Check whether any test name is mapped to multiple modules (bug in database if there is)
select test_name,commit_sha,count(module) from fs_idflakies_vers_results group by test_name,commit_sha having count(module) > 1;


-- 922 tests names of iDFlakies rerun 
cat flaky-test-in-idflakies-rerun.csv  | cut -d',' -f 4 | sort -u > uniq-flaky-tests-from-idflakies-rerun.csv

-- Unique first sha that finds FT
SELECT count(distinct p.commit_sha)
FROM flaky_test_failures ftf
JOIN (
  select distinct ftco.commit_sha from fs_experiment fe join fs_test_commit_order ftco on ftco.short_sha = fe.short_sha where ftco.order_num > -1 and fe.test_file_is_empty > 0
) p on p.commit_sha = ftf.commit_sha;

-- # of unique flaky tests from iDFlakies found from first SHAs
SELECT count(distinct fstr.test_name)
FROM flaky_test_failures ftf
JOIN fs_subj_test_raw fstr on fstr.test_name = ftf.test_name and ftf.commit_sha = fstr.commit_sha;


-- # of unique flaky tests found from first SHAs - 982 (first-sha-found-tests.csv)
SELECT count(distinct ftf.test_name)
FROM flaky_test_failures ftf
JOIN fs_test_commit_order ftco on ftco.commit_sha = ftf.commit_sha
WHERE ftco.order_num > -1;

SELECT distinct ftf.test_name
FROM flaky_test_failures ftf
JOIN fs_test_commit_order ftco on ftco.commit_sha = ftf.commit_sha
WHERE ftco.order_num > -1;

-- # of flaky tests from iDFlakies verison in ICST
select count() from fs_subj_test_raw;

-- # of projects from iDFlakies verison in ICST
select count(distinct slug) from fs_subj_test_raw ;

-- # of modules from iDFlakies verison in ICST
select count(distinct module) from fs_subj_test_raw;

-- # of flaky tests from iDFlakies verison when we rerun
select count(distinct ftf.test_name) from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha;

-- # of flaky tests union of iDFlakies ICST version and iDFlakies rerun
select count(distinct test_name)
FROM ( 
select distinct fstr.slug as slug,fstr.commit_sha as commit_sha,ftf.test_name as test_name from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha
UNION
select slug,commit_sha,test_name from fs_subj_test_raw);

select count()
from (
  select ftf.test_name, ftf.flaky_type,ftf.commit_sha
  from (
    select distinct test_name, commit_sha
    FROM ( 
    select distinct fstr.slug as slug,fstr.commit_sha as commit_sha,ftf.test_name as test_name from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha
    UNION
    select slug,commit_sha,test_name from fs_subj_test_raw)
  ) p
  join flaky_test_failures ftf on ftf.test_name = p.test_name and ftf.commit_sha = p.commit_sha
);


-- # of flaky tests union of iDFlakies ICST version and iDFlakies rerun that we do not have first sha results for - 831
select p.slug,p.commit_sha,p.test_name
from (
  select slug,commit_sha,test_name
  FROM ( 
  select distinct fstr.slug as slug,fstr.commit_sha as commit_sha,ftf.test_name as test_name from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha
  UNION
  select slug,commit_sha,test_name from fs_subj_test_raw)
) p
where (p.test_name) NOT IN (
  select ftco.test_name
  from fs_test_commit_order ftco
  where ftco.order_num > -1
);

-- # of projects with flaky tests from iDFlakies verison when we rerun
select count(distinct fe.slug) from fs_experiment fe join fs_test_commit_order ftco on ftco.short_sha = fe.short_sha join fs_subj_test_raw fstr on fstr.commit_sha = ftco.commit_sha;


-- # of flaky tests in both iDFlakies rerun and first sha runs
select count(distinct p1.test_name)
from (
  select distinct ftf.test_name from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha
) p1
JOIN (
  select distinct ftf.test_name from flaky_test_failures ftf join fs_test_commit_order ftco on ftco.commit_sha = ftf.commit_sha where ftco.order_num > -1
) p2 on p1.test_name = p2.test_name;

-- # of flaky tests only in iDFlakies rerun but not in first sha runs
select count(distinct p1.test_name)
from (
  select distinct ftf.test_name from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha
) p1
LEFT JOIN (
  select distinct ftf.test_name from flaky_test_failures ftf join fs_test_commit_order ftco on ftco.commit_sha = ftf.commit_sha where ftco.order_num > -1
) p2 on p1.test_name = p2.test_name
WHERE p2.test_name is NULL;

-- # of flaky tests only in first sha runs but not in iDFlakies rerun
select count(distinct p1.test_name)
from (
  select distinct ftf.test_name from flaky_test_failures ftf join fs_test_commit_order ftco on ftco.commit_sha = ftf.commit_sha where ftco.order_num > -1
) p1
LEFT JOIN (
  select distinct ftf.test_name from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha
) p2 on p1.test_name = p2.test_name
WHERE p2.test_name is NULL;


-- # of flaky tests in both iDFlakies rerun and IDFlakies ICST
select count(distinct ftf.test_name) from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha and fstr.test_name = ftf.test_name;

-- # of flaky tests only in iDFlakies rerun and not IDFlakies ICST
SELECT count(distinct ftf.test_name)
FROM (
  select distinct ftf.test_name from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha
) ftf
LEFT JOIN fs_subj_test_raw fstr on ftf.test_name = fstr.test_name
WHERE fstr.test_name is NULL;

-- # of flaky tests only in iDFlakies ICST and not IDFlakies rerun
SELECT count(distinct fstr.test_name)
FROM fs_subj_test_raw fstr
LEFT JOIN (
  select distinct ftf.test_name from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha
) ftf on ftf.test_name = fstr.test_name
WHERE ftf.test_name is NULL;

-- # of flaky tests in both iDFlakies rerun and IDFlakies ICST and 
select count() 
from (
SELECT fttqt.uniq_test_name, fttqt.commit_sha, count()
FROM (
  select distinct ftf.test_name,ftf.commit_sha from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha and fstr.test_name = ftf.test_name
) p
JOIN fs_test_to_uniq_test fttqt on fttqt.orig_test_name = p.test_name
GROUP BY fttqt.uniq_test_name,fttqt.commit_sha);

select count(distinct p2.orig_test_name)
from (
  SELECT fttqt.uniq_test_name as uniq_test_name, fttqt.commit_sha, p.test_name as orig_test_name
  FROM (
    select distinct ftf.test_name,ftf.commit_sha from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha and fstr.test_name = ftf.test_name
  ) p
  JOIN fs_test_to_uniq_test fttqt on fttqt.orig_test_name = p.test_name
) p2
JOIN flaky_test_failures ftf on p2.orig_test_name = ftf.test_name and ftf.commit_sha = p2.commit_sha;

select count(distinct p2.uniq_test_name)
from (
  SELECT fttqt.uniq_test_name as uniq_test_name, fttqt.commit_sha, p.test_name as orig_test_name
  FROM (
    select distinct ftf.test_name,ftf.commit_sha from flaky_test_failures ftf join fs_subj_test_raw fstr on fstr.commit_sha = ftf.commit_sha and fstr.test_name = ftf.test_name
  ) p
  JOIN fs_test_to_uniq_test fttqt on fttqt.orig_test_name = p.test_name
) p2
JOIN flaky_test_failures ftf on p2.uniq_test_name = ftf.test_name and ftf.commit_sha = p2.commit_sha;



