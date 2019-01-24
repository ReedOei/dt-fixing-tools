-- Count how many of the tests that have cleaners or setters succeeded
select count(*)
from fixable_tests ft
inner join test_patch tp on tp.test_name = ft.test_name
group by tp.succeeded
having tp.succeeded = ?;
