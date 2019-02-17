SELECT test_name FROM fix_method_order_tests
UNION
SELECT test_name FROM original_order WHERE fix_method_order != 0;
