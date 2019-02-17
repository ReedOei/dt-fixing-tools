SELECT * FROM fix_method_order_tests
UNION
SELECT * FROM original_order WHERE fix_method_order != 0;