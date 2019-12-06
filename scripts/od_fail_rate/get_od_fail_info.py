# Usage: python -c'import get_od_fail_info; get_od_fail_info.main("test.csv")'
# python -c'import get_od_fail_info; print(get_od_fail_info.calculate("all_polluter_cleaner_info.csv"))'

import os.path
import os
import math
import json
from decimal import Decimal

def multiplyList(myList) : 
    # Multiply elements one by one 
    result = 1
    for x in myList: 
         result = result * x  
    return result  

def factorial(n):
    return Decimal(math.factorial(n))

def pol(p,c):
    return p * factorial(c + p - 1)

def p_fail(p, c): 
    if p == 0:
        return 0
    else:
        return p * factorial(p + c - 1) * (p + c) / factorial(p + c + 1)

def ord(p, c): 
    return factorial(p + c)

def ord_class(test_classes):
    return factorial(len(test_classes)) * multiplyList(ord(p, c) for p, c in test_classes)

def p_fail_class(t_v, test_classes):
    p, c = t_v 
    prob = p_fail(p, c)

    all_ord_num = ord_class([(p + 1, c)] + test_classes) # We want to count the victim in the set of orders, so add 1 to p (doesn't matter polluter or cleaner)
    for i, t_p in enumerate(test_classes):
        ord_num = ord_class([t_c for j, t_c in enumerate(test_classes) if i != j]) 
        prob += pol(*t_p) * ord_num * len(test_classes) * ord(*t_v) / all_ord_num

    return prob


def split_line(line) :
    result = line.split(",")
    return (result[0],result[1],result[2])

def add_to_dict(dict, v, val):
    if v in dict.keys(): 
        result = dict[v]
    else: 
        result = []
    result.append(val)
    dict[v] = result

def uniq_len(values):
    length = set(values)
    return len(length)

def count_p_c(values, v) :
    v_full_name = v.rsplit('.', 1)[0]

    full_names = set([])
    p_count = set([])
    c_count = set([])

    c_full_name_to_test = {}
    p_full_name_to_test = {}
    for p,c in values:
        p_split = p.rsplit('.', 1)
        p_full_name = p_split[0]
        p_test_name = p_split[1]
        full_names.add(p_full_name)
        add_to_dict(p_full_name_to_test, p_full_name, p_test_name)
        p_count.add(p)

        if c != "\n" and c != "":
            c_split = c.rsplit('.', 1)
            c_full_name = c_split[0]
            c_test_name = c_split[1]
            full_names.add(c_full_name)
            add_to_dict(c_full_name_to_test, c_full_name, c_test_name)
            c_count.add(c)

    result = []
    for full_name in full_names:
        if full_name != v_full_name:
            result.append((uniq_len(p_full_name_to_test.get(full_name,[])), uniq_len(c_full_name_to_test.get(full_name,[]))))

    return ( v, (uniq_len(p_full_name_to_test.get(v_full_name,[])), uniq_len(c_full_name_to_test.get(v_full_name,[]))) , result, p_count, c_count)

# Assumes all c's cleans all p's, which may not be true
def read_file(filepath):
    dict = {}
    with open(filepath) as fp:
        for cnt, line in enumerate(fp):
            v, p, c = split_line(line)
            add_to_dict(dict, v, (p,c))

    list_v_tc = {}
    for key in dict:
        add_to_dict(list_v_tc, key, count_p_c(dict[key], key))

    return list_v_tc

def count_num_p_c(test_classes):
    p_count = set([])
    c_count = set([])
    for p, c in test_classes:
        p_count.add(p)
        c_count.add(c)

    return p_count, c_count

def calculate(filepath):
    result = []
    stats = read_file(filepath)
    for v, values in stats.items():
        #(v2, t_v, test_classes, p_count, c_count) = stats.get(v)[0]
        v2, t_v, test_classes, p_count, c_count = values[0]
        prob = p_fail_class(t_v, test_classes)
#        test_classes.append(t_v)
#        p_count, c_count = count_num_p_c(test_classes)
        result.append((v, prob, uniq_len(p_count), uniq_len(c_count)))
    return result

def print_stats(filepath):
    result = calculate(filepath)
    for v, prob, p_count, c_count in result:
        print str.format("{},{},{},{}",v, p_count, c_count, prob)

def max_index(test_order, tests):
    i = -1
    for test in tests:
        if test in test_order:
            j = test_order.index(test)

            if j > i:
                i = j
    return i
        
def should_od_fail(test_order, v, ps, cs):
    # expects test_order to contain v
    trunc_test_order = test_order[:test_order.index(v)]

    max_p = max_index(trunc_test_order, ps)
    max_c = max_index(trunc_test_order, cs)

    if (max_p < max_c or max_p == -1) :
        return "PASS"
    else:
        return "FAIL"

def check_od_fails(dep_info_file, dirpath, module_file):
    stats = read_file(dep_info_file)

    dict = {}
    with open(module_file) as fp:
        for cnt, line in enumerate(fp):
            v, module = line.split(",")
            add_to_dict(dict, v, module)


    for v, module in dict.items():
        if v in stats.keys():
            (v2, t_v, test_classes, p_count, c_count) = stats.get(v)[0]

            test_runs_path = os.path.join(dirpath, module[0].rstrip(), "test-runs", "results")
            for file in os.listdir(test_runs_path):
                with open(os.path.join(test_runs_path,file)) as f:
                    data = json.load(f)

                if v in data['results'].keys():
                    v_result = data['results'].get(v)['result']
                    expected_result = should_od_fail(data['testOrder'], v, p_count, c_count)

                    if ((expected_result == "PASS" or v_result == "PASS") and expected_result != v_result):
                        status = "not_match"
                    else:
                        status = "match"

                    print str.format("{},{},{},{},{}", v, status, v_result, expected_result, file)
                else:
                    print str.format("{},no_victim,,,{}",v,file)
                
        else:
            print str.format("{},missing_dep_info,,,", v)

def get_check_od_fails_summary(filepath):
    dict = {}
    with open(filepath) as fp:
        for cnt, line in enumerate(fp):
            v, status, observed_result, expected_result, file = line.split(",")
            if (v == "test_name"):
                continue
            add_to_dict(dict, v, (status, observed_result, expected_result, file))

    # v, num_runs_with_v, num_runs, num_match, num_not_match
    for key, values in dict.items():
        count_missing_dep_info = 0
        count_no_victim = 0
        count_match = 0
        count_not_match = 0

        for value in values:
            (status, observed_result, expected_result, file) = value

            if (status == "missing_dep_info"):
                count_missing_dep_info += 1
            elif (status == "no_victim"):
                count_no_victim += 1
            elif (status == "match"):
                count_match += 1
            elif (status == "not_match"):
                count_not_match += 1

        print str.format("{},{},{},{},{},{}",key,count_missing_dep_info, len(values) - count_no_victim, len(values), count_match, count_not_match)
        
        
