# Usage: python -c'import get_od_fail_info; get_od_fail_info.main("test.csv")'
# python -c'import get_od_fail_info; print(get_od_fail_info.calculate("all_polluter_cleaner_info.csv"))'

import math

def multiplyList(myList) : 
    # Multiply elements one by one 
    result = 1
    for x in myList: 
         result = result * x  
    return result  

def factorial(n):
    return math.factorial(n) * 1.0

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

def count_p_c(values, v) :
    v_full_name = v.rsplit('.', 1)[0]

    full_names = set([])
    c_full_name_to_test = {}
    p_full_name_to_test = {}
    for p,c in values:
        p_split = p.rsplit('.', 1)
        p_full_name = p_split[0]
        p_test_name = p_split[1]
        full_names.add(p_full_name)
        add_to_dict(p_full_name_to_test, p_full_name, p_test_name)

        if c != "\n" and c != "":
            c_split = c.rsplit('.', 1)
            c_full_name = c_split[0]
            c_test_name = c_split[1]
            full_names.add(c_full_name)
            add_to_dict(c_full_name_to_test, c_full_name, c_test_name)

    result = []
    for full_name in full_names:
        if full_name != v_full_name:
            result.append((len(p_full_name_to_test.get(full_name,[])), len(c_full_name_to_test.get(full_name,[]))))

    return ( v, (len(p_full_name_to_test.get(v_full_name,[])), len(c_full_name_to_test.get(v_full_name,[]))) , result)

# Assumes all c's cleans all p's, which may not be true
def read_file(filepath):
    dict = {}
    with open(filepath) as fp:
        for cnt, line in enumerate(fp):
            v, p, c = split_line(line)
            add_to_dict(dict, v, (p,c))

    list_v_tc = []
    for key in dict:
        list_v_tc.append(count_p_c(dict[key], key))

    return list_v_tc

def calculate(filepath):
    result = []
    stats = read_file(filepath)
    for v, t_v, test_classes in stats:
        prob = p_fail_class(t_v, test_classes)
        result.append((v, prob))
    return result

def main(filepath):
    result = calculate(filepath)
    for v, prob in result:
        print str.format("{},{}",v,prob)
