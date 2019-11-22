import json
import os
import sys

def main(args):
    jsonfile = args[1]

    with open(jsonfile) as f:
        data = json.load(f)

    dts = data['dts']
    entry = dts[0]
    name = entry['name']
    revealed = entry['revealed']
    order = revealed['order']
    print name, float(order.index(name)) / len(order)

if __name__ == '__main__':
    main(sys.argv)
