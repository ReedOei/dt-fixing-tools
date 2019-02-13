import json
import os
import sys

def main(args):
    jsonfile = args[1]

    with open(jsonfile) as f:
        data = json.load(f)

    if 'INLINE' in data['status']:
        print data['time']['elapsedSeconds']
    else:
        print 0

if __name__ == '__main__':
    main(sys.argv)
