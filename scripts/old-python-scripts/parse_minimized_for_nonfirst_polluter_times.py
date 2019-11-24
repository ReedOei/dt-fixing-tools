import json
import os
import sys

def main(args):
    jsonfile = args[1]

    with open(jsonfile) as f:
        data = json.load(f)

    totaltime = 0
    for pol in data['polluters']:
        # Skip the first
        if pol['index'] == 0:
            continue
        totaltime += pol['time']['elapsedSeconds']
    print totaltime

if __name__ == '__main__':
    main(sys.argv)
