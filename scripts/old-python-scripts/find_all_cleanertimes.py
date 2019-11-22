import json
import os
import sys

def main(args):
    jsonfile = args[1]

    with open(jsonfile) as f:
        data = json.load(f)

    totaltime = 0
    for pol in data['polluters']:
        polluter = '|'.join(pol['deps'])
        cleanerData = pol['cleanerData']
        if len(cleanerData['cleaners']) > 0:
            totaltime += cleanerData['time']['totalTime']['elapsedSeconds']

    # If the total time found so far is 0, then there must be no polluters, so grab overall time minus the polluter times
    if totaltime == 0:
        poltime = 0
        for pol in data['polluters']:
            poltime += pol['time']['elapsedSeconds']
        totaltime = max(data['time']['elapsedSeconds'] - poltime, 0)

    print totaltime

if __name__ == '__main__':
    main(sys.argv)
