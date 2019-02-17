import json
import os
import sys

def main(args):
    jsonfile = args[1]

    with open(jsonfile) as f:
        data = json.load(f)

    firstpol = data['polluters'][0]
    cleanerData = firstpol['cleanerData']

    # If there are cleaners, we want the time spent looking for that first cleaner of the first polluter
    if len(cleanerData['cleaners']) > 0:
        for cleaner in cleanerData['cleaners']:
            print cleaner['time']['totalTime']['elapsedSeconds']
    else:
        # If there is only one polluter, then it is the overall time, minus the time for the polluter
        if len(data['polluters']) == 1:
            print data['time']['elapsedSeconds'] - firstpol['time']['elapsedSeconds']
        else:
            sys.stderr.write('WEIRD CASE, NEED TO HANDLE ' + jsonfile + '\n')

if __name__ == '__main__':
    main(sys.argv)
