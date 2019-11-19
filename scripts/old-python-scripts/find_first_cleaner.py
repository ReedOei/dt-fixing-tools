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
            print str(cleaner['time']['addTime']['elapsedSeconds']) + ',SUCCESS'
    else:
        # If there is only one polluter, then it is the overall time, minus the time for the polluter
        if len(data['polluters']) == 1:
            print str(data['time']['elapsedSeconds'] - firstpol['time']['elapsedSeconds']) + ',FAIL'
        else:
            # Otherwise, need to grab time of second polluter and subtract the start of that with the end of the first
            secondpol = data['polluters'][1]
            print str((secondpol['time']['startTime'] - firstpol['time']['endTime']) / 1000.0) + ',FAIL'
            #sys.stderr.write('WEIRD CASE, NEED TO HANDLE ' + jsonfile + '\n')

if __name__ == '__main__':
    main(sys.argv)
