import json
import os
import sys

def main(args):
    jsonfile = args[1]

    with open(jsonfile) as f:
        data = json.load(f)

    for pol in data['polluters']:
        polluter = '|'.join(pol['deps'])
        cleanerData = pol['cleanerData']
        dependentTest = cleanerData['dependentTest']
        for cleaner in cleanerData['cleaners']:
            if cleaner['orderFound'] == 0:
                print cleaner['time']['totalTime']['elapsedSeconds']

if __name__ == '__main__':
    main(sys.argv)
