import json
import os
import sys

def main(args):
    fixerfile = args[1]
    jsonfile = args[2]

    # Count how many for each polluter when desperately looking
    polluter2singlecleaners = {}
    with open(fixerfile) as f:
        for line in f:
            if 'POLLUTER: ' in line:
                polluter = line.split('[')[2].split(']')[0]
                polluter2singlecleaners[polluter] = 0
                continue
            if 'DESPERATELY TRYING CLEANERS INDIVIDUALLY: ' in line:
                polluter2singlecleaners[polluter] += 1
                continue

    # Count how many total cleaners per polluter by parsing the final json
    polluter2allcleaners = {}
    with open(jsonfile) as f:
        data = json.load(f)
        for pol in data['polluters']:
            polluter = ', '.join(pol['deps'])
            polluter2allcleaners[polluter] = len(pol['cleanerData']['cleaners'])

    for polluter in polluter2allcleaners:
        if polluter2allcleaners[polluter] > 0:
            print polluter, '{0:.2f}'.format(float(polluter2singlecleaners[polluter] * 100) / polluter2allcleaners[polluter])

if __name__ == '__main__':
    main(sys.argv)
