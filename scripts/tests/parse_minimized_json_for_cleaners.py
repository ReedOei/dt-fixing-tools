import json
import os
import sys

def main(args):
    # Read a JSON file
    inputfile = args[1]
    with open(inputfile) as f:
        s = json.load(f)
    # Look through the fields for any cleaners
    isCleaner = False
    polluters = s['polluters']
    for pol in polluters:
        cleaners = pol['cleanerData']['cleaners']
        for c in cleaners:
            if len(c['cleanerTests']) > 0:
                isCleaner = True
    print isCleaner

if __name__ == '__main__':
    main(sys.argv)
