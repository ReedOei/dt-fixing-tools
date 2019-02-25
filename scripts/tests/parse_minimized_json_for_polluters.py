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
    dependentTest = s['dependentTest']
    for pol in polluters:
        polluter = '|'.join(pol['deps'])
        print dependentTest + ',' + polluter

if __name__ == '__main__':
    main(sys.argv)
