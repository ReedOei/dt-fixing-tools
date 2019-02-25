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
    testOrder = s['expectedRun']['testOrder']
    for pol in polluters:
        polluter = pol['deps'][0]  # Focus on first one
        dependentTestIndex = testOrder.index(dependentTest)
        polluterIndex = testOrder.index(polluter)
        # Distance between polluter and dependentTest, ratio over dependentTest distance from beginning
        distance = dependentTestIndex - polluterIndex
        distanceRatio = float(distance) / dependentTestIndex
        # If in same test class or not
        if '.'.join(dependentTest.split('.')[:-1]) == '.'.join(polluter.split('.')[:-1]):
            sameclass = True
        else:
            sameclass = False
        print dependentTest + ',' + polluter + ',' + str(distance) + ',' + str(distanceRatio) + ',' + str(sameclass)

if __name__ == '__main__':
    main(sys.argv)
