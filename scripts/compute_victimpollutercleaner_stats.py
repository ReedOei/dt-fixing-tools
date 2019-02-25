import os
import sys

def main(args):
    inputfile = args[1]

    mapping = {}
    with open(inputfile) as f:
        for line in f:
            parts = line.strip().split(',')
            victim = parts[0]
            if not victim in mapping:
                mapping[victim] = {}
            polluter = parts[1]
            if not polluter in mapping[victim]:
                mapping[victim][polluter] = set()
            cleaner = parts[2]
            mapping[victim][polluter].add(cleaner)

    # Compute for each victim common cleaners among polluters divided by all cleaners among polluters
    for victim in mapping:
        #common_cleaners = reduce(lambda x, y: x & y, [mapping[victim][polluter] for polluter in mapping[victim].keys()])
        #all_cleaners = reduce(lambda x, y: x | y, [mapping[victim][polluter] for polluter in mapping[victim].keys()])
        cleanersets = [mapping[victim][polluter] for polluter in mapping[victim].keys()]
        common_cleaners = cleanersets[0]
        for c in cleanersets[1:]:
            common_cleaners = common_cleaners & c
            if not common_cleaners == c:
                print victim, polluter, c - common_cleaners
        all_cleaners = set()
        for c in cleanersets:
            all_cleaners = all_cleaners | c
        print victim, len(common_cleaners), len(all_cleaners), float(len(common_cleaners)) / len(all_cleaners)

if __name__ == '__main__':
    main(sys.argv)
