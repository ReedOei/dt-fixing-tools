import fnmatch
import os
import re
import sys

def find_file(pattern, path):
    result = []
    for root, dirs, files in os.walk(path):
        for name in files:
            if fnmatch.fnmatch(name, pattern):
                result.append(os.path.join(root, name))
    return result

def grep(pattern, file):
    with open(file) as f:
        for line in f:
            if re.search(pattern, line):
                return line

def search_best(polluter2victims, victim2polluters):
    # Collect number across each polluter/victim, sort based on outgoing edges
    ordering = []
    for p in polluter2victims:
        ordering.append((len(polluter2victims[p]), 'P:' + p))
    for v in victim2polluters:
        ordering.append((len(victim2polluters[v]), 'V:' + v))

    ordering.sort(key=lambda x: x[0], reverse=True)
    if ordering:
        return ordering[0]
    else:
        return None

def main(args):
    inputfile = args[1]
    debuggingdir = args[2]

    polluter2victims = {}
    victim2polluters = {}
    pair2cleaners = {}

    with open(inputfile) as f:
        for line in f:
            victim = line.strip().split(',')[0]
            polluter = line.strip().split(',')[1]
            cleaner = line.strip().split(',')[2]
            if not polluter in polluter2victims:
                polluter2victims[polluter] = set()
            polluter2victims[polluter].add(victim)
            if not victim in victim2polluters:
                victim2polluters[victim] = set()
            victim2polluters[victim].add(polluter)
            if not (victim, polluter) in pair2cleaners:
                pair2cleaners[(victim, polluter)] = set()
            pair2cleaners[(victim, polluter)].add(cleaner)

    # Get best, update the mappings, keep getting best afterwards until all are considered
    res = search_best(polluter2victims, victim2polluters)
    while not res == None:
        i, e = res
        print i, e
        victims = set() # Take the victims, since we need to use it to search across patches
        pairs = set()   # Keep track of pairs and make sure all of them have same cleaner
        # Update and remove those that are already considered by this one
        if e.startswith('P'):
            polluter = e.split(':')[1]
            del polluter2victims[polluter]
            for v in victim2polluters:
                if polluter in victim2polluters[v]:
                    victim2polluters[v].remove(polluter)
                    pairs.add((v, polluter))
                    victims.add(v)
        else:
            victim = e.split(':')[1]
            victims.add(victim)
            del victim2polluters[victim]
            for p in polluter2victims:
                if victim in polluter2victims[p]:
                    polluter2victims[p].remove(victim)
                    pairs.add((victim, p))

        # Check that all pairs have at least one cleaner in common, otherwise indicate
        if i > 0:
            commoncleaners = reduce(lambda x, y: x & y, [pair2cleaners[p] for p in pairs])
            if len(commoncleaners) == 0:
                print 'NO GO'

        # Look for the size for the smallest patch that is in common then across the cleaners
        smallestsize = sys.maxint
        for v in victims:   # Search across victims, since patch names are based off of name of the victim
            allpatches = find_file(v + "*.patch*", os.path.join(debuggingdir))
            for c in commoncleaners:
                for p in allpatches:
                    if not grep('CLEANER: ' + c, p):
                        size = int(grep('NEW CLEANER SIZE: ', p).split(':')[1].strip())
                        if size < smallestsize:
                            smallestsize = size
        print 'SIZE:', smallestsize

        res = search_best(polluter2victims, victim2polluters)

if __name__ == '__main__':
    main(sys.argv)
