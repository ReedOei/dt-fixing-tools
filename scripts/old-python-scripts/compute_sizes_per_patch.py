import fnmatch
import json
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

def same_class(test1, test2):
    return test1.split('.')[-2] == test2.split('.')[-2]

def outputline(fixerdirname, testname, polluter, cleaner):
    for patchfile in find_file('*.patch*', fixerdirname):
        cleanerline = grep('CLEANER: ', patchfile)
        if not cleaner in cleanerline:
            continue
        polluterline = grep('POLLUTER: ', patchfile)
        if not polluter in polluterline:
            continue
        cleanersize = grep('NEW CLEANER SIZE: ', patchfile).split(' ')[3].strip()
        originalsize = grep('ORIGINAL CLEANER SIZE: ', patchfile).split(' ')[3].strip()
        print testname + ',' + polluter + ',' + cleaner + ',' + cleanersize + ',' + originalsize
        return True
    return False

def main(args):
    jsonfile = args[1]

    testname = os.path.basename(jsonfile).split('-')[0]
    ispass = os.path.basename(jsonfile).split('-')[2] == 'PASS'

    # Go two directories up to get directory, then add fixer/ to the end
    fixerdirname = os.path.join(os.path.dirname(os.path.dirname(jsonfile)), 'fixer')

    data = {}
    with open(jsonfile) as f:
        data = json.load(f)

    polluters = data['polluters']
    for pol in polluters:
        # If brittle, then cleaner is each "polluter", otherwise try each cleaner
        if ispass:
            outputline(fixerdirname, testname, 'N/A', pol['deps'][0])
        else:
            found = False
            for c in pol['cleanerData']['cleaners']:
                # Search through patches in the fixer/ directory, see if they have the polluter and cleaner
                if outputline(fixerdirname, testname, pol['deps'][0], c['cleanerTests'][0]):
                    found = True
                    continue
            if len(pol['cleanerData']['cleaners']) > 0 and not found:
                print 'AWSHI2', testname, pol['deps'][0]

if __name__ == '__main__':
    main(sys.argv)
