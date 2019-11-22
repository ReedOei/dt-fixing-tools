import json
import os
import sys

def find(key, dictionary):
    if hasattr(dictionary, 'iteritems'):
        for k, v in dictionary.iteritems():
            if k == key:
                yield v
            elif isinstance(v, dict):
                for result in find(key, v):
                    yield result
            elif isinstance(v, list):
                for d in v:
                    for result in find(key, d):
                        yield result

def main(args):
    jsonfile = args[1]

    base = os.path.basename(jsonfile)
    filename = os.path.splitext(base)[0]
    testname = filename.split('-')[0]

    with open(jsonfile) as f:
        data = json.load(f)

    allpol = data['polluters']

    for pol in allpol:
        cleanertests = find('cleanerTests', pol)
        didprint = False
        for cleanertest in cleanertests:
            didprint = True
            print testname + ',' +  ('|'.join(pol['deps'])) + ',' + ('|'.join(cleanertest))
        if not didprint:
            print testname + ',' +  ('|'.join(pol['deps'])) + ','

if __name__ == '__main__':
    main(sys.argv)
