import json
import os
import sys

def main(args):
    jsonfile = args[1]

    base = os.path.basename(jsonfile)
    filename = os.path.splitext(base)[0]
    testname = filename.split('-')[0]

    with open(jsonfile) as f:
        data = json.load(f)

    allpol = data['polluters']

    for pol in allpol:
        print testname + ',' +  ('|'.join(pol['deps']))
        
if __name__ == '__main__':
    main(sys.argv)
