import json
import os
import sys

def getcontents(patchfile):
    contents = []
    with open(patchfile) as f:
        for line in f:
            if line.startswith('MODIFIED'):
                contents.append(line.strip())
            if line.startswith('+'):
                contents.append(line.strip())
    return contents

def main(args):
    jsonfiles = args[1:]

    workingcount = 0
    canremovecount = 0
    setupteardown = 0
    allcontents = set()
    allcontents_canremove = set()
    allcontents_setupteardown = set()

    for jsonfile in jsonfiles:
        data = {}
        with open(jsonfile) as f:
            data = json.load(f)

        testname = os.path.basename(jsonfile).replace('.json', '')
        dirname = os.path.dirname(jsonfile)

        for res in data['patchResults']:
            # Count how many are working, and how many are good based on stripping out their cleaner statements
            status = res['status']
            if not 'patchLocation' in res:
                continue
            patchname = os.path.basename(res['patchLocation'])
            patchfile = os.path.join(dirname, patchname)

            if not status in ['NOD', 'NO_DEPS', 'UNSUPPORTED', 'MISSING_METHOD', 'NOT_FAILING', 'NO_CLEANER']:
                workingcount += 1

                # Look at the contents of the patch and count uniqueness
                patchname = os.path.basename(res['patchLocation'])
                patchfile = os.path.join(dirname, patchname)
                allcontents.add(tuple(getcontents(patchfile)))

            if 'INLINE' in status:
                if 'CANREMOVE' in status or 'SETUPTEARDOWN' in status:
                    canremovecount += 1
                    allcontents_canremove.add(tuple(getcontents(patchfile)))
                if 'SETUPTEARDOWN' in status:
                    setupteardown += 1
                    allcontents_setupteardown.add(tuple(getcontents(patchfile)))

    print workingcount, canremovecount, setupteardown, len(allcontents), len(allcontents_canremove), len(allcontents_setupteardown)

if __name__ == '__main__':
    main(sys.argv)
