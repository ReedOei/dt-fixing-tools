#!/usr/bin/env bash

# Usage: bash zip-results.sh <OUTPUT DIR 1> <OUTPUT DIR 2> ...
# OUTPUT DIR is the directory called *_output that should be generated for each project (should contain a diagnosis.log)

sha=$(git log -1 --format="%H")
time=$(date +%Y-%m-%d-%H-%M-%S)
fname="$time-$sha-results"
zipname="$fname.zip"

echo "Copying files to $fname"

mkdir -p "$fname"
for f in $@; do
    echo "Copying $f"
    cp -r "$f" "$fname"
done

echo "Zipping files to $zipname"
zip -q -r "$zipname" "$fname"

rm -rf "$fname"

