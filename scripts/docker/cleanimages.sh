#!/bin/bash

if [[ $1 == "" ]]; then
    echo "arg1 - Path to CSV file with project,sha"
    exit
fi

projfile=$1

# Delete the base tooling
docker rmi toolingdetectorbase:latest

for line in $(cat ${projfile}); do
    # Create name of image for each project, then delete it
    slug=$(echo ${line} | cut -d',' -f1 | rev | cut -d'/' -f1-2 | rev)
    modifiedslug=$(echo ${slug} | sed 's;/;.;')
    image=detector-${modifiedslug}:latest
    docker rmi ${image}:latest
done
