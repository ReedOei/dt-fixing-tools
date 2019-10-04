#!/bin/bash

# returns 0 if successful build
# returns 1 in case of build failure

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
  echo "arg1 - repo"
  echo "arg2 - sha list"
  exit
fi

repo=$1
sha=$2

[ -d /home/awshi2/${sha} ] && exit 0
[ -f /home/awshi2/mvn-install-${sha}.log ] && exit 1

git clone ${repo} /home/awshi2/${sha}
cd /home/awshi2/$sha
timeout 1h /home/awshi2/apache-maven/bin/mvn clean install -DskipTests -fn -B |& tee /home/awshi2/mvn-install-${sha}.log
slug=$(echo ${repo} | rev | cut -d'/' -f1-2 | rev)
modifiedslug=$(echo ${slug} | sed 's;/;.;' | tr '[:upper:]' '[:lower:]')
[ -d /Scratch/${modifiedslug}=${sha}_build_output ] || mkdir /Scratch/${modifiedslug}=${sha}_build_output
cp /home/awshi2/mvn-install-${sha}.log /Scratch/${modifiedslug}=${sha}_build_output/

[ $? -ne 0 ] && { rm -rf /home/awshi2/${sha}; exit 1; }


