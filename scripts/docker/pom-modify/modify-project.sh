#!/bin/bash

echo "hi1"

if [ "$1" == "" ]; then
    echo "arg1 - the path to the project, where high-level pom.xml is"
    exit
fi
echo "hi2"

crnt=`pwd`
working_dir=`dirname $0`
project_path=$1
echo "hi3"

cd ${project_path}
project_path=`pwd`
cd - > /dev/null
echo "hi4"

cd ${working_dir}
echo "hi5"


function process_poms
{
echo "hi6"
    while read pom; do
echo "hi7"
        ./modify-pom.sh ${pom}
    done
}
echo "hi8"

#find ${project_path} -name pom.xml | process_poms
javac PomFile.java
echo "hi9"
find ${project_path} -name pom.xml | grep -v "src/" | java PomFile
echo "hi10"
rm -f PomFile.class
echo "hi11"

cd ${crnt}
