#!/bin/sh

# Argument Parsing
if [[ "$1" == ""]] || [["$2" == ""]] ; then
    echo "arg1 - github project repository"
    echo "arg2 - github project module path"
    exit
fi

# Setup Variables
crnt=`pwd`
working_dir=`dirname $0`

cd ${working_dir}
cd ../..
working_dir=`pwd`
project_repository=$1
project_module_path=${working_dir}/$2

# Dt-Fixing-Tools
cd ${working_dir}
if [ ! -d "dt-fixing-tools" ]; then
	echo "Error. Please run the main.sh inside https://github.com/ReedOei/dt-fixing-tools.git"
	exit
fi
cd dt-fixing-tools
mvn install

# TestRunner
cd ${working_dir}
if [ ! -d "testrunner" ]; then
    git clone https://github.com/ReedOei/testrunner.git
fi
cd testrunner
mvn install

# Project Repository
cd ${working_dir}
if [ ! -d "$project_module_path" ]; then
    git clone ${project_repository}.git
fi
cd ${project_module_path}
{ time -p mvn test -fn |& tee mvn-test.log ;} 2> mvn-test-time.log

# Run Necessary Setup Scripts
cd ${working_dir}/dt-fixing-tools/scripts/docker/pom-modify
bash ./modify-project.sh ${project_module_path}

# Run Plugin
cd ${project_module_path}
mvn testrunner:testplugin

# Return To Original Directory
cd ${crnt}