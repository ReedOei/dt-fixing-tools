# Argument Parsing
echo "Positional Parameters"
echo 'Github Project Repository: ' $1
echo 'Github Project Repository Module Path: ' $2

if [ "$1" == "" ]; then
    echo "arg1 - github project repository"
    exit
elif [ "$2" == "" ]; then
    echo "arg2 - github project module path"
    exit
fi

# Setup Variables
crnt=`pwd`
working_dir=`dirname $0`
project_repository=$1
project_module_path=${crnt}/$2

# TestRunner
if [ ! -d "testrunner" ]; then
    git clone https://github.com/ReedOei/testrunner.git
fi
cd testrunner
mvn install
cd ..

# Dt-Fixing-Tools
if [ ! -d "dt-fixing-tools" ]; then
    git clone https://github.com/ReedOei/dt-fixing-tools.git
fi
cd dt-fixing-tools
mvn install
cd ..

# Project Repository
if [ ! -d "$project_module_path" ]; then
    git clone ${project_repository}.git
fi
cd ${project_module_path}
{ time -p mvn test -fn |& tee mvn-test.log ;} 2> mvn-test-time.log
cd ${crnt}

# Setup Necessary Scripts
if [ ! -f "modify-project.sh" ]; then
    wget https://raw.githubusercontent.com/ReedOei/dt-fixing-tools/master/scripts/docker/pom-modify/modify-project.sh
fi
if [ ! -f "PomFile.java" ]; then
    wget https://raw.githubusercontent.com/ReedOei/dt-fixing-tools/master/scripts/docker/pom-modify/PomFile.java
fi

# Run Scripts
bash ./modify-project.sh ${project_module_path}

# Run Plugin
cd ${project_module_path}
mvn testrunner:testplugin
