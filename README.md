# DT Fixing Tools

Tools for automatically debugging dependent tests.
Currently a work in progress.

## Quickstart

### Use Pre-Compiled Script
```bash
bash ./run.sh PROJECT_REPO_URL PROJECT_MODULE_PATH
```

### Self-Compile Scripts
Clone the testrunner and dt-fixing-tools repository. Run mvn install in both
```bash
git clone https://github.com/ReedOei/testrunner.git
cd testrunner
mvn install
cd ..

git clone https://github.com/ReedOei/dt-fixing-tools.git
cd dt-fixing-tools
mvn install
cd ..
```

Clone the desired project directory and run the following script inside
```bash
git clone PROEJCT_REPO_URL
cd PROJECT_MODULE_PATH
{ time -p mvn test -fn |& tee mvn-test.log ;} 2> mvn-test-time.log
```

Download both Pomfile.java and modify-project.sh from [this link](https://github.com/ReedOei/dt-fixing-tools/tree/master/scripts/docker/pom-modify)
```bash
cd WORKING_DIRECTORY
wget https://raw.githubusercontent.com/ReedOei/dt-fixing-tools/master/scripts/docker/pom-modify/PomFile.java
wget https://raw.githubusercontent.com/ReedOei/dt-fixing-tools/master/scripts/docker/pom-modify/modify-project.sh
```

Run modify-project.sh with the following parameters
```bash
bash ./modify-project.sh PROJECT_MODULE_PATH
```

Change into the directory of the project module and run the following command
```bash
cd PROJECT_MODULE_PATH
mvn testrunner:testplugin
```

## Properties

The following properties can currently be specified via `dtfixingtools.properties`:

- `dt.verify` (`boolean`, default `false`): Whether to verify dependent tests detection results by rerunning the order several times.
- `dt.verify.rounds` (`int`, default `1`): How many times to rerun orders to verify results.
- `dt.randomize.rounds` (`int`, default `10`): How many random orders to run when looking for dependent tests/flaky tests.