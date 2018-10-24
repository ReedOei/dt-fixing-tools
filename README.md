# DT Fixing Tools

Tools for automatically debugging dependent tests.
Currently a work in progress.

## Quickstart

### Use Pre-Compiled Script

```bash
bash ./PATH/main.sh PROJECT_REPO_URL PROJECT_PATH PROJECT_MODULE_PATH
```

## Running the debugger

To run the debugger, you need to add the following plugin declaration to the Maven project containing the dependent tests you wish to debug.

```
<plugin>
  <groupId>com.reedoei</groupId>
  <artifactId>testrunner-maven-plugin</artifactId>
  <version>0.1-SNAPSHOT</version>
  <configuration>
    <className>edu.illinois.cs.dt.tools.diagnosis.DiagnoserPlugin</className>
  </configuration>
  <dependencies>
    <dependency>
      <groupId>edu.illinois.cs</groupId>
      <artifactId>dt-fixing-tools</artifactId>
      <version>1.0.0-SNAPSHOT</version>
    </dependency>
  </dependencies>
</plugin>
```

Then simply run:

```
mvn testrunner:testplugin -e
```

To run the debugger on a specific set of dependent tests, you can create a folder named `.dtfixingtools/detection-results` inside the relevant module and place the `dt-lists.json` found by running the detector in that directory.

## Properties

The following properties can currently be specified via `dtfixingtools.properties`:

- `dt.verify` (`boolean`, default `false`): Whether to verify dependent tests detection results by rerunning the order several times.
- `dt.verify.rounds` (`int`, default `1`): How many times to rerun orders to verify results.
- `dt.randomize.rounds` (`int`, default `10`): How many random orders to run when looking for dependent tests/flaky tests.

