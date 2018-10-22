# DT Fixing Tools

Tools for automatically debugging dependent tests.
Currently a work in progress.

## Quickstart

### Use Pre-Compiled Script
```bash
bash ./PATH/main.sh PROJECT_REPO_URL PROJECT_MODULE_PATH
```

## Properties

The following properties can currently be specified via `dtfixingtools.properties`:

- `dt.verify` (`boolean`, default `false`): Whether to verify dependent tests detection results by rerunning the order several times.
- `dt.verify.rounds` (`int`, default `1`): How many times to rerun orders to verify results.
- `dt.randomize.rounds` (`int`, default `10`): How many random orders to run when looking for dependent tests/flaky tests.