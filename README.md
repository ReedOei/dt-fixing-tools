# DT Fixing Tools

Tools for automatically debugging dependent tests.
Currently very much a work in progress.

## Properties

The following properties can currently be specified via `dtfixingtools.properties`:

- `dt.verify` (`boolean`, default `false`): Whether to verify dependent tests detection results by rerunning the order several times.
- `dt.verify.rounds` (`int`, default `1`): How many times to rerun orders to verify results.
- `dt.randomize.rounds` (`int`, default `10`): How many random orders to run when looking for dependent tests/flaky tests.
- `runner.timeout.default` (`double`, default `10800`): How many seconds to wait for before timing out when running tests if there isn't enough prior information to guess (default is 6 hours).
- `runner.timeout.multiplier` (`double`, defualt `4`): How much longer than expected to wait before timing out (e.g., if it expects the tests to take 10 seconds, it will wait 4 * 10 = 40 seconds).
- `runner.timeout.offset` (`double`, default `5`): Minimum number of seconds to wait for any order (i.e., will never timeout before 5 seconds).
- `runner.timeout.pertest` (`double`, default `2`): A flat number of seconds to wait per test (e.g., if you have 20 tests, it will add 40 seconds to the timeout time).
- `runner.throw_on_flaky` (`boolean`, defualt `true`): Whether to throw a `FlakyTestException` if a flaky test is detected.

