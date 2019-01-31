# Overview

The scripts in this directory are used to run the pipeline and subsequently generate the database.
The main script to run the pipeline is `create_and_run_dockers.sh`, and can be run as follows:

```bash
bash create_and_run_dockers.sh <path to csv> <round num> <timeout (seconds)>
```

where `<path to csv>` is a csv file containing lines of `github url,sha`.

To generate the database, you can run:

```bash
bash build-database.sh <output dir> <database path> <path to subject csv>
```

To generate the commands given an already built database, run:
```bash
bash generate-commands.sh <database>
```

To generate the tables given an already built database, run:
```bash
bash generate-tables.sh <database>
```

The `<path to subject csv>` should be the path to the `csv` file used above.
The `<output dir>` is the directory containing all the output from every subject.
The `<database path>` is the path to write out the database to after it has been built.

# Database

The database may be used to more easily explore the dataset.
All the information found in the database comes from the raw files described below, but some information from the raw files is omitted in the database (i.e., the full output log of each test run, and the stack traces from failing tests), as it is not generally useful for querying, and greatly increases the size of the database.

## Database Format

The following database contains the following tables and views.
The full schema may be viewed in the companion file which should be located in this directory called `create_table.sql`.

The following tables and views contain information about the subjects in our experiments:
- `subject_raw`: Contains each subjects GitHub slug, url, SHA used in our experiment, and LOC, both total and just for tests.
- `subject`: Subjects for which we have results, containing the name (base project name + hyphenated module path) and slug. The subject *name* is a unique identifier for each module.
- `subject_info`: The name and number of test methods for each subject.
- `module_test_time`: Contains how long each module's tests took to run (not including time from Maven).
- `original_order`: The order that Surefire ran the tests in. Has a column to indicate order, with each record representing a single test, as well as the name of the class name and package containing the test method.
- `num_rounds`: The number of rounds of each type ran for each subject.
- `subject_with_od`: A unique list of subjects (modules) that have at least one dependent tests.

The following tables contain data from the detector that has been gathered and processed to be easier to query. These tables will be the most frequently used.
- `flaky_test_classification`: The full list of flaky tests found, including which subject they come from and what type of flaky test they are.
- `flaky_test_failures`: The number of failures for each flaky test for each detector. Also has the number of rounds that test ran in for that detector.
- `flaky_test_counts`: The number of flaky tests of each kind for each subject.
- `detection_round_failures`: The number of failures for each flaky test type.
- `confirmation_effectiveness`: How often each confirmation round type confirmed the flaky test as the flaky type we found at the end of the experiment. For example, if a test was found to be order dependent (that is, to pass in the passing order and fail in the failing order) in 1 run, but was found to be non-order-dependent in 2 other runs, it would be recorded as being confirmed twice, out of a total of 3 runs (because it was found to be non-order-dependent).
- `confirmation_by_test`: The number of times each test both passed in the passing order and failed in the failing order over all confirmation steps.
- `confirmation_runs`: The results of confirming each test. Contains the test name, as well the expected and revealed results for both the passing and failing order.

The followng tables contain miscellaneous information:
- `operation_time`: Represents the amount of time that a computation takes. Has start time/end time (in milliseconds by `System.currentTimeMillis()`), and elapsed time (in seconds)

The following tables and views contain data related to the minimization of dependencies:
- `od_classification`: The master table containing the classification of dependent tests. The tests are this list are dependent tests (as far we can tell), so use this whenever you want to refer to **all** dependent tests to protect against duplicates/non-order dependent tests having results.
- `all_no_test`: A list of all tests found to be non-order dependent.
- `polluter_data`: Each minimized dependency has polluter data. **This also refers to setters**. Exists to allow matching minimize test results to dependencies. Represents a single dependency group.
- `dependency`: The actual test names in the polluter or setter group, with the order indicated by `order_index`.
- `minimize_test_result`: Represents a minimization of an entire test order for a specific dependent test (`test_name`). Includes the time, the expected run's id, and the subject.
- `cleaner_data`: Represents a collection of all cleaner groups for a given dependency group. Contains the amount of time that **minimization** took, but **NOT** the whole time to find cleaners.
- `cleaner_group`: A single cleaner group for a given dependency group/dependent test.
- `cleaner_test`: The actual method names in the cleaner group, with the order given by the `order_index`.
- `minimized_tests`: A unique list of subject names/test names that were minimized.
- `polluter_data_count`: The number of dependencies found that cause the test to pass, and cause the test to fail. Exactly one of these numbers should be greater than 0, anything else most likely indicates a non-order dependent test.
- `dependency_info`: Information about dependencies, how many dependent tests per dependency group for a given test/subject/test order that was minimized.
- `cleaner_info`: Information about cleaners, how many cleaner tests per cleaner group for a given test/subject/test order that was minimized.
- `tests_with_cleaner`: The tests that have a cleaner, and the maximum number of tests in a cleaner group.
- `tests_with_setter`: The tests that have a setter, and the maximum number of tests in a setter group.
- `tests_with_polluter`: The tests that have a polluter, and the maximum number of tests in a polluter group.
- `dependency_groups`: The dependency groups, by subject/test name, containing each test combined into a single field, separated by commas, as well as the number of dependencies.
- `cleaner_Groups`: Same as above (`dependency_groups`), but for cleaners.

The following tables and views contain data related to debugging/automatically fixing dependent tests:
- `static_field_info`: Information about which test/test order/mode the static field info is for.
- `static_field_info_field`: The field the information is about.
- `polluted_field`: A field whose value was different between the two runs: the field/test name and the values with and without the dependencies.
- `rewrite_target`: A field that was attempted to be rewritten, as well as the id of the result.
- `rewrite_result`: The result of rewriting a field (`actual_result` is the result after rewriting, `expected_result` is the result without rewriting).
- `polluter_diagnosis`: Represents the diagnosis of a single polluter group.
- `diagnosis_result`: The diagnosis for a dependent test in some order (every polluter/setter group).
- `field_diff`: A part of some field whose xml representation differents with/without dependencies: includes the xpath to the field and the differing values at that location.
- `test_patch`: Information about a patch generated, including whether it was successful. **NOTE**: There may be more than one patch per test, not all of which succeeded.
- `fixed_tests`: A complete list of tests that had at least one successful patch. Ues this when you want to refer to fixed tests, don't generate one yourself. If you want **not** fixed tests, left join this table and filter for null.
- `fixable_tests`: The tests that *could* be fixed (not necessarily *have been* fixed). That is, all tests with a cleaner or a setter.
- `diagnosis_info`: A summary of the diagnosis for a given subject/test/polluter, containing the number of fields found to change the result.
- `diagnosed_tests`: Unique list of test names number of tests where we found more than one field.

The following tables and views contain mostly unprocessed information about the flaky tests found and the detection rounds themselves, and generally don't need to be used.
- `flaky_test`: Each row represents a failure in some detection round. Contains identifiers for the intended (passing) and revealed (failing) test runs, as well as the test name.
- `flaky_test_list`: Used to match records from `flaky_test` to the detection round in which they occur.
- `detection_round`: Represents a single round for some detector running on some module (e.g., running the random class+method detector on http-request-lib). Contains the subject name, round type (i.e., detector type), round time (in seconds), and round number (the first round is round 0, not round 1). Also contains the ids of the flaky ttest lists corresponding to the *unfiltered* and *filtered* lists of flaky tests found by this detection round.
- `detection_round_tests_runs`: This tables exists to allow matching detection rounds to test run ids (may be useful to manually examine test outputs).
- `verify_round`: The result of performing the confirmation step for a single result (e.g., confirming that a test passes in the original order). Contains the subject name, corresponding detection round number, test run id, test name, and the expected and revealed results. The expected result is what was historically observed, and the revealed result is what actually happened in this confirmation step. Both confirmation and reconfirmation are present in this table, and may be differentiated by the round type.
- `unfiltered_flaky_tests`: The list of unfiltered flaky tests, for each subject/round/detector type.
- `filtered_flaky_tests`: The list of filtered flaky tests, for each subject/round/detector type.

## Raw Data Format

The raw data from the detector is in two categories:
- Log files, from running the various tools
- Data files, containing the actual results of running each tools

### Log Files

The log files for each project are:
- `mvn-test.log`: This is the full stdout and stderr of running the `mvn test` command on the project.
- `mvn-test-time.log`: This is the total time spent running `mvn test`, in the following format:
```
real 10.59
user 14.80
sys 0.96
```
- `original.log`: Output from running the original order detector, which runs the original order many times.
- `random_class.log`: Output from running the random class detector, which randomly orders test classes, but **not** test methods *within* test classes, and runs the tests many times.
- `random_class_method.log`: Output from running the random class+method detector, which randomly orders both test classes and test methods, and runs the tests many times.
- `reverse_class.log`: Output from running the test classes in reverse order, but keeping the test methods in the same order as the original order.
- `reverse_original.log`: Output from reversing the order of both test classes and test methods in the original order and running this reversed order.
- `module_test_time.log`: This log shows the total time spent actually running tests for a given module, not counting time spent running Maven. This is currently unused by any tool.

### Data Files

Each module's output folder (inside the corresponding `*_output` folder for the entire project) will have some of the following files (paths relative to the module output folder).
Missing files are caused by errors in the module, modules timing out, or incompatibilities with our tools.

- `original-order`: The original order, as run by surefire. One test method per line.
- `error`: The error that caused the run to stop, if any. Generally a stack trace + an exception message.
- `minimized/<TEST_NAME>-<ORDER_HASH>-<TEST_RESULT>-dependencies.json`: Each file represents the minimization of a single test order for a single tests, containing dependnecies as well as cleaners.
- `fixer/<TEST_NAME>.patch[.<N>]`: There is one file for each polluter or setter tried, representing the results of trying to fix this polluter/setter.
- `diagnosis/<TEST_NAME>-<ORDER_HASH>-<TEST_RESULT>.json`: Each field represents trying to diagnose (find fields that, when reset, cause the test to pass/fail) a single order for a single test.
- `static-field-info-<TYPE>/<TEST_NAME>-<ORDER_HASH>`: The fields accessed by a test when run with the static field tracer in some mode.
- `pollution-diffs/<FIELD_NAME>-<ORDER_HASH>`: For each field found to be polluter for the victim/brittle test in some order, the in-depth diff (that is, deeper than just the static root) for that field.
- `pollution-data/`: The raw pollution data, that is, values for each field with and without dependncies, and a summary for each test showing the diff:
    - `<TEST_NAME>-<ORDER_HASH>-<TEST_RESULT>-with-deps`: The values of the fields for the depnednet test when run **with** dependencies.
    - `<TEST_NAME>-<ORDER_HASH>-<TEST_RESULT>-without-deps`: The values of the fields for the depnednet test when run **with** dependencies.
    - `<TEST_NAME>-<TEST_RESULT>.xml`: The diffs in the values found above (inlcudes when a field appears in one file but is missing in the other).
- `test-runs/output`: The actual output, to stdout and stderr, of each test run. The filename is a unique identifier that occurs in many places through the dataset.
- `test-runs/results`: The results of running the tests, including the order of tests, as well as the result, time, and stack trace (if applicable) for each test in the order.
- `detection-results/flaky-lists.json`: The list of dependent test methods as found by the detector run last in our experiment. Includes two orders+results for each tests, an intended (passing) order, and a revealed (failing) order. This is only a convenient summary, and may be generated from the `round<N>.json` files.
- `detection-results/list.txt`: The list of dependent tests, one full qualified name per line.
- `detection-results/<DETECTOR_TYPE>/round<N>.json`: The results from each detection round. Lists the test run(s) for each round, as well as the flaky tests found before and after filtering (filtering includes removing duplicates, and in the case of dependent tests, confirmation). Includes orders that should allow reproduction of results (but may not, as flaky tests may not *always* flake).
- `detection-results/<DETECTOR_TYPE>-verify/round<N>.json/<TEST_NAME>-<TEST_RESULT>-round<N>.json`: The results of the confirmation step for each suspected dependent test (failing for the first time) in the given round. There will be two files, one to confirm that the test passes, and another to confirm that the test fails. This is the full test run result (as described for `test-runs/results`) for the confirmation step.
- `detection-results/<DETECTOR_TYPE>-confirmation-sampling/round<N>.json/<TEST_NAME>-<TEST_RESULT>-round<N>.json`: The result of *reconfirming* a test. This will only occur for tests that have previously been successfully confirmed. This is in the same format as the original confirmation step file.

where `<DETECTOR_TYPE>` may be one of:

- `original`: The original order detector; runs the original order many times.
- `random`: From running orders which shuffle both the test class and the test method order (but never test methods between test classes).
- `random-class`: From running orders which shuffled **only** the test class order.
- `reverse`: From reversing both the test class and test method order.
- `reverse-class`: From reversing **only** the test class order.

The following exceptions to the above structure exist:
- `flaky` will never have a `*-verify` folder, because non-order-dependent tests can be confirmed by only seeing the result change once. By extension, they will also not have a `confirmation-sampling` folder.
- `reverse` and `reverse-class` will never have a `*-confirmation-sampling` folder because only one run is done.

