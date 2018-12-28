package edu.illinois.cs.dt.tools.fixer;

import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import scala.util.Try;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public class FailingTestDetector {
    private final InstrumentingSmartRunner runner;

    public FailingTestDetector(final InstrumentingSmartRunner runner) {
        this.runner = runner;
    }

    public Optional<Set<String>> notPassingTests(final List<String> tests) {
        final Set<String> notPassingTests = new HashSet<>();

        if (tests.isEmpty()) {
            return Optional.of(notPassingTests);
        }

        final Try<TestRunResult> testRunResultTry = runner.runList(tests);

        if (testRunResultTry.isSuccess()) {
            testRunResultTry.get().results().forEach((testName, res) -> {
                if (!res.result().equals(Result.PASS)) {
                    notPassingTests.add(testName);
                }
            });

            return Optional.of(notPassingTests);
        } else {
            return Optional.empty();
        }
    }
}
