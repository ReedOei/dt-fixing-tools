package edu.illinois.cs.dt.tools.detection;

import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestResult;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.detection.filters.UniqueFilter;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.stream.Stream;

public class FlakyDetector extends ExecutingDetector {
    private static final int ORIGINAL_ORDER_TRIES = 3;

    private final List<String> tests;
    private TestRunResult origResult;

    public FlakyDetector(final Runner runner, final int rounds, final List<String> tests, final TestRunResult origResult) {
        super(runner, rounds, "flaky");

        this.tests = tests;
        this.origResult = origResult;

        addFilter(new UniqueFilter());
    }

    public FlakyDetector(final Runner runner, final int rounds, final List<String> tests) {
        super(runner, rounds, "flaky");

        this.tests = tests;

        System.out.println("[INFO] Getting original results (" + tests.size() + " tests).");

        // Try to run it three times, to see if we can get everything to pass (except for ignored tests)
        for (int i = 0; i < ORIGINAL_ORDER_TRIES; i++) {
            final TestRunResult origResult = runList(tests);
            final Stream<TestResult> values = origResult.results().values().stream();

            try {
                Files.write(DetectorPathManager.originalResultsLog(), (origResult.id() + "\n").getBytes(), StandardOpenOption.APPEND);
            } catch (IOException ignored) {}

            // Ignored tests will show up as SKIPPED, but that's fine because surefire would've skipped them too
            if (values.allMatch(tr -> tr.result().equals(Result.PASS) || tr.result().equals(Result.SKIPPED))) {
                this.origResult = origResult;
                break;
            }
        }

        if (this.origResult == null) {
            throw new NoPassingOrderException("No passing order for tests (" + ORIGINAL_ORDER_TRIES + " runs)");
        }

        addFilter(new UniqueFilter());
    }

    @Override
    public DetectionRound results() throws Exception {
        return makeDts(tests, origResult, tests, runList(tests));
    }
}
