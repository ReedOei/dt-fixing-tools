package edu.illinois.cs.dt.tools.detection;

import com.reedoei.eunomia.collections.StreamUtil;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestResult;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.SmartRunner;
import edu.illinois.cs.dt.tools.detection.filters.ConfirmationFilter;
import edu.illinois.cs.dt.tools.detection.filters.UniqueFilter;

import java.util.List;
import java.util.stream.Stream;

public class RandomDetector extends ExecutingDetector {
    public static final double CONFIRMATION_SAMPLING_RATE =
            Configuration.config().getProperty("detector.random.confirmation_sampling_rate", 0.1);

    private final List<String> tests;
    private TestRunResult origResult;

    private final TestShuffler testShuffler;

    public RandomDetector(final String type, final Runner runner, final int rounds, final List<String> tests) {
        super(runner, rounds, type);

        this.tests = tests;

        this.testShuffler = new TestShuffler(type, rounds, tests);

        System.out.println("[INFO] Getting original results (" + tests.size() + " tests).");

        // Try to run it three times, to see if we can get everything to pass (except for ignored tests)
        for (int i = 0; i < 3; i++) {
            final TestRunResult origResult = runList(tests);
            final Stream<TestResult> values = origResult.results().values().stream();

            // Ignored tests will show up as SKIPPED, but that's fine because surefire would've skipped them too
            if (values.allMatch(tr -> tr.result().equals(Result.PASS) || tr.result().equals(Result.SKIPPED))) {
                this.origResult = origResult;
                break;
            }
        }

        if (this.origResult == null) {
            throw new RuntimeException("No passing order for tests");
        }

        System.out.println("[INFO] Detecting flaky tests.");
        StreamUtil.seq(new FlakyDetector(runner, rounds, tests, origResult).detect());
        System.out.println();

        final SmartRunner smartRunner;

        if (runner instanceof SmartRunner) {
            smartRunner = new SmartRunner(runner.project(), runner.framework(), ((SmartRunner) runner).info());
        } else {
            smartRunner = SmartRunner.withFramework(runner.project(), runner.framework());
        }

        // Filters to be applied in order
        addFilter(new ConfirmationFilter(name, tests, smartRunner));
        addFilter(new UniqueFilter());
    }

    @Override
    public DetectionRound results() throws Exception {
        final List<String> fullTestOrder = testShuffler.shuffledOrder(absoluteRound.get());

        return makeDts(tests, origResult, fullTestOrder, runList(fullTestOrder));
    }
}
