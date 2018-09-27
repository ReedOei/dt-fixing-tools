package edu.illinois.cs.dt.tools.detection;

import com.reedoei.eunomia.collections.StreamUtil;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.SmartRunner;
import edu.illinois.cs.dt.tools.detection.filters.FlakyFilter;
import edu.illinois.cs.dt.tools.detection.filters.UniqueFilter;
import edu.illinois.cs.dt.tools.detection.filters.VerifyFilter;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class RandomDetector extends ExecutingDetector {
    private final List<String> tests;
    private final TestRunResult origResult;

    private final Set<String> alreadySeenOrders = new HashSet<>();

    private final TestShuffler testShuffler;

    public RandomDetector(final Runner runner, final int rounds, final List<String> tests) {
        super(runner, rounds, "random");

        this.tests = tests;

        this.testShuffler = new TestShuffler(rounds, tests);

        System.out.println("[INFO] Getting original results (" + tests.size() + " tests).");
        this.origResult = runList(tests);

        System.out.println("[INFO] Detecting flaky tests.");
        StreamUtil.seq(new FlakyDetector(runner, rounds, tests, origResult).detect());
        System.out.println();

        final SmartRunner smartRunner;

        if (runner instanceof SmartRunner) {
            smartRunner = new SmartRunner(runner.project(), runner.framework(), ((SmartRunner) runner).info());
        } else {
            smartRunner = SmartRunner.withFramework(runner.project(), runner.framework());
        }

        addFilter(new FlakyFilter(smartRunner));
        addFilter(new UniqueFilter());
        addFilter(new VerifyFilter(name, runner));
        addFilter(new UniqueFilter());
    }

    @Override
    public List<DependentTest> results() throws Exception {
        if (hasMoreToRun()) {
            final List<String> fullTestOrder = testShuffler.shuffledOrder(alreadySeenOrders);

            return makeDts(tests, origResult, fullTestOrder, runList(fullTestOrder));
        } else {
            return new ArrayList<>();
        }
    }

    private boolean hasMoreToRun() {
        return alreadySeenOrders.size() < testShuffler.permutationCount();
    }
}
