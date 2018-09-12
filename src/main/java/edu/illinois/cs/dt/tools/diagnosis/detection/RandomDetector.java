package edu.illinois.cs.dt.tools.diagnosis.detection;

import com.reedoei.eunomia.collections.RandomList;
import com.reedoei.eunomia.collections.StreamUtil;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.SmartRunner;
import edu.illinois.cs.dt.tools.diagnosis.detection.filters.FlakyFilter;
import edu.illinois.cs.dt.tools.diagnosis.detection.filters.UniqueFilter;
import edu.illinois.cs.dt.tools.diagnosis.detection.filters.VerifyFilter;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;

import java.util.List;

public class RandomDetector extends ExecutingDetector {
    private final List<String> tests;
    private final TestRunResult origResult;

    public RandomDetector(final Runner runner, final int rounds, final List<String> tests) {
        super(runner, rounds);

        this.tests = tests;

        System.out.println("[INFO] Getting original results (" + tests.size() + " tests).");
        this.origResult = runner.runList(tests).get();

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
        addFilter(new VerifyFilter(runner));
        addFilter(new UniqueFilter());
    }

    @Override
    public List<DependentTest> results() throws Exception {
        final RandomList<String> order = new RandomList<>(tests).shuffled();
        return makeDts(tests, origResult, order, runner.runList(order).get());
    }
}
