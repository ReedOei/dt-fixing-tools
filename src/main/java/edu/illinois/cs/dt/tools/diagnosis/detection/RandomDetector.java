package edu.illinois.cs.dt.tools.diagnosis.detection;

import com.reedoei.eunomia.collections.RandomList;
import com.reedoei.eunomia.collections.StreamUtil;
import edu.illinois.cs.dt.tools.diagnosis.detection.filters.FlakyFilter;
import edu.illinois.cs.dt.tools.diagnosis.detection.filters.UniqueFilter;
import edu.illinois.cs.dt.tools.diagnosis.detection.filters.VerifyFilter;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.washington.cs.dt.TestExecResult;
import edu.washington.cs.dt.main.Main;

import java.util.List;

public class RandomDetector extends ExecutingDetector {
    private final List<String> tests;
    private final TestExecResult origResult;

    public RandomDetector(final String classpath, final int rounds, final List<String> tests) throws Exception {
        super(classpath, rounds);

        this.tests = tests;

        Main.removeredundancy = false;

        System.out.println("[INFO] Getting original results (" + tests.size() + " tests).");
        this.origResult = runner.runOrder(tests).result();

        System.out.println("[INFO] Detecting flaky tests.");
        StreamUtil.seq(new FlakyDetector(classpath, rounds, tests, runner, origResult).detect());
        System.out.println();

        addFilter(new FlakyFilter(runner));
        addFilter(new UniqueFilter());
        addFilter(new VerifyFilter(classpath));
        addFilter(new UniqueFilter());
    }

    @Override
    public SmartTestRunner makeRunner(final String classpath) {
        return new SmartTestRunner(classpath, false);
    }

    @Override
    public List<DependentTest> results() throws Exception {
        final RandomList<String> order = new RandomList<>(tests).shuffled();
        final TestExecResult result = runner.runOrder(order).result();
        return makeDts(tests, origResult, order, result);
    }
}
