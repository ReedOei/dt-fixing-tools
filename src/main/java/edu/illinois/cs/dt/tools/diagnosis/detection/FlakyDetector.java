package edu.illinois.cs.dt.tools.diagnosis.detection;

import com.reedoei.eunomia.collections.RandomList;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.illinois.cs.dt.tools.runner.data.TestResult;
import edu.washington.cs.dt.TestExecResultsDifferentior;
import edu.washington.cs.dt.main.Main;

import java.util.List;

public class FlakyDetector extends UniqueDetector {
    private final List<String> tests;

    public FlakyDetector(final String classpath, final int rounds, final List<String> tests) {
        this(classpath, rounds, tests, new SmartTestRunner(classpath, false));
    }

    public FlakyDetector(final String classpath, final int rounds, final List<String> tests, final SmartTestRunner runner) {
        super(classpath, rounds);

        this.tests = tests;
        this.runner = runner;

        Main.removeredundancy = false;
    }

    @Override
    public SmartTestRunner makeRunner(final String classpath) {
        return runner;
    }

    @Override
    public List<DependentTest> run() throws Exception {
        // Run shuffled orders. The SmartTestRunner will automatically catch flaky dts.
        final List<String> shuffled = new RandomList<>(tests).shuffled();
        final TestResult firstRun = runner.runOrder(shuffled);
        final TestResult secondRun = runner.runOrder(shuffled);

        return makeDts(new TestExecResultsDifferentior(firstRun.result(), secondRun.results()).diffResults());
    }
}
