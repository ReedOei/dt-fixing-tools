package edu.illinois.cs.dt.tools.diagnosis.detection;

import edu.illinois.cs.dt.tools.runner.SmartTestRunner;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.illinois.cs.dt.tools.runner.data.TestResult;
import edu.washington.cs.dt.TestExecResult;
import edu.washington.cs.dt.TestExecResultsDifferentior;
import edu.washington.cs.dt.main.Main;

import java.util.List;

public class FlakyDetector extends UniqueDetector {
    private final List<String> tests;
    private final TestExecResult origResult;

    public FlakyDetector(final String classpath, final int rounds, final List<String> tests,
                         final SmartTestRunner runner, final TestExecResult origResult) {
        super(classpath, rounds);

        this.tests = tests;
        this.runner = runner;
        this.origResult = origResult;

        Main.removeredundancy = false;
    }

    @Override
    public SmartTestRunner makeRunner(final String classpath) {
        return runner;
    }

    @Override
    public List<DependentTest> run() throws Exception {
        final TestResult run = runner.runOrder(tests);

        return makeDts(new TestExecResultsDifferentior(origResult, run.results()).diffResults());
    }
}
