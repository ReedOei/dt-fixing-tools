package edu.illinois.cs.dt.tools.diagnosis.detection;

import edu.illinois.cs.dt.tools.diagnosis.detection.filters.UniqueFilter;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.washington.cs.dt.TestExecResult;
import edu.washington.cs.dt.main.Main;

import java.util.List;

public class FlakyDetector extends ExecutingDetector {
    private final List<String> tests;
    private final TestExecResult origResult;

    public FlakyDetector(final String classpath, final int rounds, final List<String> tests,
                         final SmartTestRunner runner, final TestExecResult origResult) {
        super(classpath, rounds);

        this.tests = tests;
        this.runner = runner;
        this.origResult = origResult;

        Main.removeredundancy = false;

        addFilter(new UniqueFilter());
    }

    @Override
    public SmartTestRunner makeRunner(final String classpath) {
        return runner;
    }

    @Override
    public List<DependentTest> results() throws Exception {
        return makeDts(tests, origResult, tests, runner.runOrder(tests).result());
    }
}
