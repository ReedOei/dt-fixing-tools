package edu.illinois.cs.dt.tools.diagnosis.detection;

import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;

import java.util.List;

public class FlakyDetector extends ExecutingDetector {
    private final List<String> tests;
    private final TestRunResult origResult;

    public FlakyDetector(final Runner runner, final int rounds, final List<String> tests, final TestRunResult origResult) {
        super(runner, rounds);

        this.tests = tests;
        this.origResult = origResult;
    }

    @Override
    public List<DependentTest> results() throws Exception {
        return makeDts(tests, origResult, tests, runSilent(tests));
    }
}
