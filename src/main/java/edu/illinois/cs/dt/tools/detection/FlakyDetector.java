package edu.illinois.cs.dt.tools.detection;

import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.runner.Runner;

import java.util.List;

public class FlakyDetector extends ExecutingDetector {
    private final List<String> tests;
    private final TestRunResult origResult;

    public FlakyDetector(final Runner runner, final int rounds, final List<String> tests, final TestRunResult origResult) {
        super(runner, rounds, "flaky");

        this.tests = tests;
        this.origResult = origResult;
    }

    @Override
    public DetectionRound results() throws Exception {
        return makeDts(tests, origResult, tests, runList(tests));
    }
}
