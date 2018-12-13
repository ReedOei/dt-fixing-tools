package edu.illinois.cs.dt.tools.diagnosis.rewrite;

import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestRunResult;

public class RewritingResult {
    private final RewriteTargetContainer targets;
    private final TestRunResult testRunResult;
    private final Result result;
    private final Result expected;

    public RewritingResult(final RewriteTargetContainer targets, final TestRunResult testRunResult, final Result result, final Result expected) {
        this.targets = targets;
        this.testRunResult = testRunResult;
        this.result = result;
        this.expected = expected;
    }

    public RewriteTargetContainer target() {
        return targets;
    }

    public TestRunResult testRunResult() {
        return testRunResult;
    }

    public Result result() {
        return result;
    }

    public Result expected() {
        return expected;
    }
}
