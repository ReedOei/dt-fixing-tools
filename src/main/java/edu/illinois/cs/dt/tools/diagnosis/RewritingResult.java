package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestRunResult;

public class RewritingResult {
    private final RewriteTarget target;
    private final TestRunResult testRunResult;
    private final Result result;
    private final Result expected;

    public RewritingResult(final RewriteTarget target, final TestRunResult testRunResult, final Result result, final Result expected) {

        this.target = target;
        this.testRunResult = testRunResult;
        this.result = result;
        this.expected = expected;
    }

    public RewriteTarget target() {
        return target;
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
