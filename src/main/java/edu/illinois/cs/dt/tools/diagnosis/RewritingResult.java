package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestRunResult;

public class RewritingResult {
    private final String fieldName;
    private final DiffContainer.Diff diff;
    private final TestRunResult testRunResult;
    private final Result result;
    private final Result expected;

    public RewritingResult(final String fieldName, final DiffContainer.Diff diff,
                           final TestRunResult testRunResult, final Result result,
                           final Result expected) {
        this.fieldName = fieldName;
        this.diff = diff;
        this.testRunResult = testRunResult;
        this.result = result;
        this.expected = expected;
    }

    public String fieldName() {
        return fieldName;
    }

    public DiffContainer.Diff diff() {
        return diff;
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
