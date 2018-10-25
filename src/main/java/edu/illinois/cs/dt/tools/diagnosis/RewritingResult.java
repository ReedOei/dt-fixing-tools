package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.diagnosis.pollution.PollutedField;

public class RewritingResult {
    private final String fieldName;
    private final PollutedField field;
    private final TestRunResult testRunResult;
    private final Result result;
    private final Result expected;

    public RewritingResult(final String fieldName, final PollutedField field,
                           final TestRunResult testRunResult, final Result result,
                           final Result expected) {
        this.fieldName = fieldName;
        this.field = field;
        this.testRunResult = testRunResult;
        this.result = result;
        this.expected = expected;
    }

    public String fieldName() {
        return fieldName;
    }

    public PollutedField diff() {
        return field;
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
