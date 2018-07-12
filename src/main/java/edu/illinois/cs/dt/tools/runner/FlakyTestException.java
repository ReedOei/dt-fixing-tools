package edu.illinois.cs.dt.tools.runner;

import edu.washington.cs.dt.RESULT;

import java.util.List;

public class FlakyTestException extends RuntimeException {
    private final String testName;
    private final RESULT result;
    private final RESULT testResult;
    private final List<String> testsBefore;

    public FlakyTestException(final String testName,
                              final RESULT result,
                              final RESULT testResult,
                              final List<String> testsBefore) {
        super(String.format("Flaky test '%s' found. Result was both %s and %s when run in order %s", testName, result, testResult, testsBefore));

        this.testName = testName;
        this.result = result;
        this.testResult = testResult;
        this.testsBefore = testsBefore;
    }

    public String testName() {
        return testName;
    }

    public RESULT result() {
        return result;
    }

    public RESULT testResult() {
        return testResult;
    }

    public List<String> testsBefore() {
        return testsBefore;
    }

}
