package edu.illinois.cs.dt.tools.runner;

import edu.washington.cs.dt.RESULT;

import java.util.List;

public class FlakyTestException extends RuntimeException {
    public FlakyTestException(final String testName,
                              final RESULT result,
                              final RESULT testResult,
                              final List<String> testsBefore) {
        super(String.format("Flaky test '%s' found. Result was both %s and %s when run in order %s", testName, result, testResult, testsBefore));
    }
}
