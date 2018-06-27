package edu.illinois.cs.dt.tools.runner;

import edu.washington.cs.dt.RESULT;

import java.util.List;

public class FlakyTestException extends RuntimeException {
    public FlakyTestException(String testName, RESULT result, RESULT testResult, List<String> testsBefore) {
    }
}
