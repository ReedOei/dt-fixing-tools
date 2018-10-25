package edu.illinois.cs.dt.tools.diagnosis.pollution;

public class PollutedField {
    private final String testName;
    private final String withoutDepsVal;
    private final String withDepsVal;

    public PollutedField(final String testName, final String withoutDepsVal, final String withDepsVal) {
        this.testName = testName;
        this.withoutDepsVal = withoutDepsVal;
        this.withDepsVal = withDepsVal;
    }

    public String testName() {
        return testName;
    }

    public String withDepsVal() {
        return withDepsVal;
    }

    public String withoutDepsVal() {
        return withoutDepsVal;
    }

    public boolean different() {
        return withoutDepsVal() != null && withDepsVal() != null && !withoutDepsVal().equals(withDepsVal());
    }
}
