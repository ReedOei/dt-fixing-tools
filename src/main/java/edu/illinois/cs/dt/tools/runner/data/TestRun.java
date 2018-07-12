package edu.illinois.cs.dt.tools.runner.data;

import edu.washington.cs.dt.RESULT;

import java.util.List;

public class TestRun {
    private final List<String> order;
    private final RESULT result;

    public TestRun(List<String> order, RESULT result) {
        this.order = order;
        this.result = result;
    }

    public List<String> order() {
        return order;
    }

    public RESULT result() {
        return result;
    }
}
