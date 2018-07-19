package edu.illinois.cs.dt.tools.runner;

import edu.illinois.cs.dt.tools.runner.data.TestResult;

import java.util.List;

public abstract class Runner {
    private final ExecutionInfo executionInfo;

    public Runner(final ExecutionInfo executionInfo) {
        this.executionInfo = executionInfo;
    }

    public abstract TestResult runOrder(final List<String> order) throws Exception;
}
