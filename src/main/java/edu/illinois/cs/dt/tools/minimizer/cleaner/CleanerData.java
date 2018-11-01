package edu.illinois.cs.dt.tools.minimizer.cleaner;

import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.testrunner.data.results.Result;

import java.util.List;

public class CleanerData {
    private final String dependentTest;
    private final List<String> deps;
    private final Result expected;
    private final Result isolationResult;
    private final List<String> testOrder;
    private final ListEx<CleanerGroup> cleaners;

    public CleanerData(final String dependentTest, final List<String> deps,
                       final Result expected, final Result isolationResult, final List<String> testOrder,
                       final ListEx<CleanerGroup> cleaners) {
        this.dependentTest = dependentTest;
        this.deps = deps;
        this.expected = expected;
        this.isolationResult = isolationResult;
        this.testOrder = testOrder;
        this.cleaners = cleaners;
    }

    public String dependentTest() {
        return dependentTest;
    }

    public List<String> deps() {
        return deps;
    }

    public Result expected() {
        return expected;
    }

    public Result isolationResult() {
        return isolationResult;
    }

    public List<String> testOrder() {
        return testOrder;
    }

    public ListEx<CleanerGroup> cleaners() {
        return cleaners;
    }
}
