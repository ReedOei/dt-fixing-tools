package edu.illinois.cs.dt.tools.minimizer.cleaner;

import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.testrunner.data.results.Result;
import edu.illinois.cs.dt.tools.utility.OperationTime;

import java.util.List;

public class CleanerData {
    private final String dependentTest;
    private final OperationTime time;
    private final Result expected;
    private final Result isolationResult;
    private final ListEx<CleanerGroup> cleaners;

    public CleanerData(final String dependentTest, final OperationTime time,
                       final Result expected, final Result isolationResult,
                       final ListEx<CleanerGroup> cleaners) {
        this.dependentTest = dependentTest;
        this.time = time;
        this.expected = expected;
        this.isolationResult = isolationResult;
        this.cleaners = cleaners;
    }

    public OperationTime time() {
        return time;
    }

    public String dependentTest() {
        return dependentTest;
    }

    public Result expected() {
        return expected;
    }

    public Result isolationResult() {
        return isolationResult;
    }

    public ListEx<CleanerGroup> cleaners() {
        return cleaners;
    }
}
