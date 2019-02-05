package edu.illinois.cs.dt.tools.minimizer.cleaner;

import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.testrunner.data.results.Result;
import edu.illinois.cs.dt.tools.utility.OperationTime;

public class CleanerData {
    private final String dependentTest;
    private final OperationTime time;
    private final Result expected;
    private final Result isolationResult;
    private final ListEx<CleanerGroup> cleaners;

    public CleanerData(final String dependentTest,
                       final Result expected, final Result isolationResult,
                       final ListEx<CleanerGroup> cleaners) {
        this.dependentTest = dependentTest;
        this.expected = expected;
        this.isolationResult = isolationResult;
        this.cleaners = cleaners;

        if (this.cleaners.isEmpty()) {
            this.time = new OperationTime(0,0);
        } else {
            this.time = cleaners.first().get().time();
            cleaners.stream().map(cleaner -> this.time.addTime(cleaner.time()));
        }
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
