package edu.illinois.cs.dt.tools.runner;

import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.math.MathUtil;
import edu.washington.cs.dt.OneTestExecResult;
import edu.washington.cs.dt.RESULT;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TestInfo {
    // The key is all tests in the order, and the value is the result of the last test.
    // This is used to discover flaky tests during runs of this tool.
    private final Map<List<String>, RESULT> knownRuns = new HashMap<>();

    private final List<Long> times = new ArrayList<>();

    private final String testName;

    public TestInfo(final List<String> order, final String testName, final OneTestExecResult result)
            throws FlakyTestException {
        this.testName = testName;
        updateWith(order, result);
    }

    public void updateWith(final List<String> order, final OneTestExecResult result) throws FlakyTestException {
        updateTime(result);
        updateFlakiness(order, result);
    }

    private void updateTime(final OneTestExecResult result) {
        // Time is in nanoseconds
        times.add((long) (result.getExecTime() / 1E9));
    }

    private void updateFlakiness(List<String> order, OneTestExecResult result) throws FlakyTestException {
        final RESULT testResult = result.result;

        final List<String> testsBefore = ListUtil.beforeInc(order, testName);

        if (knownRuns.containsKey(testsBefore) && !knownRuns.get(testsBefore).equals(testResult)) {
            throw new FlakyTestException(testName, knownRuns.get(testsBefore), testResult, testsBefore);
        } else {
            knownRuns.put(testsBefore, testResult);
        }
    }

    public double averageTime() {
        return MathUtil.sum(times) / times.size();
    }
}
