package edu.illinois.cs.dt.tools.runner;

import com.reedoei.eunomia.math.Averager;
import edu.washington.cs.dt.OneTestExecResult;
import edu.washington.cs.dt.TestExecResult;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TestInfoStore {
    // 6 hours in seconds. This is the time to use when we don't know how long tests should take.
    private static final long MAX_DEFAULT_TIMEOUT = 6 * 3600;
    // How much longer we should wait than expected.
    private static final double TIMEOUT_MULTIPLIER = 2.5;
    private static final long TIMEOUT_OFFSET = 5; // Add a flat 5 seconds to all timeouts

    private final Map<String, TestInfo> testInfo = new HashMap<>();

    public TestInfoStore() {
    }

    public void update(final List<String> order, final TestExecResult results) throws FlakyTestException {
        for (final String testName : results.getAllTests()) {
            final OneTestExecResult result = results.getResult(testName);
            if (testInfo.containsKey(testName)) {
                testInfo.get(testName).updateWith(order, result);
            } else {
                testInfo.put(testName, new TestInfo(order, testName, result));
            }
        }
    }

    public long getTimeout(final List<String> order) {
        double totalExpectedTime = 0.0;

        for (final String s : order) {
            if (!testInfo.containsKey(s)) {
                return MAX_DEFAULT_TIMEOUT;
            } else {
                totalExpectedTime += testInfo.get(s).averageTime();
            }
        }

        return order.size() + TIMEOUT_OFFSET + (long) (TIMEOUT_MULTIPLIER * totalExpectedTime);
    }

    public double averageTime() {
        return new Averager<>(testInfo.values().stream().map(TestInfo::averageTime)).mean();
    }
}
