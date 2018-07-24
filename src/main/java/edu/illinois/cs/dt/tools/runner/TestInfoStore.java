package edu.illinois.cs.dt.tools.runner;

import com.reedoei.eunomia.math.Averager;
import edu.illinois.cs.dt.tools.configuration.Configuration;
import edu.washington.cs.dt.OneTestExecResult;
import edu.washington.cs.dt.TestExecResult;
import edu.washington.cs.dt.TestExecResults;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TestInfoStore {
    // This is the time to use when we don't know how long dts should take.
    // This time is still affected by the other modifiers below.
    private static final long MAX_DEFAULT_TIMEOUT = (long) Configuration.config().getProperty("runner.timeout.max", 3 * 3600);
    // How much longer we should wait than expected.
    private static final double TIMEOUT_MULTIPLIER = Configuration.config().getProperty("runner.timeout.multiplier", 4.0);
    // Adds a flat number of seconds to all timeouts
    private static final double TIMEOUT_OFFSET = Configuration.config().getProperty("runner.timeout.offset", 5.0);
    // Add a flat number of seconds per test.
    private static final double PER_TEST_MULTIPLIER = Configuration.config().getProperty("runner.timeout.pertest", 2.0);

    private final Map<String, TestInfo> testInfo = new HashMap<>();
    private final Averager<Double> testTimes = new Averager<>();

    public TestInfoStore() {
    }

    public void update(final List<String> order, final TestExecResults results) throws FlakyTestException {
        results.getExecutionRecords().forEach(result -> update(order, result));
    }

    public void update(final List<String> order, final TestExecResult results) throws FlakyTestException {
        for (final String testName : results.getAllTests()) {
            final OneTestExecResult result = results.getResult(testName);
            if (testInfo.containsKey(testName)) {
                testInfo.get(testName).updateWith(order, result);
                testTimes.add(results.getResult(testName).getExecTime() / 1E9);
            } else {
                testInfo.put(testName, new TestInfo(order, testName, result));
            }
        }
    }

    public long getTimeout(final List<String> order) {
        double totalExpectedTime = 0.0;

        for (final String s : order) {
            if (!testInfo.containsKey(s)) {
                totalExpectedTime = MAX_DEFAULT_TIMEOUT;
                break;
            } else {
                totalExpectedTime += testInfo.get(s).averageTime();
            }
        }

        return (long) (PER_TEST_MULTIPLIER * order.size() + TIMEOUT_OFFSET + TIMEOUT_MULTIPLIER * totalExpectedTime);
    }

    public double averageTime() {
        return testTimes.mean();
    }

    public boolean isFlaky(final String testName) {
        return testInfo.containsKey(testName) && testInfo.get(testName).isFlaky();
    }
}
