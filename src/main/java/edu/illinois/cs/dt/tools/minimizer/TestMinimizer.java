package edu.illinois.cs.dt.tools.minimizer;

import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.util.Util;
import edu.illinois.cs.dt.tools.runner.FlakyTestException;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;
import edu.washington.cs.dt.RESULT;
import edu.washington.cs.dt.TestExecResult;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

public class TestMinimizer {
    private final List<String> testOrder;
    private final String classpath;
    private final String dependentTest;
    private final RESULT expected;
    private final SmartTestRunner runner;

    @Nullable
    private MinimizeTestsResult minimizedResult = null;

    public TestMinimizer(final List<String> testOrder, final String dependentTest)
            throws FlakyTestException, InterruptedException, ExecutionException, TimeoutException {
        this(testOrder, System.getProperty("java.class.path"), dependentTest);
    }

    public TestMinimizer(final List<String> testOrder, final String classpath, final String dependentTest)
            throws FlakyTestException, InterruptedException, ExecutionException, TimeoutException {
        this.testOrder = testOrder;
        this.classpath = classpath;
        this.dependentTest = dependentTest;

        this.runner = new SmartTestRunner(classpath);

        // Run in given order to determine what the result should be.
        System.out.println("[INFO] Getting expected result for: " + dependentTest);
        this.expected = result(testOrder);
        System.out.println("[INFO] Expected: " + expected);
    }

    private RESULT result(final List<String> order) throws FlakyTestException, InterruptedException, ExecutionException, TimeoutException {
        final List<String> actualOrder = new ArrayList<>(order);

        if (!actualOrder.contains(dependentTest)) {
            actualOrder.add(dependentTest);
        }

        final TestExecResult results = runner.runOrder(actualOrder);

        return results.getResult(dependentTest).result;
    }

    public MinimizeTestsResult run()
            throws MinimizeTestListException, FlakyTestException, InterruptedException, ExecutionException, TimeoutException {
        if (minimizedResult == null) {
            System.out.println("[INFO] Running minimizer for: " + dependentTest);

            final List<String> order =
                    testOrder.contains(dependentTest) ? ListUtil.beforeInc(testOrder, dependentTest) : new ArrayList<>(testOrder);

            minimizedResult = new MinimizeTestsResult(dependentTest, run(order));
            System.out.println();
        }

        return minimizedResult;
    }

    private List<String> run(List<String> order)
            throws MinimizeTestListException, FlakyTestException, InterruptedException, ExecutionException, TimeoutException {
        final List<String> deps = new ArrayList<>();

        if (order.isEmpty()) {
            System.out.println("[INFO] Order is empty, so it is already minimized!");
            return deps;
        }

        final int origSize = order.size();

        while (order.size() > 1) {
            System.out.print("\r\033[2K[INFO] Trying both halves, " + order.size() + " tests remaining.");

            final RESULT topResult = result(Util.prependAll(deps, Util.topHalf(order)));
            System.out.print(" Top result: " + topResult);

            final RESULT botResult = result(Util.prependAll(deps, Util.botHalf(order)));
            System.out.print(", Bottom result: " + botResult);

            if (topResult == expected && botResult != expected) {
                order = Util.topHalf(order);
            } else if (topResult != expected && botResult == expected) {
                order = Util.botHalf(order);
            } else {
                System.out.println();
                // It's not 100% obvious what to do in this case (could have weird dependencies that are hard to deal with).
                // But sequential will definitely work (except because of flakiness for other reasons).
                return runSequential(deps, order);
            }
        }

        if (origSize > 1) {
            System.out.println();
        }

        final RESULT orderResult = result(order);
        if (order.size() == 1 || orderResult == expected) {
            System.out.println("Found dependencies: " + order);

            deps.addAll(order);
        } else {
            throw new MinimizeTestListException("Could not find dependencies. There is only one " +
                    "test left but the result '" + orderResult +
                    "' does not match expected '" + expected + "'");
        }

        return deps;
    }

    private int triangleNum(final int n) {
        return n * (n + 1) / 2;
    }

    private long estimate(final long currentTime, final double testsRun, final int numTests, final int deps) {
        if (testsRun == 0) {
            return 0;
        } else {
            final double averageTime = currentTime / testsRun;
            final int estimatedDeps = (int) (deps + (((double) deps) / testsRun));

            return (long) (averageTime * (numTests + estimatedDeps * numTests + triangleNum(numTests)) / 1000.0);
        }
    }

    private List<String> runSequential(final List<String> deps, final List<String> testOrder)
            throws FlakyTestException, InterruptedException, ExecutionException, TimeoutException {
        final long startTime = System.currentTimeMillis();
        int testsRun = 0;

        final List<String> remainingTests = new ArrayList<>(testOrder);

        while (!remainingTests.isEmpty()) {
            final long endTime = System.currentTimeMillis();
            final long estimated = estimate(endTime - startTime, testsRun, remainingTests.size(), deps.size());

            System.out.print("\r\033[2K[INFO] Running sequentially, " + remainingTests.size() + " tests left (" + estimated + " seconds remaining).");
            final String current = remainingTests.remove(0);

            final List<String> order = Util.prependAll(deps, remainingTests);
            final RESULT r = result(order);
            testsRun += order.size();

            if (r != expected) {
                System.out.println();
                System.out.println("[INFO] Found dependency: " + current);
                deps.add(current);
            }
        }

        System.out.println();
        System.out.println("[INFO] Found " + deps.size() + " dependencies.");

        return deps;
    }
}
