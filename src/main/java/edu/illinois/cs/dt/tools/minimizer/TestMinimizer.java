package edu.illinois.cs.dt.tools.minimizer;

import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.io.IOUtil;
import com.reedoei.eunomia.util.Util;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;
import edu.washington.cs.dt.RESULT;
import edu.washington.cs.dt.TestExecResult;

import javax.annotation.Nullable;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TestMinimizer {
    private final List<String> testOrder;
    private final String classpath;
    private final String dependentTest;
    private final RESULT expected;
    private final SmartTestRunner runner;

    @Nullable
    private MinimizeTestsResult minimizedResult = null;

    public TestMinimizer(final List<String> testOrder, final String dependentTest) throws Exception {
        this(testOrder, System.getProperty("java.class.path"), dependentTest);
    }

    public TestMinimizer(final List<String> testOrder, final String classpath, final String dependentTest) throws Exception {
        this(testOrder, classpath, dependentTest, Paths.get(""));
    }

    public TestMinimizer(final List<String> testOrder, final String classpath, final String dependentTest, final Path javaAgent)
            throws Exception {
        this.testOrder = testOrder;
        this.classpath = classpath;
        this.dependentTest = dependentTest;

        this.runner = new SmartTestRunner(classpath, javaAgent);

        // Run in given order to determine what the result should be.
        System.out.println("[INFO] Getting expected result for: " + dependentTest);
        System.out.print("[INFO]");
        this.expected = result(testOrder);
        System.out.println(" Expected: " + expected);
    }

    private RESULT result(final List<String> order) throws Exception {
        final List<String> actualOrder = new ArrayList<>(order);

        if (!actualOrder.contains(dependentTest)) {
            actualOrder.add(dependentTest);
        }

        final TestExecResult results = runner.runOrder(actualOrder).result();

        return results.getResult(dependentTest).result;
    }

    public MinimizeTestsResult run() throws Exception {
        if (minimizedResult == null) {
            System.out.println("[INFO] Running minimizer for: " + dependentTest);

            final List<String> order =
                    testOrder.contains(dependentTest) ? ListUtil.beforeInc(testOrder, dependentTest) : new ArrayList<>(testOrder);

            minimizedResult = new MinimizeTestsResult(expected, dependentTest, run(order));
            minimizedResult.verify(runner);

            System.out.println();
        }

        return minimizedResult;
    }

    private List<String> run(List<String> order) throws Exception {
        final List<String> deps = new ArrayList<>();

        if (order.isEmpty()) {
            System.out.println("[INFO] Order is empty, so it is already minimized!");
            return deps;
        }

        System.out.println("[INFO] Trying tests as isolated dependencies.");
        if (tryIsolated(deps, order)) {
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

        System.out.print("[INFO] ");
        final RESULT orderResult = result(order);
        if (order.size() == 1 || orderResult == expected) {
            System.out.println(" Found dependencies: " + order);

            deps.addAll(order);
        } else {
            throw new MinimizeTestListException("Could not find dependencies. There is only one " +
                    "test left but the result '" + orderResult +
                    "' does not match expected '" + expected + "'");
        }

        return deps;
    }

    private boolean tryIsolated(final List<String> deps, final List<String> order) throws Exception {
        System.out.print("[INFO] Trying dependent test '" + dependentTest + "' in isolation.");
        final RESULT isolated = result(Collections.singletonList(dependentTest));
        System.out.println();

        // TODO: Move to another method probably.
        if (isolated == expected) {
            deps.clear();
            System.out.println("[INFO] Test has expected result in isolation.");
            return true;
        }

        for (int i = 0; i < order.size(); i++) {
            String test = order.get(i);

            IOUtil.printClearLine("[INFO] Running test " + i + " of " + order.size() + ". ");
            final RESULT r = result(Collections.singletonList(test));

            // Found an order where we get the expected result with just one test, can't be more
            // minimal than this.
            if (r == expected) {
                System.out.println();
                System.out.println("[INFO] Found dependency: " + test);
                deps.add(test);
                return true;
            }
        }

        return false;
    }

    private int triangleNum(final int n) {
        return n * (n + 1) / 2;
    }

    private long estimate(final double testsRun, final int numTests, final int deps) {
        final int estimatedDeps = testsRun == 0 ? deps : (int) (deps + (((double) deps) / testsRun));

        return (long) (runner.averageTestTime() * (numTests + estimatedDeps * numTests + triangleNum(numTests)));
    }

    private List<String> runSequential(final List<String> deps, final List<String> testOrder)
            throws Exception {
        int testsRun = 0;

        final List<String> remainingTests = new ArrayList<>(testOrder);

        while (!remainingTests.isEmpty()) {
            final long estimated = estimate(testsRun, remainingTests.size(), deps.size());

            IOUtil.printClearLine(String.format("[INFO] Running sequentially, %d tests left (%d seconds remaining)", remainingTests.size(), estimated));
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

    public String getDependentTest() {
        return dependentTest;
    }
}
