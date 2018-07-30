package edu.illinois.cs.dt.tools.minimizer;

import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.data.caching.FileCache;
import com.reedoei.eunomia.io.VerbosePrinter;
import com.reedoei.eunomia.util.RuntimeThrower;
import com.reedoei.eunomia.util.Util;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.washington.cs.dt.RESULT;
import edu.washington.cs.dt.TestExecResult;
import org.checkerframework.checker.initialization.qual.Initialized;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.UnknownKeyFor;

import javax.annotation.Nullable;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TestMinimizer extends FileCache<MinimizeTestsResult> implements VerbosePrinter {
    private final List<String> testOrder;
    private final String classpath;
    private final String dependentTest;
    private final RESULT expected;
    private final SmartTestRunner runner;

    private final int verbosity;
    private final Path path;

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
        this(testOrder, classpath, dependentTest, javaAgent, 1);
    }

    public TestMinimizer(final List<String> testOrder, final String classpath, final String dependentTest, final Path javaAgent, int verbosity)
            throws Exception {
        this.testOrder = testOrder;
        this.classpath = classpath;
        this.dependentTest = dependentTest;
        this.verbosity = verbosity;

        this.runner = new SmartTestRunner(classpath, javaAgent);

        // Run in given order to determine what the result should be.
        println("[INFO] Getting expected result for: " + dependentTest);
        print("[INFO]");
        this.expected = result(testOrder);
        println(" Expected: " + expected);

        this.path = MinimizeTestsResult.path(dependentTest, expected, Paths.get("minimized"));
    }

    public RESULT expected() {
        return expected;
    }

    @Override
    public int verbosity() {
        return verbosity;
    }

    private RESULT result(final List<String> order) throws Exception {
        final List<String> actualOrder = new ArrayList<>(order);

        if (!actualOrder.contains(dependentTest)) {
            actualOrder.add(dependentTest);
        }

        final TestExecResult results = runner.runOrder(actualOrder).result();

        return results.getResult(dependentTest).result;
    }

    private MinimizeTestsResult run() throws Exception {
        if (minimizedResult == null) {
            System.out.println("[INFO] Running minimizer for: " + dependentTest);

            final List<String> order =
                    testOrder.contains(dependentTest) ? ListUtil.beforeInc(testOrder, dependentTest) : new ArrayList<>(testOrder);

            minimizedResult = new MinimizeTestsResult(expected, dependentTest, run(order));
            minimizedResult.verify(runner);
        }

        return minimizedResult;
    }

    private List<String> run(List<String> order) throws Exception {
        final List<String> deps = new ArrayList<>();

        if (order.isEmpty()) {
            println("[INFO] Order is empty, so it is already minimized!");
            return deps;
        }

        println("[INFO] Trying dts as isolated dependencies.");
        if (tryIsolated(deps, order)) {
            return deps;
        }

        final int origSize = order.size();

        while (order.size() > 1) {
            print("\r\033[2K[INFO] Trying both halves, " + order.size() + " dts remaining.");

            final RESULT topResult = result(Util.prependAll(deps, Util.topHalf(order)));
            print(" Top result: " + topResult);

            final RESULT botResult = result(Util.prependAll(deps, Util.botHalf(order)));
            print(", Bottom result: " + botResult);

            if (topResult == expected && botResult != expected) {
                order = Util.topHalf(order);
            } else if (topResult != expected && botResult == expected) {
                order = Util.botHalf(order);
            } else {
                println();
                // It's not 100% obvious what to do in this case (could have weird dependencies that are hard to deal with).
                // But sequential will definitely work (except because of flakiness for other reasons).
                return runSequential(deps, order);
            }
        }

        if (origSize > 1) {
            println();
        }

        print("[INFO] ");
        final RESULT orderResult = result(order);
        if (order.size() == 1 || orderResult == expected) {
            println(" Found dependencies: " + order);

            deps.addAll(order);
        } else {
            throw new MinimizeTestListException("Could not find dependencies. There is only one " +
                    "test left but the result '" + orderResult +
                    "' does not match expected '" + expected + "'");
        }

        return deps;
    }

    private boolean tryIsolated(final List<String> deps, final List<String> order) throws Exception {
        print("[INFO] Trying dependent test '" + dependentTest + "' in isolation.");
        final RESULT isolated = result(Collections.singletonList(dependentTest));
        println();

        // TODO: Move to another method probably.
        if (isolated == expected) {
            deps.clear();
            println("[INFO] Test has expected result in isolation.");
            return true;
        }

        for (int i = 0; i < order.size(); i++) {
            String test = order.get(i);

            print("\r\033[2K[INFO] Running test " + i + " of " + order.size() + ". ");
            final RESULT r = result(Collections.singletonList(test));

            // Found an order where we get the expected result with just one test, can't be more
            // minimal than this.
            if (r == expected) {
                println();
                println("[INFO] Found dependency: " + test);
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

            print(String.format("\r\033[2K[INFO] Running sequentially, %d dts left (%d seconds remaining)", remainingTests.size(), estimated));
            final String current = remainingTests.remove(0);

            final List<String> order = Util.prependAll(deps, remainingTests);
            final RESULT r = result(order);
            testsRun += order.size();

            if (r != expected) {
                println();
                println("[INFO] Found dependency: " + current);
                deps.add(current);
            }
        }

        println();
        println("[INFO] Found " + deps.size() + " dependencies.");

        return deps;
    }

    public String getDependentTest() {
        return dependentTest;
    }

    @Override
    public Path path() {
        return path;
    }

    @Override
    protected MinimizeTestsResult load() {
        minimizedResult = new RuntimeThrower<>(() -> MinimizeTestsResult.fromPath(path())).run();
        return minimizedResult;
    }

    @Override
    protected void save() {
        if (minimizedResult != null) {
            minimizedResult.print(path().getParent());
        }
    }

    @Override
    protected MinimizeTestsResult generate() {
        return new RuntimeThrower<>(this::run).run();
    }
}
