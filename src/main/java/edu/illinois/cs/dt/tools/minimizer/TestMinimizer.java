package edu.illinois.cs.dt.tools.minimizer;

import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.data.caching.FileCache;
import com.reedoei.eunomia.util.RuntimeThrower;
import com.reedoei.eunomia.util.Util;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestResult;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;
import scala.util.Try;

import javax.annotation.Nullable;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TestMinimizer extends FileCache<MinimizeTestsResult> {
    private final List<String> testOrder;
    private final String dependentTest;
    private final Result expected;
    private final Runner runner;

    private final Path path;

    @Nullable
    private MinimizeTestsResult minimizedResult = null;

    private void debug(final String str) {
        TestPluginPlugin.mojo().getLog().debug(str);
    }

    private void info(final String str) {
        TestPluginPlugin.mojo().getLog().info(str);
    }

    public TestMinimizer(final List<String> testOrder, final Runner runner, final String dependentTest) {
        this.testOrder = testOrder;
        this.dependentTest = dependentTest;

        this.runner = runner;

        // Run in given order to determine what the result should be.
        debug("Getting expected result for: " + dependentTest);
        this.expected = result(testOrder);
        debug("Expected: " + expected);

        this.path = MinimizerPathManager.minimized(dependentTest, expected);
    }

    public Result expected() {
        return expected;
    }

    private Result result(final List<String> order) {
        final List<String> actualOrder = new ArrayList<>(order);

        if (!actualOrder.contains(dependentTest)) {
            actualOrder.add(dependentTest);
        }

        return runner.runList(actualOrder)
                .flatMap(r -> Try.apply(() -> r.results().get(dependentTest)))
                .map(TestResult::result)
                .get();
    }

    private MinimizeTestsResult run() throws Exception {
        if (minimizedResult == null) {
            info("Running minimizer for: " + dependentTest);

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
            debug("Order is empty, so it is already minimized!");
            return deps;
        }

        debug("Trying dts as isolated dependencies.");
        if (tryIsolated(deps, order)) {
            return deps;
        }

        final int origSize = order.size();

        while (order.size() > 1) {
            debug("Trying both halves, " + order.size() + " dts remaining.");

            final Result topResult = result(Util.prependAll(deps, Util.topHalf(order)));
            debug("Top result: " + topResult);

            final Result botResult = result(Util.prependAll(deps, Util.botHalf(order)));
            debug("Bottom result: " + botResult);

            if (topResult == expected && botResult != expected) {
                order = Util.topHalf(order);
            } else if (topResult != expected && botResult == expected) {
                order = Util.botHalf(order);
            } else {
                // It's not 100% obvious what to do in this case (could have weird dependencies that are hard to deal with).
                // But sequential will definitely work (except because of flakiness for other reasons).
                return runSequential(deps, order);
            }
        }

        final Result orderResult = result(order);
        if (order.size() == 1 || orderResult == expected) {
            debug("Found dependencies: " + order);

            deps.addAll(order);
        } else {
            throw new MinimizeTestListException("Could not find dependencies. There is only one " +
                    "test left but the result '" + orderResult +
                    "' does not match expected '" + expected + "'");
        }

        return deps;
    }

    private boolean tryIsolated(final List<String> deps, final List<String> order) {
        debug("Trying dependent test '" + dependentTest + "' in isolation.");
        final Result isolated = result(Collections.singletonList(dependentTest));

        if (isolated == expected) {
            deps.clear();
            debug("Test has expected result in isolation.");
            return true;
        }

        for (int i = 0; i < order.size(); i++) {
            String test = order.get(i);

            debug("Running test " + i + " of " + order.size() + ". ");
            final Result r = result(Collections.singletonList(test));

            // Found an order where we get the expected result with just one test, can't be more
            // minimal than this.
            if (r == expected) {
                debug("Found dependency: " + test);
                deps.add(test);
                return true;
            }
        }

        return false;
    }

    private List<String> runSequential(final List<String> deps, final List<String> testOrder) {
        final List<String> remainingTests = new ArrayList<>(testOrder);

        while (!remainingTests.isEmpty()) {
            debug(String.format("Running sequentially, %d tests left", remainingTests.size()));
            final String current = remainingTests.remove(0);

            final List<String> order = Util.prependAll(deps, remainingTests);
            final Result r = result(order);

            if (r != expected) {
                debug("Found dependency: " + current);
                deps.add(current);
            }
        }

        debug("Found " + deps.size() + " dependencies.");

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
            minimizedResult.save();
        }
    }

    @Override
    protected MinimizeTestsResult generate() {
        return new RuntimeThrower<>(this::run).run();
    }
}
