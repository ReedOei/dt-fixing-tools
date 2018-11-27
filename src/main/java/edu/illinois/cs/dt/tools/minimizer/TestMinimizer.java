package edu.illinois.cs.dt.tools.minimizer;

import com.google.gson.Gson;
import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.data.caching.FileCache;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.util.RuntimeThrower;
import com.reedoei.eunomia.util.Util;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import edu.illinois.cs.dt.tools.minimizer.cleaner.CleanerFinder;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.utility.MD5;
import edu.illinois.cs.dt.tools.utility.OperationTime;
import org.checkerframework.checker.nullness.qual.NonNull;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TestMinimizer extends FileCache<MinimizeTestsResult> {
    private final List<String> testOrder;
    private final String dependentTest;
    private final Result expected;
    private final Result isolationResult;
    private final InstrumentingSmartRunner runner;

    private final Path path;

    private TestRunResult expectedRun;

    private void debug(final String str) {
        TestPluginPlugin.mojo().getLog().debug(str);
    }

    private void info(final String str) {
        TestPluginPlugin.mojo().getLog().info(str);
    }

    public TestMinimizer(final List<String> testOrder, final InstrumentingSmartRunner runner, final String dependentTest) {
        // Only take the tests that come before the dependent test
        this.testOrder = testOrder.contains(dependentTest) ? ListUtil.before(testOrder, dependentTest) : testOrder;
        this.dependentTest = dependentTest;

        this.runner = runner;

        // Run in given order to determine what the result should be.
        debug("Getting expected result for: " + dependentTest);
        this.expectedRun = runResult(testOrder);
        this.expected = expectedRun.results().get(dependentTest).result();
        this.isolationResult = result(Collections.singletonList(dependentTest));
        debug("Expected: " + expected);

        this.path = MinimizerPathManager.minimized(dependentTest, MD5.hashOrder(expectedRun.testOrder()), expected);
    }

    public Result expected() {
        return expected;
    }

    private TestRunResult runResult(final List<String> order) {
        final List<String> actualOrder = new ArrayList<>(order);

        if (!actualOrder.contains(dependentTest)) {
            actualOrder.add(dependentTest);
        }

        return runner.runList(actualOrder).get();
    }

    private Result result(final List<String> order) {
        return runResult(order).results().get(dependentTest).result();
    }

    public MinimizeTestsResult run() throws Exception {
        return OperationTime.runOperation(() -> {
            info("Running minimizer for: " + dependentTest + " (expected result in this order: " + expected + ")");

            final List<String> order =
                    testOrder.contains(dependentTest) ? ListUtil.beforeInc(testOrder, dependentTest) : new ArrayList<>(testOrder);

            // Keep going as long as there are tests besides dependent test to run
            List<PolluterData> polluters = new ArrayList<>();
            while (!order.isEmpty()) {
                // First need to check if remaining tests in order still lead to expected value
                if (result(order) != expected) {
                    info("Remaining tests no longer match expected: " + order);
                    break;
                }

                final List<String> deps = run(new ArrayList<>(order));
                if (deps.isEmpty()) {
                    info("Did not find any deps");
                    break;
                }

                info("Ran minimizer, dependencies: " + deps);

                polluters.add(new PolluterData(deps,
                    new CleanerFinder(runner, dependentTest, deps, expected, isolationResult, expectedRun.testOrder()).find()));

                order.removeAll(deps);  // Look for other deps besides the ones already found
            }

            return polluters;
        }, (polluters, time) -> {
            final MinimizeTestsResult minimizedResult =
                    new MinimizeTestsResult(time, expectedRun, expected, dependentTest, polluters);

            minimizedResult.verify(runner);

            return minimizedResult;
        });
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
        if (isolationResult.equals(expected)) {
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
    public @NonNull Path path() {
        return path;
    }

    @Override
    protected MinimizeTestsResult load() {
        return new RuntimeThrower<>(() -> new Gson().fromJson(FileUtil.readFile(path()), MinimizeTestsResult.class)).run();
    }

    @Override
    protected void save() {
        new RuntimeThrower<>(() -> {
            Files.createDirectories(path().getParent());
            Files.write(path(), new Gson().toJson(get()).getBytes());

            return null;
        }).run();
    }

    @NonNull
    @Override
    protected MinimizeTestsResult generate() {
        return new RuntimeThrower<>(this::run).run();
    }
}
