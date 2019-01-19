package edu.illinois.cs.dt.tools.minimizer;

import com.google.gson.Gson;
import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.data.caching.FileCache;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.util.RuntimeThrower;
import com.reedoei.eunomia.util.Util;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import edu.illinois.cs.dt.tools.minimizer.cleaner.CleanerData;
import edu.illinois.cs.dt.tools.minimizer.cleaner.CleanerFinder;
import edu.illinois.cs.dt.tools.minimizer.cleaner.CleanerGroup;
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

                // Only look for cleaners if the order is not passing; in case of minimizing for setter don't need to look for cleaner
                CleanerData cleanerData;
                if (!expected.equals(Result.PASS)) {
                    cleanerData = new CleanerFinder(runner, dependentTest, deps, expected, isolationResult, expectedRun.testOrder()).find();
                } else {
                    cleanerData = new CleanerData(dependentTest, new OperationTime(0, 0), expected, isolationResult, new ListEx<CleanerGroup>());
                }

                polluters.add(new PolluterData(deps, cleanerData));

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

    private List<String> deltaDebug(final List<String> deps, int n) throws Exception {
        // If n granularity is greater than number of tests, then finished, simply return passed in tests
        if (deps.size() < n) {
            return deps;
        }

        // Cut the tests into n equal chunks and try each chunk
        int chunkSize = (int)Math.round((double)(deps.size()) / n);
        List<List<String>> chunks = new ArrayList<>();
        for (int i = 0; i < deps.size(); i += chunkSize) {
            List<String> chunk = new ArrayList<>();
            List<String> otherChunk = new ArrayList<>();
            // Create chunk starting at this iteration
            int endpoint = Math.min(deps.size(), i + chunkSize);
            chunk.addAll(deps.subList(i, endpoint));

            // Complement chunk are tests before and after this current chunk
            otherChunk.addAll(deps.subList(0, i));
            otherChunk.addAll(deps.subList(endpoint, deps.size()));

            // Check if running this chunk works
            if (this.expected == result(chunk)) {
                return deltaDebug(chunk, 2); // If works, then delta debug some more this chunk
            }
            // Otherwise, check if applying complement chunk works
            if (this.expected == result(otherChunk)) {
                return deltaDebug(otherChunk, 2);   // If works, then delta debug some more the complement chunk
            }
        }
        // If not chunk/complement work, increase granularity and try again
        return deltaDebug(deps, n * 2);
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

        deps.addAll(deltaDebug(order, 2));

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
