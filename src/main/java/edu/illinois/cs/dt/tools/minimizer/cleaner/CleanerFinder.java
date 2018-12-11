package edu.illinois.cs.dt.tools.minimizer.cleaner;

import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.eunomia.collections.StreamUtil;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import edu.illinois.cs.dt.tools.detection.DetectorPathManager;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.runner.RunnerPathManager;
import edu.illinois.cs.dt.tools.utility.OperationTime;
import edu.illinois.cs.dt.tools.utility.TestRunParser;
import org.codehaus.plexus.util.StringUtils;
import scala.util.Try;

import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

public class CleanerFinder {
    private final InstrumentingSmartRunner runner;
    private final String dependentTest;
    private final List<String> deps;
    private final Result expected;
    private final Result isolationResult;
    private final List<String> testOrder;

    public CleanerFinder(final InstrumentingSmartRunner runner,
                         final String dependentTest, final List<String> deps,
                         final Result expected, final Result isolationResult, final List<String> testOrder) {
        this.runner = runner;
        this.dependentTest = dependentTest;
        this.deps = deps;
        this.expected = expected;
        this.isolationResult = isolationResult;
        this.testOrder = testOrder;
    }

    /**
     * Finds minimal cleaner groups.
     *
     * A group of tests "c" forms a cleaner group for a dependent test "t" with a non-empty list of dependencies "ds" if:
     * the sequence [ds, c, t] has the same result as [t] (i.e., the isolation result)
     *
     * A minimal cleaner group is a cleaner group such that, if any test is removed, it is no longer a cleaner group.
     *
     * This analysis is sound but not complete:
     * All minimal cleaner groups were confirmed to be minimal cleaner groups (barring non-determinism), but this method
     * may not find ALL minimal cleaner groups.
     * @return Cleaner data, containing the confirmed cleaner groups.
     * @throws IOException If the original order file does not exist
     */
    public CleanerData find() throws Exception {
        final long startTime = System.currentTimeMillis();
        TestPluginPlugin.info("Looking for cleaners for: " + dependentTest);

        // If it's empty, we can't have any cleaners (they'd be polluters, not cleaners
        if (deps.isEmpty()) {
            TestPluginPlugin.info("No dependencies for " + dependentTest + " in this order, so no cleaners.");
            return new CleanerData(dependentTest, OperationTime.instantaneous(),
                    expected, isolationResult, new ListEx<>());
        } else {
            final ListEx<String> originalOrder = new ListEx<>(Files.readAllLines(DetectorPathManager.originalOrderPath()));
            return summarizeCleanerGroups(makeCleanerData(findCleanerGroups(originalOrder)));
        }
    }

    private CleanerData makeCleanerData(final ListEx<ListEx<String>> cleanerGroups) throws Exception {
        return OperationTime.runOperation(() -> cleanerGroups
                .mapWithIndex(this::minimalCleanerGroup)
                .distinct()
                .filter(cleanerGroup -> cleanerGroup.confirm(runner, new ListEx<>(deps), expected, isolationResult)),
            (minimizedCleanerGroups, time) -> {
                final CleanerData cleanerData = new CleanerData(dependentTest, time, expected, isolationResult, minimizedCleanerGroups);
                TestPluginPlugin.info(dependentTest + " has " + cleanerData.cleaners().size() + " cleaners: " + cleanerData.cleaners());
                return cleanerData;
            });
    }

    private ListEx<ListEx<String>> findCleanerGroups(final ListEx<String> originalOrder) {
        final ListEx<ListEx<String>> candidates = new ListEx<>(cleanerCandidates(originalOrder)).distinct();
        TestPluginPlugin.info("Found " + candidates.size() + " cleaner group candidates.");

        final ListEx<ListEx<String>> cleanerGroups = filterCleanerGroups(candidates);
        TestPluginPlugin.info("Found " + cleanerGroups.size() + " cleaner groups.");

        return cleanerGroups;
    }

    private CleanerData summarizeCleanerGroups(final CleanerData cleanerData) {
        if (!cleanerData.cleaners().isEmpty()) {
            for (final CleanerGroup cleanerGroup : cleanerData.cleaners()) {
                TestPluginPlugin.info(dependentTest + " cleaner group size: " + cleanerGroup.cleanerTests().size());

                if (cleanerGroup.cleanerTests().any(testOrder::contains)) {
                    TestPluginPlugin.info("Cleaner group contains a test that is in the failing order: " + cleanerGroup.cleanerTests());
                }
            }
        }

        return cleanerData;
    }

    private ListEx<ListEx<String>> filterCleanerGroups(final ListEx<ListEx<String>> candidates) {
        final ListEx<ListEx<String>> result = new ListEx<>();

        for (int i = 0; i < candidates.size(); i++) {
            final ListEx<String> candidate = candidates.get(i);
            System.out.printf("\rTrying group %d of %d (found %d so far)", i, candidates.size(), result.size());

            if (isCleanerGroup(candidate)) {
                result.add(candidate);
            }
        }

        System.out.print("\r");

        return result;
    }

    /**
     * @param cleanerCandidate The tests that make up the cleaner group
     * @return if the candidate satisfies the criteria above (changes the results)
     */
    private boolean isCleanerGroup(final ListEx<String> cleanerCandidate) {
        final List<String> tests = new ArrayList<>(deps);
        tests.addAll(cleanerCandidate);
        tests.add(dependentTest);

        final Try<TestRunResult> testRunResultTry = runner.runList(tests);

        return testRunResultTry.isSuccess() &&
               testRunResultTry.get().results().get(dependentTest).result().equals(isolationResult);
    }

    /**
     * @param cleanerGroup The list of tests that is a known, but not minimal, cleaner group
     * @return A minimal cleaner group
     */
    private CleanerGroup minimalCleanerGroup(final int i, final ListEx<String> cleanerGroup) {
        TestPluginPlugin.info("Minimizing cleaner group " + i + ": " +
                StringUtils.abbreviate(String.valueOf(cleanerGroup), 500));
        return new CleanerGroup(dependentTest, cleanerGroup.size(), reduce(cleanerGroup));
    }

    private ListEx<String> reduce(final ListEx<String> cleanerGroup) {
        final ListEx<String> binaryReduced = binaryReduce(cleanerGroup);

        TestPluginPlugin.debug("Reduced group " + cleanerGroup + " to " + binaryReduced);

        if (binaryReduced.size() > 1) {
            return sequentialReduce(binaryReduced);
        } else {
            return binaryReduced;
        }
    }

    private ListEx<String> sequentialReduce(final ListEx<String> reduced) {
        final ListEx<String> result = new ListEx<>(reduced);

        int i = 0;
        while (result.size() > 1 && i < result.size()) {
            final String testName = result.remove(i);

            // If it's not a cleaner group without this test, then we have to put it back
            // otherwise, we can just keep removing
            if (!isCleanerGroup(result)) {
                result.add(i, testName);
                i++;
            }
        }

        return result;
    }

    private ListEx<String> binaryReduce(final ListEx<String> cleanerGroup) {
        TestPluginPlugin.debug("Reducing: " + cleanerGroup);

        if (cleanerGroup.size() <= 1) {
            return cleanerGroup;
        }

        if (isCleanerGroup(cleanerGroup.topHalf())) {
            TestPluginPlugin.debug("Taking top half: " + cleanerGroup.topHalf());
            return binaryReduce(cleanerGroup.topHalf());
        } else if (isCleanerGroup(cleanerGroup.botHalf())) {
            TestPluginPlugin.debug("Taking bot half: " + cleanerGroup.botHalf());
            return binaryReduce(cleanerGroup.botHalf());
        } else {
            TestPluginPlugin.debug("Done: " + cleanerGroup);
            return cleanerGroup;
        }
    }

    /**
     * @return The lists of tests which could possibly be a cleaner group, with groups that are more likely to be cleaners coming first
     *         A cleaner group is considered "better" if they come between the polluter(s)
     *         and the dependent test does not have the expected result in that order
     */
    private Stream<ListEx<String>> cleanerCandidates(final ListEx<String> originalOrder) {
        if (deps.isEmpty()) {
            return Stream.empty();
        }

        return Stream.concat(
                Stream.concat(highLikelihoodCleanerGroups(), Stream.of(possibleCleaners(originalOrder))),
                // Consider each test as a possible cleaner
                originalOrder.stream().map(ListEx::fromArray));
//                // Consider each possible cleaner individually as well, in case there are other polluters
//                Stream.of(possibleCleaners(originalOrder)).flatMap(l -> l.map(ListEx::fromArray).stream()));
    }

    /**
     * @return All tests that do not come between the deps and the dependent test in the expected run,
     *         in an arbitrary order (currently it'sthe order from the original order excluding dependencies,
     *         the dependent test itself, and all tests in between the two)
     */
    private ListEx<String> possibleCleaners(final ListEx<String> originalOrder) {
        return originalOrder.filter(this::possibleCleaner);
    }

    private boolean possibleCleaner(final String testName) {
        final Optional<Integer> idx = new ListEx<>(testOrder).infixIndex(deps);
        final int dtIdx = testOrder.indexOf(dependentTest);

        final int testIdx = testOrder.indexOf(testName);

        return idx.isPresent() && (testIdx > dtIdx || testIdx < idx.get());
    }

    /**
     * If we have historical test run information (that is, we've run the tests before), and we ever see a group
     * of tests that appears to be a cleaner group
     * @return likely cleaner groups
     */
    private Stream<ListEx<String>> highLikelihoodCleanerGroups() {
        try {
            return new TestRunParser(RunnerPathManager.testRuns()).testResults()
                    .mapToStream((output, result) -> result)
                    .flatMap(this::likelyCleanerCandidate);
        } catch (IOException ignored) {}

        return Stream.empty();
    }

    private Stream<ListEx<String>> likelyCleanerCandidate(final TestRunResult testRunResult) {
        return StreamUtil.fromOpt(new ListEx<>(testRunResult.testOrder()).infixIndex(deps))
                .flatMap(depIndex -> {
                    final int dtIdx = testRunResult.testOrder().indexOf(dependentTest);

                    // If the dt comes after the dependencies and yet there result is still unexpected,
                    // this group is a cleaner group (modulo non-determinism)
                    if (dtIdx > depIndex && !testRunResult.results().get(dependentTest).result().equals(expected)) {
                        return Stream.of(new ListEx<>(testRunResult.testOrder().subList(depIndex + deps.size(), dtIdx)));
                    }

                    return Stream.empty();
                });
    }
}
