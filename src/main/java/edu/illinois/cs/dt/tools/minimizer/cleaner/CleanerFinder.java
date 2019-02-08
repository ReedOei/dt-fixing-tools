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
import edu.illinois.cs.dt.tools.utility.TimeManager;
import org.codehaus.plexus.util.StringUtils;
import scala.util.Try;

import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CleanerFinder {
    private final InstrumentingSmartRunner runner;
    private final String dependentTest;
    private final List<String> deps;
    private final Result expected;
    private final Result isolationResult;
    private final List<String> testOrder;

    // Some fields to help with computing time to first cleaner and outputing in log
    private long startTime;

    // Some fields to help with tracking when it is "desperately" trying all tests
    private int startingTryingEveryTest = -1;
    private int startingTryingEveryTestConfirmed = -1;

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
        TestPluginPlugin.info("Looking for cleaners for: " + dependentTest);

        // If it's empty, we can't have any cleaners (they'd be polluters, not cleaners)
        if (deps.isEmpty()) {
            TestPluginPlugin.info("No dependencies for " + dependentTest + " in this order, so no cleaners.");
            return new CleanerData(dependentTest,
                    expected, isolationResult, new ListEx<>());
        } else {
            final ListEx<String> originalOrder = new ListEx<>(Files.readAllLines(DetectorPathManager.originalOrderPath()));
            this.startTime = System.currentTimeMillis();
            return summarizeCleanerGroups(makeCleanerData(findCleanerGroups(originalOrder)));
        }
    }

    private CleanerData makeCleanerData(final Map<ListEx<String>, TimeManager> cleanerGroupsMap) throws Exception {
        ListEx<ListEx<String>> cleanerGroups = new ListEx<>();
        cleanerGroups.addAll(cleanerGroupsMap.keySet());
        Set<ListEx<String>> seenGroups = new HashSet<>();
        ListEx<CleanerGroup> minimizedCleanerGroups = new ListEx<>();
        for (int i = 0; i < cleanerGroups.size(); i++) {
            ListEx<String> cleanerGroup = cleanerGroups.get(i);
            CleanerGroup minimizedCleanerGroup = minimalCleanerGroup(i, cleanerGroup);
            // Skip any group we have already minimized
            if (seenGroups.contains(minimizedCleanerGroup.cleanerTests())) {
                continue;
            }
            seenGroups.add(minimizedCleanerGroup.cleanerTests());
            if (minimizedCleanerGroup.confirm(runner, new ListEx<>(deps), expected, isolationResult, cleanerGroupsMap.get(cleanerGroup))) {
                minimizedCleanerGroups.add(minimizedCleanerGroup);
                if (i >= this.startingTryingEveryTestConfirmed) {
                    double elapsedSeconds = System.currentTimeMillis() / 1000.0 - startTime / 1000.0;
                    TestPluginPlugin.info("DESPERATELY TRYING CLEANERS INDIVIDUALLY: Found such cleaner " + minimizedCleanerGroup + " for dependent test " + dependentTest + " in " + elapsedSeconds + " seconds.");
                }
            }
        }
        final CleanerData cleanerData = new CleanerData(dependentTest, expected, isolationResult, minimizedCleanerGroups);
        TestPluginPlugin.info(dependentTest + " has " + cleanerData.cleaners().size() + " cleaners: " + cleanerData.cleaners());
        return cleanerData;
    }

    private Map<ListEx<String>, TimeManager> findCleanerGroups(final ListEx<String> originalOrder) throws Exception {
        final TimeManager[] timeToFindCandidates = new TimeManager[1];

        final ListEx<ListEx<String>> candidates =
                OperationTime.runOperation(() ->
                   new ListEx<>(cleanerCandidates(originalOrder)).distinct(),
                       (candidateList, time) -> {
                        timeToFindCandidates[0] = new TimeManager(time, time);
                        return candidateList;
                });
        TestPluginPlugin.info("Found " + candidates.size() + " cleaner group candidates.");

        final Map<ListEx<String>, TimeManager> cleanerGroups = filterCleanerGroups(candidates, timeToFindCandidates[0]);
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

    private Map<ListEx<String>, TimeManager> filterCleanerGroups(final ListEx<ListEx<String>> candidates,
                                                                   final TimeManager findCandidateTime) throws Exception {
        final Map<ListEx<String>, TimeManager> result = new LinkedHashMap<>();

        for (int i = 0; i < candidates.size(); i++) {
            final ListEx<String> candidate = candidates.get(i);
            System.out.printf("\rTrying group %d of %d (found %d so far)", i, candidates.size(), result.size());

            TimeManager[] time = new TimeManager[1];
            boolean isCleanerGroup =
                    OperationTime.runOperation(() -> isCleanerGroup(candidate), (cleanGroupResult, checkTime) -> {
                        time[0] = findCandidateTime.manageTime(checkTime);
                        return cleanGroupResult;
                    }
            );

            if (isCleanerGroup) {
                // Recording if in mode trying every test when checking if is cleaner group
                if (i >= this.startingTryingEveryTest && this.startingTryingEveryTestConfirmed < 0) {
                    this.startingTryingEveryTestConfirmed = result.size();
                }
                result.put(candidate, new TimeManager(time[0]));
                double elapsedSeconds = System.currentTimeMillis() / 1000.0 - startTime / 1000.0;
                // If this is the first one, log out the result
                if (result.size() == 1) {
                    TestPluginPlugin.info("FIRST CLEANER: Found first cleaner " + candidate + " for dependent test " + dependentTest + " in " + elapsedSeconds + " seconds.");
                } else {
                    TestPluginPlugin.info("CLEANER: Found cleaner " + candidate + " for dependent test " + dependentTest + " in " + elapsedSeconds + " seconds.");
                }
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
        return new CleanerGroup(dependentTest, cleanerGroup.size(), deltaDebug(cleanerGroup, 2), i);
    }

    /**
     * @param cleanerGroup The list of tests that is a known, but not necessarily yet a minimal, cleaner group
     * @param n The granularity level at which to chunk up the tests and try to minimize
     * @return A minimal cleanerGroup obtained using delta-debugging
     */
    private ListEx<String> deltaDebug(final ListEx<String> cleanerGroup, int n) {
        // If n granularity is greater than number of tests, then finished, simply return passed in tests
        if (cleanerGroup.size() < n) {
            return cleanerGroup;
        }

        // Cut the tests into n equal chunks and try each chunk
        int chunkSize = (int)Math.round((double)(cleanerGroup.size()) / n);
        List<ListEx<String>> chunks = new ArrayList<>();
        for (int i = 0; i < cleanerGroup.size(); i += chunkSize) {
            ListEx<String> chunk = new ListEx<>();
            ListEx<String> otherChunk = new ListEx<>();
            // Create chunk starting at this iteration
            int endpoint = Math.min(cleanerGroup.size(), i + chunkSize);
            chunk.addAll(cleanerGroup.subList(i, endpoint));

            // Complement chunk are tests before and after this current chunk
            otherChunk.addAll(cleanerGroup.subList(0, i));
            otherChunk.addAll(cleanerGroup.subList(endpoint, cleanerGroup.size()));

            // Check if running this chunk works
            if (isCleanerGroup(chunk)) {
                return deltaDebug(chunk, 2); // If works, then delta debug some more this chunk
            }
            // Otherwise, check if applying complement chunk works
            if (isCleanerGroup(otherChunk)) {
                return deltaDebug(otherChunk, 2);   // If works, then delta debug some more the complement chunk
            }
        }
        // If not chunk/complement work, increase granularity and try again
        return deltaDebug(cleanerGroup, n * 2);
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

        // Get the number of elements done before considering every test as a possible cleaner
        Stream<ListEx<String>> intermediate = Stream.concat(highLikelihoodCleanerGroups(), Stream.of(possibleCleaners(originalOrder)));
        this.startingTryingEveryTest = intermediate.collect(Collectors.toList()).size();

        return Stream.concat(
                Stream.concat(highLikelihoodCleanerGroups(), Stream.of(possibleCleaners(originalOrder))),
                // Consider each test as a possible cleaner
                originalOrder.stream().map(ListEx::fromArray));
//                // Consider each possible cleaner individually as well, in case there are other polluters
//                Stream.of(possibleCleaners(originalOrder)).flatMap(l -> l.map(ListEx::fromArray).stream()));
    }

    /**
     * @return All tests that do not come between the deps and the dependent test in the expected run,
     *         in an arbitrary order (currently it's the order from the original order excluding dependencies,
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
