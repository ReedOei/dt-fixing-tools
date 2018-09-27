package edu.illinois.cs.dt.tools.detection;

import com.google.common.collect.Streams;
import com.google.gson.Gson;
import com.reedoei.eunomia.io.VerbosePrinter;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.string.StringUtil;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestResult;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.detection.filters.Filter;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.illinois.cs.dt.tools.runner.data.DependentTestList;
import edu.illinois.cs.dt.tools.runner.data.TestRun;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public abstract class ExecutingDetector implements Detector, VerbosePrinter {
    protected Runner runner;

    protected int rounds;
    private List<Filter> filters = new ArrayList<>();
    protected final String name;

    public ExecutingDetector(final Runner runner, final int rounds, final String name) {
        this.runner = runner;
        this.rounds = rounds;
        this.name = name;
    }

    public abstract List<DependentTest> results() throws Exception;

    public static <T> List<T> before(final List<T> ts, final T t) {
        final int i = ts.indexOf(t);

        if (i != -1) {
            return new ArrayList<>(ts.subList(0, Math.min(ts.size(), i)));
        } else {
            return new ArrayList<>();
        }
    }

    protected TestRunResult runList(final List<String> tests) {
//        return new CaptureErrStream<>(() -> new CaptureOutStream<>(() -> runner.runList(tests).get()).run().valueRequired()).run().valueRequired();
        return runner.runList(tests).get();
    }

    public List<DependentTest> makeDts(final List<String> intendedOrder, final TestRunResult intended,
                                       final List<String> revealedOrder, final TestRunResult revealed) {
        final List<DependentTest> result = new ArrayList<>();

        intended.results().forEach((testName, intendedResult) -> {
            final Map<String, TestResult> revealedResults = revealed.results();

            if (revealedResults.containsKey(testName)) {
                final Result revealedResult = revealedResults.get(testName).result();
                if (!revealedResult.equals(intendedResult.result())) {
                    result.add(new DependentTest(testName,
                            new TestRun(before(intendedOrder, testName), intendedResult.result(), intended.id()),
                            new TestRun(before(revealedOrder, testName), revealedResult, revealed.id())));
                }
            }
        });

        return result;
    }

    public ExecutingDetector addFilter(final Filter filter) {
        filters.add(filter);

        return this;
    }

    @Override
    public Stream<DependentTest> detect() {
        return Streams.stream(new RunnerIterator());
    }

    private Stream<DependentTest> filter(Stream<DependentTest> dts, final int absoluteRound) {
        for (final Filter filter : filters) {
            dts = dts.filter(t -> filter.keep(t, absoluteRound));
        }

        return dts;
    }

    @Override
    public void writeTo(final Path dir) throws IOException {
        FileUtil.makeDirectoryDestructive(dir);

        final Path listPath = dir.resolve("list.txt");
        final Path dtListPath = dir.resolve(DetectorPathManager.DT_LIST_PATH);

        final DependentTestList dtList = new DependentTestList(detect().collect(Collectors.toList()));
        System.out.println(); // End the progress line.

        print(String.format("[INFO] Found %d tests, writing list to %s and dt lists to %s\n", dtList.size(), listPath, dtListPath));

        Files.write(dtListPath, dtList.toString().getBytes());
        Files.write(listPath, StringUtil.unlines(dtList.names()).getBytes());
    }

    private class RunnerIterator implements Iterator<DependentTest> {
        private final long origStartTimeMs = System.currentTimeMillis();
        private long startTimeMs = System.currentTimeMillis();
        private long previousStopTimeMs = System.currentTimeMillis();

        private int i = 0;
        private int absoluteI = 0; // This will not be reset ever, so we will have unique file names for rounds

        private final List<DependentTest> result = new ArrayList<>();

        @Override
        public boolean hasNext() {
            while (i < rounds && result.isEmpty()) {
                generate();
            }

            return !result.isEmpty();
        }

        private DetectionRound generateDetectionRound(final int absoluteI) {
            final Path path = DetectorPathManager.detectionRoundPath(name, absoluteI);

            // Load it if possible
            try {
                if (Files.exists(path)) {
                    return new Gson().fromJson(FileUtil.readFile(path), DetectionRound.class);
                }
            } catch (IOException ignored) {}

            // Otherwise run the detection round
            final List<DependentTest> unfiltered;
            final List<DependentTest> currentRound;
            try {
                unfiltered = results();
                currentRound = filter(unfiltered.stream(), absoluteI).collect(Collectors.toList());
            } catch (RuntimeException e) {
                throw e;
            } catch (Exception e) {
                throw new RuntimeException(e);
            }

            final long stopTime = System.currentTimeMillis();

            final DetectionRound result = new DetectionRound(unfiltered, currentRound, (stopTime - previousStopTimeMs) / 1000.0);

            try {
                Files.createDirectories(path.getParent());
                Files.write(path, result.toString().getBytes());
            } catch (IOException ignored) {}

            previousStopTimeMs = stopTime;

            return result;
        }

        public void generate() {
            final DetectionRound round = generateDetectionRound(absoluteI);

            final double elapsed = previousStopTimeMs - startTimeMs;
            final double totalElapsed = (System.currentTimeMillis() - origStartTimeMs) / 1000.0;
            final double estimate = elapsed / (i + 1) * (rounds - i - 1) / 1000;

            if (!round.filteredTests().dts().isEmpty()) {
                System.out.print(String.format("\r[INFO] Found %d tests in round %d of %d (%.1f seconds elapsed (%.1f total), %.1f seconds remaining).\n",
                        round.filteredTests().size(), i + 1, rounds, elapsed / 1000, totalElapsed, estimate));
                result.addAll(round.filteredTests().dts());
                i = 0;
                startTimeMs = System.currentTimeMillis();
            } else {
                System.out.print(String.format("\r[INFO] Found %d tests in round %d of %d (%.1f seconds elapsed (%.1f total), %.1f seconds remaining)",
                        round.filteredTests().size(), i + 1, rounds, elapsed / 1000, totalElapsed, estimate));
                i++;
            }

            absoluteI++;
        }

        @Override
        public DependentTest next() {
            if (hasNext()) {
                return result.remove(0);
            } else {
                return null;
            }
        }
    }
}
