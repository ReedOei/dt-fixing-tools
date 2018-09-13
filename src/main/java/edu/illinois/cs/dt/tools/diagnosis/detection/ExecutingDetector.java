package edu.illinois.cs.dt.tools.diagnosis.detection;

import com.google.common.collect.Streams;
import com.reedoei.eunomia.io.VerbosePrinter;
import com.reedoei.eunomia.io.capture.CaptureErrStream;
import com.reedoei.eunomia.io.capture.CaptureOutStream;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.string.StringUtil;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestResult;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.illinois.cs.dt.tools.runner.data.DependentTestList;
import edu.illinois.cs.dt.tools.runner.data.TestRun;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public abstract class ExecutingDetector implements Detector, VerbosePrinter {
    public static final Path DT_LISTS_PATH = Paths.get("dt-lists.json");
    protected Runner runner;

    private int rounds;
    private List<Predicate<DependentTest>> filters = new ArrayList<>();

    public ExecutingDetector(final Runner runner, final int rounds) {
        this.runner = runner;
        this.rounds = rounds;
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

    protected TestRunResult runSilent(final List<String> tests) {
        return new CaptureErrStream<>(() -> new CaptureOutStream<>(() -> runner.runList(tests).get()).run().valueRequired()).run().valueRequired();
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
                            new TestRun(before(intendedOrder, testName), intendedResult.result()),
                            new TestRun(before(revealedOrder, testName), revealedResult)));
                }
            }
        });

        return result;
    }

    public ExecutingDetector addFilter(final Predicate<DependentTest> predicate) {
        filters.add(predicate);

        return this;
    }

    @Override
    public Stream<DependentTest> detect() {
        return Streams.stream(new RunnerIterator());
    }

    private Stream<DependentTest> filter(Stream<DependentTest> dts) {
        for (final Predicate<DependentTest> filter : filters) {
            dts = dts.filter(filter);
        }

        return dts;
    }

    @Override
    public void writeTo(final Path dir) throws IOException {
        FileUtil.makeDirectoryDestructive(dir);

        final Path listPath = dir.resolve("list.txt");
        final Path dtListPath = dir.resolve(DT_LISTS_PATH);

        final DependentTestList dtList = new DependentTestList(detect().collect(Collectors.toList()));
        System.out.println(); // End the progress line.

        print(String.format("[INFO] Found %d tests, writing list to %s and dt lists to %s\n", dtList.size(), listPath, dtListPath));

        Files.write(dtListPath, dtList.toString().getBytes());
        Files.write(listPath, StringUtil.unlines(dtList.names()).getBytes());
    }

    private class RunnerIterator implements Iterator<DependentTest> {
        private final long origStartTimeMs = System.currentTimeMillis();
        private long startTimeMs = System.currentTimeMillis();

        private int i = 0;

        private final List<DependentTest> result = new ArrayList<>();

        @Override
        public boolean hasNext() {
            while (i < rounds && result.isEmpty()) {
                generate();
            }

            return !result.isEmpty();
        }

        public void generate() {
            final List<DependentTest> currentRound;
            try {
                currentRound = filter(results().stream()).collect(Collectors.toList());
            } catch (RuntimeException e) {
                throw e;
            } catch (Exception e) {
                throw new RuntimeException(e);
            }

            final double elapsed = System.currentTimeMillis() - startTimeMs;
            final double totalElapsed = (System.currentTimeMillis() - origStartTimeMs) / 1000.0;
            final long estimate = (long) (elapsed / (i + 1) * (rounds - i - 1) / 1000);

            if (!currentRound.isEmpty()) {
                System.out.print(String.format("\r[INFO] Found %d tests in round %d of %d (%.1f seconds elapsed (%.1f total), %d seconds remaining).\n", currentRound.size(), i + 1, rounds, elapsed / 1000, totalElapsed, estimate));
                result.addAll(currentRound);
                i = 0;
                startTimeMs = System.currentTimeMillis();
            } else {
                System.out.print(String.format("\r[INFO] Found %d tests in round %d of %d (%.1f seconds elapsed (%.1f total), %d seconds remaining)", currentRound.size(), i + 1, rounds, elapsed / 1000, totalElapsed, estimate));
                i++;
            }
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
