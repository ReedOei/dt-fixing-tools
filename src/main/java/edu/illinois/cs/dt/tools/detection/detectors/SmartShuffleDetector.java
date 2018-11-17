package edu.illinois.cs.dt.tools.detection.detectors;

import com.reedoei.eunomia.collections.RandomList;
import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.detection.DetectionRound;
import edu.illinois.cs.dt.tools.detection.DetectorUtil;
import edu.illinois.cs.dt.tools.detection.filters.ConfirmationFilter;
import edu.illinois.cs.dt.tools.detection.filters.UniqueFilter;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class SmartShuffleDetector extends ExecutingDetector {
    private final RandomList<String> toComeFirst = new RandomList<>();
    private final RandomList<String> toComeLast = new RandomList<>();

    private final InstrumentingSmartRunner runner;
    private final int rounds;
    private final RandomList<String> tests;
    private final List<String> originalOrder;
    private final String type;
    private final TestRunResult originalResults;

    public SmartShuffleDetector(final InstrumentingSmartRunner runner,
                                final int rounds, final List<String> tests,
                                final String type) {
        super(runner, rounds, type);
        this.runner = runner;
        this.rounds = rounds;
        this.tests = new RandomList<>(tests);
        this.type = type;
        this.originalOrder = tests;

        toComeFirst.addAll(tests);
        toComeLast.addAll(tests);

        originalResults = DetectorUtil.originalResults(originalOrder, runner);

        addFilter(new ConfirmationFilter(type, tests, runner));
        addFilter(new UniqueFilter());
    }

    @Override
    public DetectionRound results() throws Exception {
        final String first = sample(toComeFirst);
        final String last = sample(toComeLast, first);

        toComeFirst.remove(first);
        toComeLast.remove(last);

        final RandomList<String> order = new RandomList<>(tests);
        order.remove(first);
        order.remove(last);
        order.shuffleThis();
        order.add(0, first);
        order.add(last);

        return makeDts(originalOrder, originalResults, order, runList(order));
    }

    private String sample(final RandomList<String> from, final String... excluding) {
        return sample(from, Arrays.stream(excluding).collect(Collectors.toSet()));
    }

    private String sample(final RandomList<String> from, final Set<String> excluding) {
        for (final String s : from.shuffled()) {
            if (!excluding.contains(s)) {
                return s;
            }
        }

        return sample(tests, excluding);
    }
}
