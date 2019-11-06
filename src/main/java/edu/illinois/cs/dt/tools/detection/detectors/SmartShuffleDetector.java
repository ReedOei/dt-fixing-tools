package edu.illinois.cs.dt.tools.detection.detectors;

import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.detection.DetectionRound;
import edu.illinois.cs.dt.tools.detection.DetectorUtil;
import edu.illinois.cs.dt.tools.detection.SmartShuffler;
import edu.illinois.cs.dt.tools.detection.filters.ConfirmationFilter;
import edu.illinois.cs.dt.tools.detection.filters.UniqueFilter;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;

import java.util.List;

public class SmartShuffleDetector extends ExecutingDetector {
    private final List<String> originalOrder;
    private final TestRunResult originalResults;

    private final SmartShuffler shuffler;

    public SmartShuffleDetector(final InstrumentingSmartRunner runner,
                                final int rounds, final List<String> tests,
                                final String type) {
        super(runner, rounds, type);

        this.originalResults = DetectorUtil.originalResults(tests, runner);
        this.originalOrder = originalResults.testOrder();
        this.shuffler = new SmartShuffler(this.originalOrder);

        addFilter(new ConfirmationFilter(type, this.originalOrder, runner));
        addFilter(new UniqueFilter());
    }

    @Override
    public DetectionRound results() throws Exception {
        final List<String> order = shuffler.nextOrder();

        return makeDts(originalResults, runList(order));
    }
}
