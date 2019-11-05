package edu.illinois.cs.dt.tools.detection.detectors;

import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.detection.DetectionRound;
import edu.illinois.cs.dt.tools.detection.DetectorUtil;
import edu.illinois.cs.dt.tools.detection.TestShuffler;
import edu.illinois.cs.dt.tools.detection.filters.ConfirmationFilter;
import edu.illinois.cs.dt.tools.detection.filters.UniqueFilter;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;

import java.util.List;

public class RandomDetector extends ExecutingDetector {
    private final List<String> tests;
    private TestRunResult origResult;

    private final TestShuffler testShuffler;

    public RandomDetector(final String type, final Runner runner, final int rounds, final List<String> tests) {
        super(runner, rounds, type);

        this.origResult = DetectorUtil.originalResults(tests, runner);
        this.tests = origResult.testOrder();
        this.testShuffler = new TestShuffler(type, rounds, this.tests);

        // Filters to be applied in order
        if (runner instanceof InstrumentingSmartRunner) {
            addFilter(new ConfirmationFilter(name, this.tests, (InstrumentingSmartRunner) runner));
        } else {
            addFilter(new ConfirmationFilter(name, this.tests, InstrumentingSmartRunner.fromRunner(runner)));
        }

        addFilter(new UniqueFilter());
    }

    @Override
    public DetectionRound results() throws Exception {
        final List<String> fullTestOrder = testShuffler.shuffledOrder(absoluteRound.get());

        return makeDts(origResult, runList(fullTestOrder));
    }
}
