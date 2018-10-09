package edu.illinois.cs.dt.tools.detection;

import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.detection.filters.UniqueFilter;
import edu.illinois.cs.dt.tools.detection.filters.VerifyFilter;

import java.util.List;

public class ReverseDetector extends ExecutingDetector {
    private final List<String> tests;
    private final TestRunResult origResult;
    private final TestShuffler testShuffler;

    public ReverseDetector(final Runner runner, final int rounds, final String name, final List<String> tests) {
        // Always 1 round, because there's only one way to reverse the tests
        super(runner, 1, name);
        this.tests = tests;

        this.origResult = runList(tests);

        testShuffler = new TestShuffler(name, rounds, tests);

        addFilter(new UniqueFilter());
        addFilter(new VerifyFilter(name, runner));
    }

    @Override
    public DetectionRound results() throws Exception {
        final List<String> reversed = testShuffler.shuffledOrder(absoluteRound.get());
        return makeDts(tests, origResult, reversed, runList(reversed));
    }
}
