package edu.illinois.cs.dt.tools.diagnosis.detection;

import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.collections.RandomList;
import com.reedoei.eunomia.collections.StreamUtil;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.washington.cs.dt.TestExecResult;
import edu.washington.cs.dt.TestExecResults;
import edu.washington.cs.dt.TestExecResultsDifferentior;
import edu.washington.cs.dt.main.Main;

import java.util.List;

public class RandomDetector extends UniqueDetector {
    private final List<String> tests;
    private final TestExecResult origResult;

    public RandomDetector(final String classpath, final int rounds, final List<String> tests) throws Exception {
        super(classpath, rounds);

        this.tests = tests;

        Main.removeredundancy = false;

        System.out.println("[INFO] Getting original results (" + tests.size() + " tests).");
        this.origResult = runner.runOrder(tests).result();

        System.out.println("[INFO] Detecting flaky tests.");
        StreamUtil.seq(new FlakyDetector(classpath, rounds, tests, runner).detect());
        System.out.println();
    }

    @Override
    public SmartTestRunner makeRunner(final String classpath) {
        return new SmartTestRunner(classpath, false);
    }

    @Override
    public List<DependentTest> run() throws Exception {
        final TestExecResults results = runner.runOrder(new RandomList<>(tests).shuffled()).results();
        return removeFlaky(makeDts(new TestExecResultsDifferentior(origResult, results).diffResults()));
    }

    private List<DependentTest> removeFlaky(final List<DependentTest> deltas) {
        return ListUtil.filter(d -> !runner.isFlaky(d.name()), deltas);
    }
}
