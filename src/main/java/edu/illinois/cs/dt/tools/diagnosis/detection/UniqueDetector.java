package edu.illinois.cs.dt.tools.diagnosis.detection;

import com.reedoei.eunomia.collections.ListUtil;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.washington.cs.dt.TestExecResultsDelta;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public abstract class UniqueDetector extends ExecutingDetector {
    private final Set<String> prevTests = new HashSet<>();

    public UniqueDetector(String classpath, int rounds) {
        super(classpath, rounds);
    }

    public List<DependentTest> makeDts(final List<TestExecResultsDelta> deltas) {
        return ListUtil.map(DependentTest::fromDelta, deltas);
    }

    public abstract List<DependentTest> run() throws Exception;

    @Override
    public List<DependentTest> results() throws Exception {
        final List<DependentTest> runResults = removeDuplicates(run());

        runResults.forEach(dt -> prevTests.add(dt.name()));

        return runResults;
    }

    private List<DependentTest> removeDuplicates(final List<DependentTest> deltas) {
        deltas.removeIf(dt -> prevTests.contains(dt.name()));

        return deltas;
    }
}
