package edu.illinois.cs.dt.tools.diagnosis.detection;

import com.reedoei.eunomia.collections.ListUtil;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.washington.cs.dt.TestExecResultsDelta;

import java.util.ArrayList;
import java.util.List;

public abstract class UniqueDetector extends ExecutingDetector {
    private final List<DependentTest> prevDeltas = new ArrayList<>();

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

        prevDeltas.addAll(runResults);

        return runResults;
    }

    private List<DependentTest> removeDuplicates(final List<DependentTest> deltas) {
        deltas.removeIf(t -> {
            for (final DependentTest dt : prevDeltas) {
                if (dt.name().equals(t.name())) {
                    return true;
                }
            }

            return false;
        });

        return deltas;
    }
}
