package edu.illinois.cs.dt.tools.minimizer;

import java.util.ArrayList;
import java.util.List;

import edu.illinois.cs.dt.tools.minimizer.cleaner.CleanerData;

public class PolluterData {
    private final List<String> deps;
    private final CleanerData cleanerData;

    public PolluterData(final List<String> deps, final CleanerData cleanerData) {
        this.deps = deps;
        this.cleanerData = cleanerData;
    }

    public List<String> deps() {
        return deps;
    }

    public CleanerData cleanerData() {
        return cleanerData;
    }

    public List<String> withDeps(final String dependentTest) {
        final List<String> order = new ArrayList<>(deps);
        if (!order.contains(dependentTest)) {
            order.add(dependentTest);
        }
        return order;
    }
}
