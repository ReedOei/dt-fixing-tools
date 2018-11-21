package edu.illinois.cs.dt.tools.minimizer;

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
}
