package edu.illinois.cs.dt.tools.diagnosis;

import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;

import java.util.Map;

public class DiagnosisResult {
    private final MinimizeTestsResult minimized;
    private final Map<String, DiffContainer.Diff> pollutions;
    private final RewritingResultContainer rewritingResults;

    public DiagnosisResult(final MinimizeTestsResult minimized,
                           final Map<String,DiffContainer.Diff> pollutions,
                           final RewritingResultContainer rewritingResults) {
        this.minimized = minimized;
        this.pollutions = pollutions;
        this.rewritingResults = rewritingResults;
    }

    public MinimizeTestsResult minimized() {
        return minimized;
    }

    public Map<String, DiffContainer.Diff> pollutions() {
        return pollutions;
    }

    public RewritingResultContainer rewritingResults() {
        return rewritingResults;
    }
}
