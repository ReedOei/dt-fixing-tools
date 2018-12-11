package edu.illinois.cs.dt.tools.diagnosis;

import edu.illinois.cs.dt.tools.diagnosis.pollution.PollutedField;
import edu.illinois.cs.dt.tools.minimizer.PolluterData;

import java.util.Map;

public class PolluterDiagnosis {
    private final PolluterData polluterData;
    private final Map<String, PollutedField> pollutions;
    private final RewritingResultContainer rewritingResults;

    public PolluterDiagnosis(final PolluterData polluterData,
                             final Map<String, PollutedField> pollutions,
                             final RewritingResultContainer rewritingResults) {
        this.polluterData = polluterData;
        this.pollutions = pollutions;
        this.rewritingResults = rewritingResults;
    }

    public PolluterData polluterData() {
        return polluterData;
    }

    public Map<String, PollutedField> pollutions() {
        return pollutions;
    }

    public RewritingResultContainer rewritingResults() {
        return rewritingResults;
    }
}
