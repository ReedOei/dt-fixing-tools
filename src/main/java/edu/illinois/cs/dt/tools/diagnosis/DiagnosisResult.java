package edu.illinois.cs.dt.tools.diagnosis;

import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;

import java.util.List;

public class DiagnosisResult {
    private final MinimizeTestsResult minimized;
    private final List<PolluterDiagnosis> diagnoses;

    public DiagnosisResult(final MinimizeTestsResult minimized,
                           final List<PolluterDiagnosis> diagnoses) {
        this.minimized = minimized;
        this.diagnoses = diagnoses;
    }

    public MinimizeTestsResult minimized() {
        return minimized;
    }

    public List<PolluterDiagnosis> diagnoses() {
        return diagnoses;
    }
}
