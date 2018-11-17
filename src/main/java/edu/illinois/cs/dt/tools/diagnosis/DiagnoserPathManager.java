package edu.illinois.cs.dt.tools.diagnosis;

import edu.illinois.cs.dt.tools.utility.PathManager;

import java.nio.file.Path;
import java.nio.file.Paths;

public class DiagnoserPathManager extends PathManager {
    public static final Path SUBJECT_PROPERTIES = Paths.get("subject.properties");
    public static Path DIAGNOSIS = Paths.get("diagnosis");

    public static Path diagnosisResult(final DiagnosisResult diagnosisResult) {
        return path(DIAGNOSIS).resolve(
                String.format("%s-%s-%s.json",
                        diagnosisResult.minimized().dependentTest(),
                        diagnosisResult.minimized().hash(),
                        diagnosisResult.minimized().expected()));
    }

    public static Path subjectProperties() {
        return path(SUBJECT_PROPERTIES);
    }
}
