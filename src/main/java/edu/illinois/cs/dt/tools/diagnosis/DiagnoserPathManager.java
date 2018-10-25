package edu.illinois.cs.dt.tools.diagnosis;

import edu.illinois.cs.dt.tools.utility.PathManager;

import java.nio.file.Path;
import java.nio.file.Paths;

public class DiagnoserPathManager extends PathManager {
    public static Path DIAGNOSIS = Paths.get("diagnosis");

    public static Path diagnosisResult(final DiagnosisResult diagnosisResult) {
        return path(DIAGNOSIS).resolve(diagnosisResult.minimized().dependentTest() + "-" + diagnosisResult.minimized().expected() + ".json");
    }
}
