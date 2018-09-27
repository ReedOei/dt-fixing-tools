package edu.illinois.cs.dt.tools.diagnosis.pollution;

import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.minimizer.MinimizerPathManager;
import edu.illinois.cs.dt.tools.utility.PathManager;

import java.nio.file.Path;
import java.nio.file.Paths;

public class PollutionPathManager extends PathManager {
    public static final Path POLLUTION_DATA = Paths.get("pollution-data");

    public static Path pollutionData() {
        return path(POLLUTION_DATA);
    }

    public static Path pollutionData(final Path relative) {
        return pollutionData().resolve(relative);
    }

    public static Path pollutionData(final MinimizeTestsResult minimized) {
        return pollutionData(Paths.get(minimized.dependentTest() + "-" + minimized.expected() + ".xml"));
    }

    public static Path pollutionData(final MinimizeTestsResult minimized, final String modifier) {
        return pollutionData(MinimizerPathManager.minimizeResultsPath(minimized, modifier));
    }
}
