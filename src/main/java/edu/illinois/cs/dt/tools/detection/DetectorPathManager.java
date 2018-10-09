package edu.illinois.cs.dt.tools.detection;

import edu.illinois.cs.dt.tools.utility.PathManager;

import java.nio.file.Path;
import java.nio.file.Paths;

public class DetectorPathManager extends PathManager {
    public static final Path DETECTION_RESULTS = Paths.get("detection-results");
    public static final Path DT_LIST_PATH  = Paths.get("dt-lists.json");
    public static final Path ORIGINAL_ORDER = Paths.get("original-order");

    public static Path detectionResults() {
        return path(DETECTION_RESULTS);
    }

    public static Path detectionFile() {
        return detectionResults().resolve(DT_LIST_PATH);
    }

    public static Path pathWithRound(final Path path, final String testName, final int round) {
        if (testName == null || testName.isEmpty()) {
            return path.resolve("round" + String.valueOf(round) + ".json");
        } else {
            return path.resolve(testName + "-round" + String.valueOf(round) + ".json");
        }
    }

    public static Path detectionRoundPath(final String name, final int round) {
        return pathWithRound(detectionResults().resolve(name), "", round);
    }

    public static Path filterPath(final String detectorType, final String filterType, final int absoluteRound) {
        return detectionRoundPath(detectorType + "-" + filterType, absoluteRound);
    }

    public static Path originalOrderPath() {
        return path(ORIGINAL_ORDER);
    }
}
