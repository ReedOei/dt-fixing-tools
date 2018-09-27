package edu.illinois.cs.dt.tools.runner;

import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.utility.PathManager;
import org.apache.commons.io.FileUtils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class RunnerPathManager extends PathManager {
    public static Path runResultPath(final TestRunResult testRunResult, final String modifier) {
        return path(Paths.get("test-runs").resolve(modifier).resolve(testRunResult.id()));
    }

    public static void outputResult(final Path tempOutput, final TestRunResult testRunResult) throws Exception {
        final Path outputPath = runResultPath(testRunResult, "output");
        final Path resultPath = runResultPath(testRunResult, "results");

        Files.createDirectories(outputPath.getParent());
        Files.move(tempOutput, outputPath);

        Files.createDirectories(resultPath.getParent());
        Files.write(resultPath, testRunResult.toString().getBytes());
    }

    public static void clearTestRuns() throws IOException {
        FileUtils.deleteDirectory(path(Paths.get("test-runs")).toFile());
    }
}
