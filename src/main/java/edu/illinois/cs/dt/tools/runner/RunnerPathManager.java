package edu.illinois.cs.dt.tools.runner;

import com.google.gson.Gson;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.utility.PathManager;
import org.apache.commons.io.FileUtils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class RunnerPathManager extends PathManager {
    public static final Path TEST_RUNS = Paths.get("test-runs");

    public static Path runResultPath(final String testRunResultId, final String modifier) {
        return path(TEST_RUNS.resolve(modifier).resolve(testRunResultId));
    }

    public static Path runResultPath(final TestRunResult testRunResult, final String modifier) {
        return runResultPath(testRunResult.id(), modifier);
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

    public static Stream<TestRunResult> resultFor(final String trKey) {
        try {
            return Stream.of(new Gson().fromJson(FileUtil.readFile(runResultPath(trKey, "results")), TestRunResult.class));
        } catch (IOException ignored) {}

        return Stream.empty();
    }
}
