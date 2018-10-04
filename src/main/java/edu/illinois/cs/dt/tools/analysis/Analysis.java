package edu.illinois.cs.dt.tools.analysis;

import com.google.gson.Gson;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.util.StandardMain;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestResult;
import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.detection.DetectionRound;
import edu.illinois.cs.dt.tools.detection.DetectorPathManager;
import edu.illinois.cs.dt.tools.runner.RunnerPathManager;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.illinois.cs.dt.tools.runner.data.DependentTestList;
import org.apache.commons.io.FilenameUtils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.concurrent.atomic.AtomicInteger;

public class Analysis extends StandardMain {
    private final Path results;
    private SQLite sqlite;
    private int dtListIndex = 0;

    private Analysis(final String[] args) throws SQLException {
        super(args);

        this.results = Paths.get(getArgRequired("results")).toAbsolutePath();
        final Path db = Paths.get(getArgRequired("db")).toAbsolutePath();

        final Connection connection = DriverManager.getConnection("jdbc:sqlite:" + db);

        this.sqlite = new SQLite(connection);
    }

    public static void main(final String[] args) {
        try {
            new Analysis(args).run();

            System.exit(0);
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.exit(1);
    }

    @Override
    protected void run() throws Exception {
        createTables();

        Files.walk(results)
                .filter(p -> Files.isDirectory(p.resolve(DetectorPathManager.DETECTION_RESULTS)) &&
                             Files.isDirectory(p.resolve(RunnerPathManager.TEST_RUNS)))
                .forEach(p -> {
                    try {
                        insertResults(p);
                    } catch (IOException | SQLException e) {
                        throw new RuntimeException(e);
                    }
                });
    }

    private void createTables() throws IOException {
        System.out.println("[INFO] Creating tables");

        sqlite.statements(SQLStatements.CREATE_TABLES).forEach(ps -> {
            try {
                ps.execute();
            } catch (SQLException e) {
                throw new RuntimeException(e);
            }
        });

        System.out.println();
    }

    private void insertResults(final Path path) throws IOException, SQLException {
        final String parent = path.getParent().getFileName().toString();

        final String name = path.getFileName().toString();
        final String slug = parent.substring(0, parent.indexOf('-')).replace('.', '/');

        insertSubject(name, slug);
        insertTestRuns(name, path.resolve(RunnerPathManager.TEST_RUNS).resolve("results"));
        insertDetectionResults(name, "flaky", path.resolve(DetectorPathManager.DETECTION_RESULTS).resolve("flaky"));
        insertDetectionResults(name, "random", path.resolve(DetectorPathManager.DETECTION_RESULTS).resolve("random"));
        insertVerificationResults(name, "random-verify", path.resolve(DetectorPathManager.DETECTION_RESULTS).resolve("random-verify"));

        System.out.println("[INFO] Finished " + name + " (" + slug + ")");
        System.out.println();
    }

    private void insertSubject(final String name, final String slug) throws IOException, SQLException {
        System.out.println("[INFO] Inserting results for " + name + " (" + slug + ")");

        sqlite.statement(SQLStatements.INSERT_SUBJECT).param(name).param(slug).executeUpdate();
    }

    private void insertTestRuns(final String name, final Path testRunResults) throws IOException {
        final int count = Math.toIntExact(Files.list(testRunResults).count());
        System.out.println("[INFO] Inserting test runs for " + name + " (" + count + " runs)");

        final AtomicInteger i = new AtomicInteger(1);
        Files.list(testRunResults).forEach(p -> {
            try {
                System.out.print("\r[INFO] Inserting run " + i + " of " + count);
                insertTestRunResult(name, new Gson().fromJson(FileUtil.readFile(p), TestRunResult.class));
                i.getAndIncrement();
            } catch (IOException | SQLException e) {
                throw new RuntimeException(e);
            }
        });

        System.out.println();
    }

    private void insertTestRunResult(final String name, final TestRunResult testRunResult) throws IOException, SQLException {
        sqlite.statement(SQLStatements.INSERT_TEST_RUN_RESULT)
                .param(name)
                .param(testRunResult.id())
                .param(0)
                .executeUpdate();

        final Procedure statement = sqlite.statement(SQLStatements.INSERT_TEST_RESULT);

        statement.beginTransaction();

        final AtomicInteger count = new AtomicInteger();
        testRunResult.results().forEach((testName, testResult) -> {
            try {
                statement.param(testRunResult.id())
                        .param(testRunResult.testOrder().indexOf(testResult.name()))
                        .param(testResult.name())
                        .param((float) testResult.time())
                        .param(String.valueOf(testResult.result()))
                        .addBatch();
                count.incrementAndGet();
            } catch (SQLException e) {
                throw new RuntimeException(e);
            }
        });

        statement.executeBatch();

        statement.commit();
        statement.endTransaction();

        sqlite.statement(SQLStatements.UPDATE_TEST_RUN_RESULT_COUNT)
                .param(count.get())
                .param(testRunResult.id())
                .executeUpdate();
    }

    private void insertTestResult(final String id, final int orderIndex, final TestResult testResult) throws IOException, SQLException {
        sqlite.statement(SQLStatements.INSERT_TEST_RESULT)
                .executeUpdate();
    }

    private int roundNumber(final String filename) {
        // Files are named roundN.json, so strip extension and "round" and we'll have the number
        final String fileName = FilenameUtils.removeExtension(filename);
        return Integer.parseInt(fileName.substring("round".length()));
    }

    private void insertDetectionResults(final String name, final String roundType, final Path detectionResults) throws IOException {
        final int count = Math.toIntExact(Files.list(detectionResults).count());
        System.out.println("[INFO] Inserting " + roundType + " detection results for " + name
                + " (" + count + " results)");

        Files.list(detectionResults).forEach(p -> {
            final int roundNumber = roundNumber(p.getFileName().toString());

            try {
                final DetectionRound round = new Gson().fromJson(FileUtil.readFile(p), DetectionRound.class);
                insertDetectionRound(name, roundType, roundNumber, round);
            } catch (IOException | SQLException e) {
                throw new RuntimeException(e);
            }
        });
    }

    private void insertDetectionRound(final String name, final String roundType,
                                       final int roundNumber, final DetectionRound round)
            throws IOException, SQLException {
        final int unfilteredId = insertDependentTestList(round.unfilteredTests());
        final int filteredId = insertDependentTestList(round.filteredTests());

        sqlite.statement(SQLStatements.INSERT_DETECTION_ROUND)
                .param(name)
                .param(unfilteredId)
                .param(filteredId)
                .param(roundType)
                .param(roundNumber)
                .param((float) round.roundTime())
                .executeUpdate();
    }

    private int insertDependentTestList(final DependentTestList dependentTestList) throws IOException, SQLException {
        final int index = dtListIndex;
        dtListIndex++;

        for (DependentTest dependentTest : dependentTestList.dts()) {
            final int dependentTestId = insertDependentTest(dependentTest);

            sqlite.statement(SQLStatements.INSERT_FLAKY_TEST_LIST)
                    .param(index)
                    .param(dependentTestId)
                    .executeUpdate();
        }

        return index;
    }

    private int insertDependentTest(final DependentTest dependentTest) throws IOException, SQLException {
        return sqlite.statement(SQLStatements.INSERT_FLAKY_TEST)
                .param(dependentTest.name())
                .param(dependentTest.intended().testRunId())
                .param(dependentTest.revealed().testRunId())
                .insertSingleRow();
    }

    private void insertVerificationResults(final String name, final String roundType, final Path verificationResults) throws IOException {
        if (!Files.isDirectory(verificationResults)) {
            return;
        }

        final int count = Math.toIntExact(Files.list(verificationResults).count());
        System.out.println("[INFO] Inserting " + roundType + " verification results for " + name + " (" + count + " rounds)");

        Files.list(verificationResults).forEach(p -> {
            try {
                insertVerificationRound(name, roundType, roundNumber(p.getFileName().toString()), p);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }

    private void insertVerificationRound(final String name, final String roundType, final int roundNumber, final Path p) throws IOException {
        Files.list(p).forEach(verificationStep -> {
            final String filename = verificationStep.getFileName().toString();
            final String[] split = filename.split("-");

            final String testName = split[0];
            final Result result = Result.valueOf(split[1]);
            final int verificationRoundNumber = roundNumber(split[2]);

            try {
                final TestRunResult testRunResult = new Gson().fromJson(FileUtil.readFile(verificationStep), TestRunResult.class);

                sqlite.statement(SQLStatements.INSERT_VERIFICATION_ROUND)
                        .param(name)
                        .param(roundNumber)
                        .param(testRunResult.id())
                        .param(roundType)
                        .param(verificationRoundNumber)
                        .param(testName)
                        .param(String.valueOf(result))
                        .executeUpdate();
            } catch (IOException | SQLException e) {
                throw new RuntimeException(e);
            }
        });
    }
}
