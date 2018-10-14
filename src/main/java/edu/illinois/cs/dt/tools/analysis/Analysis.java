package edu.illinois.cs.dt.tools.analysis;

import com.google.gson.Gson;
import com.opencsv.CSVReader;
import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.util.StandardMain;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.detection.DetectionRound;
import edu.illinois.cs.dt.tools.detection.DetectorPathManager;
import edu.illinois.cs.dt.tools.runner.RunnerPathManager;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.illinois.cs.dt.tools.runner.data.DependentTestList;
import org.apache.commons.io.FilenameUtils;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Analysis extends StandardMain {
    public static int roundNumber(final String filename) {
        // Files are named roundN.json, so strip extension and "round" and we'll have the number
        final String fileName = FilenameUtils.removeExtension(filename);
        return Integer.parseInt(fileName.substring("round".length()));
    }

    // List files and close the directory streams
    private static ListEx<Path> listFiles(final Path path) throws IOException {
        final ListEx<Path> result = new ListEx<>();

        try (final Stream<Path> stream = Files.list(path)) {
            result.addAll(stream.collect(Collectors.toList()));
        }

        return result;
    }

    private final Path results;
    private final SQLite sqlite;
    private int dtListIndex = 0;
    private final int maxTestRuns;


    private Analysis(final String[] args) throws SQLException {
        super(args);

        this.results = Paths.get(getArgRequired("results")).toAbsolutePath();
        this.sqlite = new SQLite(Paths.get(getArgRequired("db")).toAbsolutePath());
        this.maxTestRuns = getArg("max-test-runs").map(Integer::parseInt).orElse(0);
    }

    public static void main(final String[] args) {
        try {
            System.out.println(Arrays.asList(args));
            new Analysis(args).run();

            System.exit(0);
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.exit(1);
    }

    // TODO: copy to eunomia
    public static ListEx<ListEx<String>> csv(final Path path) throws IOException {
        try (final FileInputStream fis = new FileInputStream(path.toAbsolutePath().toString());
             final InputStreamReader isr = new InputStreamReader(fis);
             final CSVReader reader = new CSVReader(isr)) {
            return new ListEx<>(reader.readAll()).map(ListEx::fromArray);
        }
    }

    @Override
    protected void run() throws Exception {
        createTables();

        insertFullSubjectList("popular", Paths.get("scripts").resolve("docker").resolve("data").resolve("popular_150.csv"));
        insertFullSubjectList("deflaker-palomba", Paths.get("scripts").resolve("docker").resolve("data").resolve("new-subj.csv"));

        System.out.println();

        final List<Path> allResultsFolders = Files.walk(results)
                .filter(this::containsResultsFolders)
                .collect(Collectors.toList());

        for (int i = 0; i < allResultsFolders.size(); i++) {
            final Path p = allResultsFolders.get(i);
            System.out.println("[INFO] Inserting results for module " + (i + 1) + " of " + allResultsFolders.size() + ": " + p);
            try {
                insertResults(p);
            } catch (IOException | SQLException e) {
                throw new RuntimeException(e);
            }
        }

        runPostSetup();

        sqlite.save();
    }

    private void insertFullSubjectList(final String source, final Path fullList) throws IOException, SQLException {
        if (!Files.exists(fullList)) {
            throw new FileNotFoundException(fullList.toAbsolutePath().toString());
        }

        System.out.println("[INFO] Inserting " + source + " subject list from: " + fullList);

        try (final FileInputStream fis = new FileInputStream(fullList.toAbsolutePath().toString());
             final InputStreamReader isr = new InputStreamReader(fis);
             final CSVReader reader = new CSVReader(isr)) {

            String[] strings;
            while ((strings = reader.readNext()) != null) {
                if (strings.length == 2) {
                    insertSubjectRaw(source, strings[0], strings[1]);
                }
            }
        }
    }

    private void insertSubjectRaw(final String source, final String url, final String sha) throws MalformedURLException, SQLException {
        // Get the slug. The path starts with a slash, so get rid of it via substring
        final String slug = new URL(url).getPath().substring(1);

        System.out.println("[INFO] Inserting " + url + " with slug " + slug + " and SHA " + sha);
        sqlite.statement(SQLStatements.INSERT_RAW_SUBJECT)
                .param(slug)
                .param(source)
                .param(url)
                .param(sha)
                .executeUpdate();
    }

    private void runPostSetup() throws IOException {
        System.out.println("[INFO] Running post setup queries");
        sqlite.executeFile(SQLStatements.POST_SETUP);
    }

    private boolean containsResultsFolders(final Path p) {
        final Path detectionResults = p.resolve(DetectorPathManager.DETECTION_RESULTS);

        final boolean containsResults = Files.exists(detectionResults) || Files.exists(p.resolve("error"));

        if (!containsResults) {
            return false;
        }

        final Path parent = p.getParent();

        // We only want to run randomizeclasses, not both, because otherwise we'll try to insert some runs twice
        return !parent.getFileName().toString().equals("randomizemethods") ||
               !Files.exists(parent.resolveSibling("randomizeclasses"));
    }

    private void createTables() throws IOException {
        System.out.println("[INFO] Creating tables and views");
        sqlite.executeFile(SQLStatements.CREATE_TABLES);
        System.out.println();
    }

    private void insertResults(final Path path) throws IOException, SQLException {
        final String parent = findParent(path);

        if (parent == null) {
            return;
        }

        final String name = path.getFileName().toString();
        final String slug = parent.substring(0, parent.indexOf('_')).replace('.', '/');

        if (sqlite.checkExists("subject", name)) {
            System.out.println("[INFO] "  + name + " already in the database, skipping.");
            return;
        }

        if (slug.contains("guava")) {
            return;
        }

        insertModuleTestTime(slug, path.resolve(DetectorPathManager.DETECTION_RESULTS).resolve("module-test-time.csv"));

        insertSubject(name, slug, path);

        insertTestRuns(name, path.resolve(RunnerPathManager.TEST_RUNS).resolve("results"));

        insertDetectionResults(name, "flaky", path.resolve(DetectorPathManager.DETECTION_RESULTS).resolve("flaky"));
        insertDetectionResults(name, "random", path.resolve(DetectorPathManager.DETECTION_RESULTS).resolve("random"));
        insertDetectionResults(name, "random-class", path.resolve(DetectorPathManager.DETECTION_RESULTS).resolve("random-class"));
        insertDetectionResults(name, "reverse", path.resolve(DetectorPathManager.DETECTION_RESULTS).resolve("reverse"));
        insertDetectionResults(name, "reverse-class", path.resolve(DetectorPathManager.DETECTION_RESULTS).resolve("reverse-class"));

        insertVerificationResults(name, "random-verify", path.resolve(DetectorPathManager.DETECTION_RESULTS));
        insertVerificationResults(name, "random-class-verify", path.resolve(DetectorPathManager.DETECTION_RESULTS));
        insertVerificationResults(name, "random-confirmation-sampling", path.resolve(DetectorPathManager.DETECTION_RESULTS));
        insertVerificationResults(name, "random-class-confirmation-sampling", path.resolve(DetectorPathManager.DETECTION_RESULTS));

//        sqlite.save();

        System.out.println("[INFO] Finished " + name + " (" + slug + ")");
        System.out.println();
    }

    private void insertModuleTestTime(final String slug, final Path moduleTestTimePath) throws SQLException, IOException {
        if (!Files.exists(moduleTestTimePath)) {
            return;
        }

        System.out.println("[INFO] Inserting module test time for: " + slug);

        if (!sqlite.checkExists("subject", "slug", slug)) {
            final ListEx<ListEx<String>> rows = csv(moduleTestTimePath);

            for (ListEx<String> row : rows) {
                final String coordinates = row.get(0);
                final double time = Double.parseDouble(row.get(1));

                final String[] split = coordinates.split(":");

                sqlite.statement(SQLStatements.INSERT_MODULE_TEST_TIME)
                        .param(coordinates)
                        .param(split[0])
                        .param(split[1])
                        .param(split[2])
                        .param(time)
                        .executeUpdate();
            }
        }
    }

    private String findParent(final Path path) {
        if (path == null || path.getFileName() == null) {
            return null;
        }

        if (path.getFileName().toString().endsWith("_output")) {
            return path.getFileName().toString();
        } else {
            return findParent(path.getParent());
        }
    }

    private void insertSubject(final String name, final String slug, final Path path) throws SQLException, IOException {
        System.out.println("[INFO] Inserting results for " + name + " (" + slug + ")");

        // If the subject does not already exist, insert it
        if (!sqlite.checkExists("subject", name)) {
            sqlite.statement(SQLStatements.INSERT_SUBJECT).param(name).param(slug).executeUpdate();
        }

        if (Files.exists(path.resolve(DetectorPathManager.ORIGINAL_ORDER))) {
            final List<String> originalOrder = Files.readAllLines(path.resolve(DetectorPathManager.ORIGINAL_ORDER));
            System.out.println("[INFO] Inserting original order for " + name + " (" + originalOrder.size() + " tests)");

            final Procedure statement = sqlite.statement(SQLStatements.INSERT_ORIGINAL_ORDER);

            statement.beginTransaction();
            for (int i = 0; i < originalOrder.size(); i++) {
                statement
                    .param(name)
                    .param(originalOrder.get(i))
                    .param(i).addBatch();
            }
            statement.executeBatch();
            statement.commit();
            statement.endTransaction();
        }
    }

    private void insertTestRuns(final String name, final Path testRunResults) throws IOException, SQLException {
        if (!Files.isDirectory(testRunResults)) {
            return;
        }

        final ListEx<Path> paths = listFiles(testRunResults);

        final int limit = maxTestRuns > 0 ? Math.min(maxTestRuns, paths.size()) : paths.size();
        System.out.println("[INFO] Inserting test runs for " + name + " (" + paths.size() + " runs, saving " + limit + ")");

        for (int i = 0; i < limit; i++) {
            final Path p = paths.get(i);
            System.out.print("\r[INFO] Inserting run " + (i + 1) + " of " + paths.size());
            insertTestRunResult(name, new Gson().fromJson(FileUtil.readFile(p), TestRunResult.class));
        }

        System.out.println();
    }

    private void insertTestRunResult(final String name, final TestRunResult testRunResult) throws SQLException {
        if (testRunResult == null) {
            return;
        }

        if (sqlite.checkExists("test_run_result", testRunResult.id())) {
            return;
        }

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

    private void insertDetectionResults(final String name, final String roundType, final Path detectionResults) throws IOException {
        if (!Files.exists(detectionResults)) {
            return;
        }

        final ListEx<Path> paths = listFiles(detectionResults);

        System.out.println("[INFO] Inserting " + roundType + " detection results for " + name
                + " (" + paths.size() + " results)");

        paths.forEach(p -> {
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
        if (round == null) {
            return;
        }

        final int unfilteredId = insertDependentTestList(round.unfilteredTests());
        final int filteredId = insertDependentTestList(round.filteredTests());

        final int detectionRoundId =
                sqlite.statement(SQLStatements.INSERT_DETECTION_ROUND)
                .param(name)
                .param(unfilteredId)
                .param(filteredId)
                .param(roundType)
                .param(roundNumber)
                .param((float) round.roundTime())
                .insertSingleRow();

        // Might occur when using old results
        if (round.testRunIds() != null) {
            for (final String testRunId : round.testRunIds()) {
                sqlite.statement(SQLStatements.INSERT_DETECTION_ROUND_TEST_RUN)
                        .param(detectionRoundId)
                        .param(testRunId)
                        .executeUpdate();
            }
        }
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

    private int insertDependentTest(final DependentTest dependentTest) throws SQLException {
        return sqlite.statement(SQLStatements.INSERT_FLAKY_TEST)
                .param(dependentTest.name())
                .param(dependentTest.intended().testRunId())
                .param(dependentTest.revealed().testRunId())
                .insertSingleRow();
    }

    private void insertVerificationResults(final String name, final String roundType, final Path basePath) throws IOException {
        final Path verificationResults = basePath.resolve(roundType);

        if (!Files.isDirectory(verificationResults)) {
            return;
        }

        final ListEx<Path> paths = listFiles(verificationResults);

        System.out.println("[INFO] Inserting " + roundType + " verification results for " + name + " (" + paths.size() + " rounds)");

        paths.forEach(p -> {
            try {
                insertVerificationRound(name, roundType, roundNumber(p.getFileName().toString()), p);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }

    private void insertVerificationRound(final String name, final String roundType, final int roundNumber, final Path p) throws IOException {
        listFiles(p).forEach(verificationStep -> {
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
                        .param(String.valueOf(testRunResult.results().get(testName).result()))
                        .executeUpdate();

                insertTestRunResult(name, testRunResult);
            } catch (IOException | SQLException e) {
                throw new RuntimeException(e);
            }
        });
    }
}
