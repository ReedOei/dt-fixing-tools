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
import edu.illinois.cs.dt.tools.detection.NoPassingOrderException;
import edu.illinois.cs.dt.tools.diagnosis.DiagnoserPathManager;
import edu.illinois.cs.dt.tools.diagnosis.DiagnosisResult;
import edu.illinois.cs.dt.tools.diagnosis.PolluterDiagnosis;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticAccessInfo;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticFieldPathManager;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticTracer;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.TracerMode;
import edu.illinois.cs.dt.tools.diagnosis.pollution.PollutedField;
import edu.illinois.cs.dt.tools.diagnosis.pollution.PollutionPathManager;
import edu.illinois.cs.dt.tools.diagnosis.rewrite.FieldDiff;
import edu.illinois.cs.dt.tools.diagnosis.rewrite.RewriteTarget;
import edu.illinois.cs.dt.tools.diagnosis.rewrite.RewritingResult;
import edu.illinois.cs.dt.tools.fixer.CleanerFixerPlugin;
import edu.illinois.cs.dt.tools.fixer.CleanerPathManager;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.minimizer.MinimizerPathManager;
import edu.illinois.cs.dt.tools.minimizer.PolluterData;
import edu.illinois.cs.dt.tools.minimizer.cleaner.CleanerData;
import edu.illinois.cs.dt.tools.minimizer.cleaner.CleanerGroup;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.illinois.cs.dt.tools.runner.data.DependentTestList;
import edu.illinois.cs.dt.tools.runner.data.TestResult;
import edu.illinois.cs.dt.tools.utility.OperationTime;
import edu.illinois.cs.dt.tools.utility.TimeManager;
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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// TODO: would probably be better to have these insert methods in their respective classes with some
//       interface or something...
public class Analysis extends StandardMain {
    public static final Pattern PRIOR_PATCH_PATTERN = Pattern.compile("PRIOR PATCH FIXED \\(DEPENDENT=([^,]+),CLEANER=([^,]+),MODIFIED=([^,]+)\\)");

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
    private final Path subjectList;
    private final Path subjectListLOC;
    private final Set<Path> filesAdded;

    private Analysis(final String[] args) throws SQLException {
        super(args);

        this.results = Paths.get(getArgRequired("results")).toAbsolutePath();
        this.sqlite = new SQLite(Paths.get(getArgRequired("db")).toAbsolutePath());
        this.subjectList = Paths.get(getArgRequired("subjectList")).toAbsolutePath();
        this.subjectListLOC = Paths.get(getArgRequired("subjectListLoc")).toAbsolutePath();
        this.maxTestRuns = getArg("max-test-runs").map(Integer::parseInt).orElse(0);
        filesAdded = new HashSet<>();
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

        Files.find(results,
                   Integer.MAX_VALUE,
                   (filePath, fileAttr) -> fileAttr.isDirectory() && filePath.getFileName().toString().endsWith("_output")
                        && !filePath.equals(results))
                .forEach(parent -> {
                    try {
                        insertFSExperiment(parent);
                    } catch (SQLException e) {
                        throw new RuntimeException(e);
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                });

        insertNOTests(results.resolve("manual-data/no-tests"));
        insertPRTests(results.resolve("manual-data/pr-tests"));
        insertFixMethodOrderTests(results.resolve("manual-data/fixed-method-order-tests"));
        insertIncompatibleTests(results.resolve("manual-data/incompatible-tests"));
        insertUnfinishedTests(results.resolve("manual-data/unfinished-tests"));
        insertSeparateJVMTests(results.resolve("manual-data/separate-jvm-tests"));

        insertFSSubjTestRaw(results.resolve("manual-data/fs-subj-test-raw.csv"));
        insertFSCommitOrderTests(results.resolve("manual-data/fs-test-to-first-sha.csv"));

        insertFullSubjectList(subjectList);
        insertSubjectLOC(subjectListLOC);

        System.out.println();

        final List<Path> allResultsFolders = new ArrayList<>();
        Files.walkFileTree(results, new ResultDirVisitor(allResultsFolders));

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

    private void insertNOTests(final Path path) throws SQLException, IOException {
        for (final String testName : Files.readAllLines(path)) {
            sqlite.statement(SQLStatements.INSERT_NO_TEST)
                    .param(testName)
                    .insertSingleRow();
        }
    }

    private void insertFSSubjTestRaw(final Path path) throws SQLException, IOException {
        // File originates from combining data/icst-dataset/only-flaky/individual_split/split_by_test/comprehensive.csv
        // and data/icst-dataset/only-flaky/individual_split/split_by_test/extended.csv
        for (final String line : Files.readAllLines(path)) {
            String[] lineArr = line.split(",");

            if (lineArr.length != 5) {
                continue;
            }

            String url = lineArr[0];
            final String slug = new URL(url).getPath().substring(1);
            String commitSha = lineArr[1];
            String testName = lineArr[2];
            String module = lineArr[3];
            String dataset = lineArr[4];

            sqlite.statement(SQLStatements.INSERT_FS_SUBJ_TEST_RAW)
                    .param(slug)
                    .param(commitSha)
                    .param(testName)
                    .param(module)
                    .param(dataset)
                    .insertSingleRow();

            insertCommitOrderTestsHelp(testName, commitSha, "-1");
        }
    }

    private void insertFSCommitOrderTests(final Path path) throws SQLException, IOException {
        // File originates from docker/create_first_commit_csvs.sh
        for (final String line : Files.readAllLines(path)) {
            String[] lineArr = line.split(",");

            if (lineArr.length != 3) {
                continue;
            }

            String testName = lineArr[0];
            String commitSha = lineArr[1];
            String orderNum = lineArr[2];

            insertCommitOrderTestsHelp(testName, commitSha, orderNum);
        }
    }

    private void insertCommitOrderTestsHelp(final String testName, final String commitSha, final String orderNum) throws SQLException, IOException {
        // orderNum: Number of commits inbetween commitSha and the iDFlakies sha; -1 means this is the iDFlakies sha
        final String shortSha = commitSha.substring(0,7);
        sqlite.statement(SQLStatements.INSERT_FS_TESTS_COMMIT_NUM)
                .param(testName)
                .param(commitSha)
                .param(orderNum)
                .param(shortSha)
                .insertSingleRow();
    }

    private void insertPRTests(final Path path) throws SQLException, IOException {
        for (final String line : Files.readAllLines(path)) {
            String[] lineArr = line.split(",");

            if (lineArr.length != 4) {
                continue;
            }

            String subjName = lineArr[0];
            String testName = lineArr[1];
            String pr_status = lineArr[2];
            String pr_link = lineArr[3];

            sqlite.statement(SQLStatements.INSERT_PR_TESTS)
                    .param(subjName)
                    .param(testName)
                    .param(pr_status)
                    .param(pr_link)
                    .insertSingleRow();
        }
    }

    private void insertFixMethodOrderTests(final Path path) throws SQLException, IOException {
        for (final String testName : Files.readAllLines(path)) {
            sqlite.statement(SQLStatements.INSERT_FIX_METHOD_ORDER_TESTS)
                    .param(testName)
                    .insertSingleRow();
        }
    }

    private void insertIncompatibleTests(final Path path) throws SQLException, IOException {
        for (final String testName : Files.readAllLines(path)) {
            sqlite.statement(SQLStatements.INSERT_INCOMPATIBLE_TESTS)
                    .param(testName)
                    .insertSingleRow();
        }
    }

    private void insertUnfinishedTests(final Path path) throws SQLException, IOException {
        for (final String testName : Files.readAllLines(path)) {
            sqlite.statement(SQLStatements.INSERT_UNFINISHED_TESTS)
                    .param(testName)
                    .insertSingleRow();
        }
    }

    private void insertSeparateJVMTests(final Path path) throws SQLException, IOException {
        for (final String testName : Files.readAllLines(path)) {
            sqlite.statement(SQLStatements.INSERT_SEPARATE_JVM_TESTS)
                    .param(testName)
                    .insertSingleRow();
        }
    }

    private void insertSubjectLOC(final Path path) throws IOException, SQLException {
        System.out.println("[INFO] Inserting subject's LOC and TEST_LOC");

        final ListEx<ListEx<String>> csv = csv(path);

        for (final ListEx<String> rows : csv) {
            sqlite.statement(SQLStatements.UPDATE_SUBJECT_RAW_LOC)
                    .param(rows.get(2)) // loc
                    .param(rows.get(3)) // test_loc
                    .param(rows.get(0).toLowerCase()) // slug
                    .executeUpdate();
        }
    }

    private void insertFullSubjectList(final Path fullList) throws IOException, SQLException {
        if (!Files.exists(fullList)) {
            throw new FileNotFoundException(fullList.toAbsolutePath().toString());
        }

        System.out.println("[INFO] Inserting subject list from: " + fullList);

        try (final FileInputStream fis = new FileInputStream(fullList.toAbsolutePath().toString());
             final InputStreamReader isr = new InputStreamReader(fis);
             final CSVReader reader = new CSVReader(isr)) {

            String[] strings;
            while ((strings = reader.readNext()) != null) {
                if (strings.length >= 2) {
                    insertSubjectRaw(strings[0], strings[1]);
                }
            }
        }
    }

    private void insertSubjectRaw(final String url, final String sha) throws MalformedURLException, SQLException {
        // Get the slug. The path starts with a slash, so get rid of it via substring
        final String slug = new URL(url).getPath().substring(1);

        System.out.println("[INFO] Inserting " + url + " with slug " + slug + " and SHA " + sha);
        sqlite.statement(SQLStatements.INSERT_RAW_SUBJECT)
                .param(slug.toLowerCase())
                .param(url.toLowerCase())
                .param(sha.toLowerCase())
                .executeUpdate();
    }

    private void runPostSetup() throws IOException {
        System.out.println("[INFO] Running post setup queries");
        sqlite.executeFile(SQLStatements.POST_SETUP);
    }

    private void createTables() throws IOException {
        System.out.println("[INFO] Creating tables and views");
        sqlite.executeFile(SQLStatements.CREATE_TABLES);
        System.out.println();
    }

    private void insertFSFileLocation(final String slug, final String commitSha, final Path fileLocPath) throws SQLException, IOException {
        if (!Files.exists(fileLocPath)) {
            System.out.println("[WARNING] Cannot find file location at: " + fileLocPath.toString());
            return;
        }

        if (!filesAdded.add(fileLocPath)) {
            System.out.println("[INFO] Already inserted file location for: " + slug);
            return;
        }

        System.out.println("[INFO] Inserting file location for: " + slug);

        for (final String line : Files.readAllLines(fileLocPath)) {
            String[] lineArr = line.split(",");

            if (lineArr.length != 3) {
                continue;
            }

            String testName = lineArr[0];
            // Remove /home/awshi2/ from all paths
            String fileLoc = lineArr[1].substring(13);
            String moduleLoc = lineArr[2].substring(13);

            sqlite.statement(SQLStatements.INSERT_FS_FILE_LOC)
                    .param(testName)
                    .param(commitSha)
                    .param(fileLoc)
                    .param(moduleLoc)
                    .insertSingleRow();
        }
    }

    private void insertFSExperiment(final Path parent) throws SQLException, IOException {
        if (!Files.exists(parent)) {
            System.out.println("[WARNING] Cannot find parent for experiment info at: " + parent.toString());
            return;
        }

        if (!filesAdded.add(parent)) {
            System.out.println("[INFO] Already inserted experiment info for: " + parent.toString());
            return;
        }

        final String parentStr = parent.getFileName().toString();

        final Path testToFileCsv = parent.resolve("test-to-file.csv");

        long testToFileIsEmpty = -1;
        if (Files.isRegularFile(testToFileCsv)) {
            testToFileIsEmpty = Files.size(testToFileCsv);
        }

        System.out.println("[INFO] Inserting experiment info for: " + parentStr);

        final String[] parentStrAr = parentStr.split("=");
        final String testNamePart = parentStrAr[1];
        final String slugNamePart = parentStrAr[0];

        //final String slug = parentStr.substring(0, parentStr.indexOf('_')).replace('.', '/');
        final String slug = slugNamePart.substring(0, slugNamePart.lastIndexOf('-')).replace('.', '/');
        final String shortSha = slugNamePart.substring(slugNamePart.lastIndexOf('-') + 1);

        final String testName = testNamePart.substring(0, testNamePart.lastIndexOf('_'));

        sqlite.statement(SQLStatements.INSERT_FS_EXPERIMENT)
                .param(slug)
                .param(testName)
                .param(shortSha)
                .param(testToFileIsEmpty)
                .insertSingleRow();
    }

    private String GetInputCSVSha(final String slug, final Path fileLocPath) throws SQLException, IOException {
        if (!Files.exists(fileLocPath)) {
            return "";
        }

        System.out.println("[INFO] Getting input CSV for module: " + slug);

        String line = Files.readAllLines(fileLocPath).get(0);
        String[] lineArr = line.split(",");

        if (lineArr.length < 2) {
            return "";
        }

        return lineArr[1];
    }

    private void insertResults(final Path path) throws IOException, SQLException {
        final Path parent = findParent(path);

        if (parent == null) {
            System.out.println("[WARNING] Parent is null!");
            return;
        }
        final String parentStr = parent.getFileName().toString();


        final String moduleName = path.getFileName().toString();
        final String[] parentStrAr = parentStr.split("=");
        final String testNamePart = parentStrAr[1];
        final String slugNamePart = parentStrAr[0];

        //final String slug = parentStr.substring(0, parentStr.indexOf('_')).replace('.', '/');
        final String slug = slugNamePart.substring(0, slugNamePart.lastIndexOf('-')).replace('.', '/');
        final String testName = testNamePart.substring(0, testNamePart.lastIndexOf('_'));

        final String commitSha = GetInputCSVSha(moduleName, parent.resolve("input.csv"));

        insertFSFileLocation(slug, commitSha, parent.resolve("test-to-file.csv"));

        insertModuleTestTime(slug, path.resolve(DetectorPathManager.DETECTION_RESULTS).resolve("module-test-time.csv"));

        insertOriginalOrder(moduleName, path.resolve(DetectorPathManager.ORIGINAL_ORDER), commitSha);

        if (!sqlite.checkExists("subject", moduleName)) {
            insertSubject(moduleName, slug, path);
        }

        // If we got a no passing order exception, don't insert any of the other results
        if (!Files.exists(path.resolve("error")) ||
            !FileUtil.readFile(path.resolve("error")).contains(NoPassingOrderException.class.getSimpleName())) {
//            insertTestRuns(name, path.resolve(RunnerPathManager.TEST_RUNS).resolve("results"));

            insertDetectionResults(moduleName, "flaky", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertDetectionResults(moduleName, "random", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertDetectionResults(moduleName, "random-class", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertDetectionResults(moduleName, "reverse", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertDetectionResults(moduleName, "reverse-class", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertDetectionResults(moduleName, "smart-shuffle", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);

            insertVerificationResults(moduleName, "smart-shuffle-verify", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertVerificationResults(moduleName, "smart-shuffle-confirmation-sampling", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertVerificationResults(moduleName, "random-verify", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertVerificationResults(moduleName, "random-class-verify", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertVerificationResults(moduleName, "reverse-verify", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertVerificationResults(moduleName, "reverse-class-verify", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertVerificationResults(moduleName, "random-confirmation-sampling", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertVerificationResults(moduleName, "random-class-confirmation-sampling", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertVerificationResults(moduleName, "reverse-confirmation-sampling", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);
            insertVerificationResults(moduleName, "reverse-class-confirmation-sampling", path.resolve(DetectorPathManager.DETECTION_RESULTS), commitSha);

            insertMinimizedResults(moduleName, path.resolve(MinimizerPathManager.MINIMIZED));
            insertStaticFieldInfo(path, TracerMode.TRACK);
            insertPollutedFields(path.resolve(PollutionPathManager.POLLUTION_DATA));

            insertRewriteResults(path.resolve(DiagnoserPathManager.DIAGNOSIS));

            insertFixerResults(moduleName, path.getParent().resolve(CleanerPathManager.FIXER_LOG), path.resolve(CleanerPathManager.FIXER));
//            insertFieldDiffs(path.resolve(DiagnoserPathManager.DIFFS_PATH));
        }

        System.out.println("[INFO] Finished " + moduleName + " (" + slug + ")");
        System.out.println();
    }

    private void insertOriginalOrder(final String subjectName, final Path originalOrderPath, final String commitSha)
            throws SQLException, IOException {
        if (Files.exists(originalOrderPath)) {
            if (!sqlite.checkExists("original_order", "subject_name", subjectName)) {
                final List<String> originalOrder = Files.readAllLines(originalOrderPath);
                System.out.println("[INFO] Inserting original order for " + subjectName + " (" + originalOrder.size() + " tests)");

                final Procedure statement = sqlite.statement(SQLStatements.INSERT_ORIGINAL_ORDER);

                statement.beginTransaction();

                for (int i = 0; i < originalOrder.size(); i++) {
                    statement
                            .param(subjectName)
                            .param(originalOrder.get(i))
                            .param(testClass(originalOrder.get(i)))
                            .param(testClass(testClass(originalOrder.get(i))))
                            .param(i).param(commitSha).addBatch();
                }

                statement.executeBatch();
                statement.commit();
                statement.endTransaction();
            }
        } else {
            System.out.println("[WARNING] No original order found at " + originalOrderPath);
        }
    }

    private String testClass(final String testName) {
        final int i = testName.lastIndexOf(".");

        if (i >= 0) {
            return testName.substring(0, i);
        } else {
            return "";
        }
    }

    private void insertFixerResults(final String subjectName, final Path log, final Path patchDir)
            throws IOException, SQLException {
        if (!Files.exists(log)) {
            System.out.println("[WARNING] SKIPPING: Fixing log not found at: " + log);
            return;
        }

        if (!Files.isDirectory(patchDir)) {
            System.out.println("[WARNING] SKIPPING: No patch dir: " + patchDir);
            return;
        }

        System.out.println("[INFO] Inserting fixing results for " + subjectName + " from " + log + " and " + patchDir);
        if (Files.isDirectory(patchDir)) {
            for (final Path p : FileUtil.listFiles(patchDir)) {
                // For now, skip the extra metadata json we generate now
                if (p.toString().endsWith(".json")) {
                    continue;
                }
                insertFixInfo(subjectName, p);
            }
        }
    }

    private void insertFixInfo(final String subjectName, final Path patchFile)
            throws IOException, SQLException {
        final String fname = patchFile.getFileName().toString();
        final String testName = fname.substring(0, fname.lastIndexOf(".patch"));

        if (!Files.exists(patchFile)) {
            System.out.println("[WARNING] Patch file " + patchFile + " does not exist!");
            return;
        }

        final List<String> lines = Files.readAllLines(patchFile);

        if (lines.get(0).startsWith("STATUS: ")) {
            if (lines.size() < 4) {
                System.out.println("[WARNING] Patch file " + patchFile + " has too few lines");
                return;
            }

            final String status = lines.get(0).substring("STATUS: ".length());

            final Matcher matcher = PRIOR_PATCH_PATTERN.matcher(status);

            final String modified;
            final String cleaner;
            final String polluter;
            if (matcher.matches()) {
                cleaner = matcher.group(2);
                modified = matcher.group(3);

                final String priorFixTestName = matcher.group(1);
                final ListEx<ListEx<String>> queryRes =
                        sqlite.makeProc("select polluter_name from test_patch where test_name = ?;")
                        .param(priorFixTestName)
                        .tableQuery().table();

                if (!queryRes.isEmpty() && !queryRes.get(0).isEmpty()) {
                    polluter = queryRes.get(0).get(0);
                } else {
                    // TODO: We can handle this case by inserting this after the other test, or maybe updating it afterwards
                    polluter = "N/A";
                }
            } else {
                modified = lines.get(1).substring("MODIFIED: ".length());
                cleaner = lines.get(2).substring("CLEANER: ".length());
                polluter = lines.get(3).substring("POLLUTER: ".length());
            }

            final int sepI = lines.indexOf(CleanerFixerPlugin.PATCH_LINE_SEP);
            final int fixSize;
//            System.out.println(patchFile + " : " + sepI);
            if (sepI >= 0) {
                fixSize = lines.subList(sepI + 2, lines.size()).size();
            } else {
                fixSize = 0;
            }

            sqlite.statement(SQLStatements.INSERT_TEST_PATCH)
                    .param(subjectName)
                    .param(testName)
                    .param(cleaner)
                    .param(polluter)
                    .param(modified)
                    .param(status)
                    .param(status.contains("INLINE") ||
                           status.contains("PRIOR PATCH FIXED") ? 1 : 0)
                    // 6 because there's a line ========= that separates metadata from patch and
                    // there's a line from the diff showing the patch location
                    .param(fixSize)
                    .insertSingleRow();
        }
    }

    private void insertFieldDiffs(final Path path) throws IOException, SQLException {
        if (!Files.isDirectory(path)) {
            System.out.println("[WARNING] SKIPPING: No field diffs path: " + path);
            return;
        }

        System.out.println("[INFO] Inserting field diffs from: " + path);

        for (final Path p : FileUtil.listFiles(path)) {
//            System.out.println(p);
            final Optional<? extends FieldDiff> fieldDiff = FileUtil.safeReadJson(FieldDiff.class, p).findFirst();

            if (fieldDiff.isPresent()) {
                sqlite.statement(SQLStatements.INSERT_FIELD_DIFF)
                        .param(fieldDiff.get().fieldName())
                        .param(fieldDiff.get().xpath())
                        .param(fieldDiff.get().innerFieldName())
                        .param(fieldDiff.get().withDepsVal())
                        .param(fieldDiff.get().withoutDepsVal())
                        .insertSingleRow();
            }
        }
    }

    private void insertRewriteResults(final Path path) throws IOException, SQLException {
        if (!Files.isDirectory(path)) {
            System.out.println("[WARNING] SKIPPING: No rewrite results path: " + path);
            return;
        }

        System.out.println("[INFO] Inserting rewrite results from: " + path);

        for (final Path p : FileUtil.listFiles(path)) {
            final String contents = FileUtil.readFile(p);

            final String[] split = FilenameUtils.removeExtension(p.getFileName().toString()).split("-");

            if (split.length < 3) {
                System.out.println("[WARNING] File name format of " + p + " is incorrect. Most likely due to outdated data.");
                continue;
            }

            final String testName = split[0];
            final String hash = split[1];
            final String expectedResult = split[2];

            final int id = sqlite.statement(SQLStatements.INSERT_DIAGNOSIS_RESULT)
                    .param(testName)
                    .param(hash)
                    .param(expectedResult)
                    .insertSingleRow();

            final DiagnosisResult diagnosisResult = new Gson().fromJson(contents, DiagnosisResult.class);

            if (diagnosisResult.diagnoses() != null) {
                for (final PolluterDiagnosis diagnosis : diagnosisResult.diagnoses()) {
                    insertDiagnosis(id, diagnosis);
                }
            } else {
                System.out.println("[WARNING] File " + p + " was in the wrong format (could not read diagnoses)?!");
            }
        }
    }

    private void insertDiagnosis(final int diagnosisResultId, final PolluterDiagnosis diagnosis) throws SQLException {
        final StringBuilder queryBuilder = new StringBuilder("select pd.id\n" +
                "from polluter_data pd\n");

        final List<String> deps = diagnosis.polluterData().deps();
        for (int i = 0; i < deps.size(); i++) {
            final String dep = deps.get(i);
            queryBuilder.append("inner join dependency d")
                    .append(i)
                    .append(" on d")
                    .append(i)
                    .append(".polluter_data_id = pd.id and d")
                    .append(i)
                    .append(".test_name = '")
                    .append(dep)
                    .append("'\n");
        }

        final ListEx<LinkedHashMap<String, String>> rows = sqlite.makeProc(queryBuilder.toString()).tableQuery().rows();

        if (rows.isEmpty()) {
            System.out.println("[WARNING] No polluter data found for: " + diagnosis.polluterData().deps());
            return;
        } else if (!rows.get(0).containsKey("id")) {
            System.out.println("[WARNING] No id column found when trying to lookup polluter data.");
            return;
        }

        final int polluterDataId = Integer.valueOf(rows.get(0).get("id"));

        final int id = sqlite.statement(SQLStatements.INSERT_POLLUTER_DIAGNOSIS)
                .param(polluterDataId)
                .param(diagnosisResultId)
                .insertSingleRow();

        for (final RewritingResult rewritingResult : diagnosis.rewritingResults().rewritingResults()) {
            insertRewritingResult(polluterDataId, id, rewritingResult);
        }
    }

    private int insertRewritingResult(final int polluterDataId,
                                      final int polluterDiagnosisId,
                                      final RewritingResult rewritingResult) throws SQLException {
        final int id = sqlite.statement(SQLStatements.INSERT_REWRITING_RESULT)
                .param(rewritingResult.testRunResult().id())
                .param(rewritingResult.result().toString())
                .param(rewritingResult.expected().toString())
                .param(polluterDiagnosisId)
                .insertSingleRow();

        for (final RewriteTarget target : rewritingResult.target().targets()) {
            insertRewriteTarget(id, target);
        }

        return id;
    }

    private int insertRewriteTarget(final int rewritingResultId, final RewriteTarget target) throws SQLException {
        final ListEx<ListEx<String>> table = sqlite.statement(SQLStatements.GET_POLLUTED_FIELD_ID)
                .param(target.staticFieldName())
                .tableQuery().table();

        if (table.isEmpty() || table.get(0).isEmpty()) {
            System.out.println("[WARNING] Did not find polluted field: " + target.staticFieldName());
            return -1;
        }

        final int pollutedFieldId = Integer.valueOf(table.get(0).get(0));

        return sqlite.statement(SQLStatements.INSERT_REWRITE_TARGET)
                .param(target.staticFieldName())
                .param(target.fieldName())
                .param(pollutedFieldId)
                .param(rewritingResultId)
                .insertSingleRow();
    }

    private void insertPollutedFields(final Path path) throws IOException, SQLException {
        if (!Files.isDirectory(path)) {
            System.out.println("[WARNING] SKIPPING: No polluted fields path: " + path);
            return;
        }

        System.out.println("[INFO] Inserting polluted fields from: " + path);

        for (final Path p : FileUtil.listFiles(path)) {
            if (FilenameUtils.isExtension(p.getFileName().toString(), "xml")) {
                final Map<String, PollutedField> pollutedFields =
                        (Map<String, PollutedField>) TestResult.getXStreamInstance().fromXML(p.toFile());

                final String fname = FilenameUtils.removeExtension(path.getFileName().toString());
                final String[] split = fname.split("-");

                if (split.length < 2) {
                    System.out.println("[WARNING] SKIPPING: File " + p + " is named incorrectly! " +
                            "There should be two parts to the filename, with the second being the expected result.");
                    continue;
                }

                final String expected = split[1];

                for (final Map.Entry<String, PollutedField> entry : pollutedFields.entrySet()) {
                    final String fieldName = entry.getKey();
                    final PollutedField field = entry.getValue();
                    sqlite.statement(SQLStatements.INSERT_POLLUTED_FIELD)
                            .param(fieldName)
                            .param(field.testName())
                            .param(expected)
                            .param(field.withoutDepsVal())
                            .param(field.withDepsVal())
                            .insertSingleRow();
                }
            }
        }
    }

    private void insertStaticFieldInfo(final Path path, final TracerMode mode) throws IOException, SQLException {
        final Path staticFieldInfoPath =
                path.resolve(StaticFieldPathManager.STATIC_FIELD_INFO_PATH.getFileName().toString() + "-" + mode.toString());

        if (!Files.isDirectory(staticFieldInfoPath)) {
            System.out.println("[WARNING] SKIPPING: No static field info path: " + staticFieldInfoPath);
            return;
        }

        System.out.println("[INFO] Inserting static field info (mode: " + mode + ") from: " + path);

        for (Path p : FileUtil.listFiles(staticFieldInfoPath)) {
            final String[] filenameParts = p.getFileName().toString().split("-");
            final StaticTracer staticTracer = new Gson().fromJson(FileUtil.readFile(p), StaticTracer.class);

            if (mode == TracerMode.TRACK) {
                insertStaticTracerTrack(filenameParts[0], filenameParts[1], mode, staticTracer);
            }
        }
    }

    private void insertStaticTracerTrack(final String testName, final String hash,
                                         final TracerMode mode, final StaticTracer staticTracer) throws SQLException {
        int id = sqlite.statement(SQLStatements.INSERT_STATIC_FIELD_INFO)
                .param(testName)
                .param(hash)
                .param(mode.toString())
                .insertSingleRow();

        for (final Map.Entry<String, StaticAccessInfo> entry : staticTracer.staticFields().entrySet()) {
            final String fieldName = entry.getKey();
            final StaticAccessInfo fieldValue = entry.getValue();

            sqlite.statement(SQLStatements.INSERT_STATIC_FIELD_INFO_FIELD)
                    .param(id)
                    .param(fieldName)
                    .insertSingleRow();
        }
    }

    private void insertMinimizedResults(final String subjectName, final Path minimized) throws IOException {
        if (!Files.isDirectory(minimized)) {
            System.out.println("[WARNING] SKIPPING: No minimized folder " + minimized);
            return;
        }

        System.out.println("[INFO] Inserting minimized results from: " + minimized);

        FileUtil.listFiles(minimized).stream()
                .flatMap(FileUtil::safeReadFile)
                .map(s -> new Gson().fromJson(s, MinimizeTestsResult.class))
                .forEach(minimizeTestsResult -> {
                    try {
                        insertMinimizeTestResult(subjectName, minimizeTestsResult);
                    } catch (SQLException e) {
                        throw new RuntimeException(e);
                    }
                });
    }

    private int insertMinimizeTestResult(final String subjectName, final MinimizeTestsResult minimizeTestsResult) throws SQLException {
        int id = sqlite.statement(SQLStatements.INSERT_MINIMIZE_TEST_RESULT)
                .param(subjectName)
                .param(minimizeTestsResult.dependentTest())
                .param(minimizeTestsResult.expectedRun().id())
                .param(minimizeTestsResult.expected().toString())
                .param(minimizeTestsResult.hash())
                .param(insertOperationTime(minimizeTestsResult.time()))
                .insertSingleRow();

        for (final PolluterData polluter : minimizeTestsResult.polluters()) {
            insertPolluterData(id, polluter);
        }

        return id;
    }

    private int insertPolluterData(final int minimizeTestResultId, final PolluterData polluter) throws SQLException {
        int id = sqlite.statement(SQLStatements.INSERT_POLLUTER_DATA)
                    .param(minimizeTestResultId)
                    .insertSingleRow();

        for (int i = 0; i < polluter.deps().size(); i++) {
            sqlite.statement(SQLStatements.INSERT_DEPENDENCY)
                    .param(id)
                    .param(polluter.deps().get(i))
                    .param(i)
                    .insertSingleRow();
        }

        insertCleanerData(id, polluter.cleanerData());
        return id;
    }

    private int insertCleanerData(final int polluterDataId,
                                  final CleanerData cleanerData) throws SQLException {
        // TODO: add stats about things like same package, same class, etc.
        int id = sqlite.statement(SQLStatements.INSERT_CLEANER_DATA)
                    .param(polluterDataId)
                    .param(cleanerData.isolationResult().toString())
                    .param(cleanerData.expected().toString())
                    .param(insertTimeManager(cleanerData.time()))
                    .insertSingleRow();

        for (final CleanerGroup cleanerGroup : cleanerData.cleaners()) {
            insertCleanerGroup(id, cleanerGroup);
        }

        return id;
    }

    private int insertCleanerGroup(final int cleanerDataId, final CleanerGroup cleanerGroup) throws SQLException {
        final int id = sqlite.statement(SQLStatements.INSERT_CLEANER_GROUP)
                .param(cleanerDataId)
                .param(cleanerGroup.originalSize())
                .param(cleanerGroup.cleanerTests().size())
                .insertSingleRow();

        final ListEx<String> cleanerTests = cleanerGroup.cleanerTests();
        for (int i = 0; i < cleanerTests.size(); i++) {
            final String cleanerTest = cleanerTests.get(i);

            sqlite.statement(SQLStatements.INSERT_CLEANER_TEST)
                    .param(id)
                    .param(cleanerTest)
                    .param(i)
                    .insertSingleRow();
        }

        return id;
    }

    private int insertTimeManager(final TimeManager time) throws SQLException {
        return sqlite.statement(SQLStatements.INSERT_TIME_MANAGER)
                .param(insertOperationTime(time.addTime()))
                .param(insertOperationTime(time.totalTime()))
                .insertSingleRow();
    }

    private int insertOperationTime(final OperationTime time) throws SQLException {
        if (time == null) {
            System.out.println("[WARNING] Tried to insert a null operation time! This likely indicates an issue with the data files (possibly out of date format that didn't have the time)");
            return -1;
        }

        return sqlite.statement(SQLStatements.INSERT_OPERATION_TIME)
                .param(time.startTime())
                .param(time.endTime())
                .param(time.elapsedSeconds())
                .insertSingleRow();
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

    private Path findParent(final Path path) {
        if (path == null || path.getFileName() == null) {
            return null;
        }

        if (path.getFileName().toString().endsWith("_output")) {
            return path;
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

    private void insertDetectionResults(final String name, final String roundType, final Path path,
                                        final String commitSha) throws IOException {
        final Path detectionResults = path.resolve(roundType);

        if (!Files.exists(detectionResults)) {
            return;
        }

        final ListEx<Path> paths = listFiles(detectionResults);
        System.out.println("[INFO] Inserting " + roundType + " detection results for " + name
                + " (" + paths.size() + " results)");

        for (int i = 0; Files.exists(detectionResults.resolve("round" + i + ".json")); i++) {

            final Path p = detectionResults.resolve("round" + i  + ".json");
            final int roundNumber = roundNumber(p.getFileName().toString());

            try {
                final DetectionRound round = new Gson().fromJson(FileUtil.readFile(p), DetectionRound.class);
                insertDetectionRound(name, roundType, roundNumber, round, commitSha);
            } catch (IOException | SQLException e) {
                throw new RuntimeException(e);
            }
        }
    }

    private void insertDetectionRound(final String name, final String roundType,
                                      final int roundNumber, final DetectionRound round,
                                      final String commitSha)
            throws IOException, SQLException {
        if (round == null) {
            return;
        }

        final int unfilteredId = insertDependentTestList(round.unfilteredTests(), commitSha);
        final int filteredId = insertDependentTestList(round.filteredTests(), commitSha);

        final int detectionRoundId =
                sqlite.statement(SQLStatements.INSERT_DETECTION_ROUND)
                .param(name)
                .param(unfilteredId)
                .param(filteredId)
                .param(roundType)
                .param(roundNumber)
                .param((float) round.roundTime())
                .param(commitSha)
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

    private int insertDependentTestList(final DependentTestList dependentTestList, final String commitSha) throws IOException, SQLException {
        final int index = dtListIndex;
        dtListIndex++;

        for (DependentTest dependentTest : dependentTestList.dts()) {
            final int dependentTestId = insertDependentTest(dependentTest, commitSha);

            sqlite.statement(SQLStatements.INSERT_FLAKY_TEST_LIST)
                    .param(index)
                    .param(dependentTestId)
                    .param(commitSha)
                    .executeUpdate();
        }

        return index;
    }

    private int insertDependentTest(final DependentTest dependentTest, final String commitSha) throws SQLException {
        return sqlite.statement(SQLStatements.INSERT_FLAKY_TEST)
                .param(dependentTest.name())
                .param(dependentTest.intended().testRunId())
                .param(dependentTest.revealed().testRunId())
                .param(commitSha)
                .insertSingleRow();
    }

    private void insertVerificationResults(final String name, final String roundType, final Path basePath,
                                           final String commitSha) throws IOException {
        final Path verificationResults = basePath.resolve(roundType);

        if (!Files.isDirectory(verificationResults)) {
            return;
        }

        final ListEx<Path> paths = listFiles(verificationResults);

        System.out.println("[INFO] Inserting " + roundType + " verification results for " + name + " (" + paths.size() + " rounds)");

        paths.forEach(p -> {
            try {
                insertVerificationRound(name, roundType, roundNumber(p.getFileName().toString()), p, commitSha);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }

    private void insertVerificationRound(final String name, final String roundType, final int roundNumber,
                                         final Path p, final String commitSha) throws IOException {
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
                        .param(commitSha)
                        .executeUpdate();

                insertTestRunResult(name, testRunResult);
            } catch (IOException | SQLException e) {
                throw new RuntimeException(e);
            }
        });
    }
}
