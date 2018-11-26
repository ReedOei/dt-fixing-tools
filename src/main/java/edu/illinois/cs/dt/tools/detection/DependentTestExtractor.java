package edu.illinois.cs.dt.tools.detection;

import com.google.gson.Gson;
import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.util.StandardMain;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.utility.analysis.ResultDirVisitor;
import edu.illinois.cs.dt.tools.detection.classifiers.DependentClassifier;
import edu.illinois.cs.dt.tools.detection.classifiers.NonorderClassifier;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.illinois.cs.dt.tools.runner.data.DependentTestList;
import edu.illinois.cs.dt.tools.runner.data.TestRun;
import edu.illinois.cs.dt.tools.utility.MD5;
import edu.illinois.cs.dt.tools.utility.TestRunParser;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.BiPredicate;
import java.util.stream.Collectors;

public class DependentTestExtractor extends StandardMain {
    private final Path results;

    private final Path outputPath;

    private DependentTestExtractor(final String[] args) {
        super(args);

        results = Paths.get(getArgRequired("results"));
        outputPath = Paths.get(getArg("output").orElse("output"));
    }

    public static void main(final String[] args) {
        try {
            new DependentTestExtractor(args).run();
        } catch (Exception e) {
            e.printStackTrace();

            System.exit(1);
        }

        System.exit(0);
    }

    @Override
    protected void run() throws Exception {
        final ListEx<Path> allResultsFolders = new ListEx<>();
        Files.walkFileTree(results, new ResultDirVisitor(allResultsFolders));

        for (final Path resultsFolder : allResultsFolders) {
            System.out.println("[INFO] Extracting results from " + resultsFolder);
            save(resultsFolder, extract(resultsFolder));
            System.out.println();
        }
    }

    private boolean isNew(final DependentTestList dependentTestList, final DependentTest dependentTest) {
        final BiPredicate<TestRun, TestRun> pred =
                (a, b) -> MD5.hashOrder(a.order()).equals(MD5.hashOrder(b.order()));

        return dependentTestList.dts().stream()
                .anyMatch(dt -> !pred.test(dt.intended(), dependentTest.intended()) ||
                                !pred.test(dt.revealed(), dependentTest.revealed())) ||
               dependentTestList.dts().stream()
                .noneMatch(dt -> dt.name().equals(dependentTest.name()));
    }

    private void save(final Path resultsFolder, final DependentTestList extracted) throws IOException {
        final String moduleName = resultsFolder.getParent().getFileName().toString();
        final String outputFileName = String.format("%s-%s", moduleName, DetectorPathManager.DT_LIST_PATH.getFileName().toString());
        final Path outputFile = outputPath.resolve(outputFileName);

        if (Files.exists(outputFile)) {
            try {
                final DependentTestList l = new Gson().fromJson(FileUtil.readFile(outputFile), DependentTestList.class);

                if (l != null) {
                    for (final DependentTest dependentTest : l.dts()) {
                        if (isNew(extracted, dependentTest)) {
                            extracted.dts().add(dependentTest);
                        }
                    }
                }
            } catch (Exception ignored) {}
        }

        System.out.println("[INFO] Writing dt list to (" + extracted.size() + " tests) to: " + outputFile);

        Files.write(outputFile, new Gson().toJson(extracted).getBytes());
    }

    public DependentTestList extract(final Path path) throws IOException {
        return dependentTests(new TestRunParser(path).testResults().mapToStream((output, res) -> res).collect(Collectors.toList()));
    }

    private DependentTestList dependentTests(final List<TestRunResult> results) {
        try (final NonorderClassifier nonorderClassifier = new NonorderClassifier();
             final DependentClassifier dependentClassifier = new DependentClassifier(true);
            final DependentClassifier dependentClassifierWithFailures = new DependentClassifier(false)) {
            for (int i = 0; i < results.size(); i++) {
                final TestRunResult testRunResult = results.get(i);
                System.out.printf("\rUpdating classifiers with test run %s of %s (no: %d, od: %d, odf: %d): %s",
                        i + 1,
                        results.size(),
                        nonorderClassifier.nonorderTests().size(),
                        dependentClassifier.dependentTests(nonorderClassifier.nonorderTests()).size(),
                        dependentClassifierWithFailures.dependentTests(nonorderClassifier.nonorderTests()).size(),
                        testRunResult.id());
                nonorderClassifier.update(testRunResult);
                dependentClassifier.update(testRunResult);
                dependentClassifierWithFailures.update(testRunResult);
            }

            if (!results.isEmpty()) {
                System.out.println();
            }
            System.out.println("Finished updating classifiers.");

            return new DependentTestList(makeDependentTestList(nonorderClassifier, dependentClassifier));
        } catch (Exception e) {
            e.printStackTrace();
        }

        return DependentTestList.empty();
    }

    private List<DependentTest> makeDependentTestList(final NonorderClassifier nonorderClassifier,
                                                      final DependentClassifier dependentClassifier) {
        System.out.println("Making dependent test list.");
        final Set<String> nonorderTests = nonorderClassifier.nonorderTests();

        final List<DependentTest> dependentTests = new ArrayList<>();

        dependentClassifier.dependentRuns().forEach((testName, testRuns) -> {
            if (nonorderTests.contains(testName)) {
                return;
            }

            testRuns.stream().filter(tr -> tr.result().equals(Result.PASS)).findFirst().ifPresent(passingRun -> {
                for (final TestRun testRun : testRuns) {
                    if (!testRun.result().equals(Result.PASS)) {
                        System.out.println("Creating dependent test entry (expected: " + testRun.result() + ")");
                        dependentTests.add(new DependentTest(testName, passingRun, testRun));
                    }
                }
            });
        });

        return dependentTests;
    }
}
