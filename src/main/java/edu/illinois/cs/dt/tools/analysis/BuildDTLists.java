package edu.illinois.cs.dt.tools.analysis;

import com.google.gson.Gson;
import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.eunomia.collections.PairStream;
import com.reedoei.eunomia.util.OptUtil;
import com.reedoei.eunomia.util.StandardMain;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.detection.DetectorPathManager;
import edu.illinois.cs.dt.tools.runner.RunnerPathManager;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.illinois.cs.dt.tools.runner.data.DependentTestList;
import edu.illinois.cs.dt.tools.runner.data.TestRun;
import edu.illinois.cs.dt.tools.utility.TestRunParser;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class BuildDTLists extends StandardMain {
    private final List<String> fullDtList;
    private final Path datasetPath;

    private BuildDTLists(final String[] args) throws IOException {
        super(args);

        this.fullDtList = Files.readAllLines(Paths.get(getArgRequired("full-dt-list")));
        this.datasetPath = Paths.get(getArgRequired("dataset"));
    }

    public static void main(final String[] args) {
        try {
            new BuildDTLists(args).run();
        } catch (Exception e) {
            e.printStackTrace();

            System.exit(1);
        }

        System.exit(0);
    }

    @Override
    protected void run() throws Exception {
        final ListEx<Path> allResultsFolders = new ListEx<>();
        Files.walkFileTree(datasetPath, new ResultDirVisitor(allResultsFolders));

        for (final Path resultsFolder : allResultsFolders) {
            if (Files.isDirectory(resultsFolder.resolve(DetectorPathManager.DETECTION_RESULTS)) &&
                Files.isDirectory(resultsFolder.resolve(RunnerPathManager.TEST_RUNS))) {
                System.out.println("[INFO] Gathering test runs from: " + resultsFolder);
                save(resultsFolder, buildDtLists(gatherTestRuns(resultsFolder)));
            }
        }
    }

    private DependentTestList buildDtLists(final PairStream<String, TestRun> testRuns) {
        final Map<String, TestRun> intendedRuns = new HashMap<>();
        final Map<String, TestRun> revealedRuns = new HashMap<>();

        final Set<String> toDo = new HashSet<>(fullDtList);

        final ListEx<DependentTest> result = new ListEx<>();

        testRuns.forEach((testName, testRun) -> {
            if (testRun.result().equals(Result.PASS)) {
                intendedRuns.putIfAbsent(testName, testRun);
            } else {
                if (toDo.contains(testName)) {
                    if (intendedRuns.containsKey(testName)) {
                        result.add(new DependentTest(testName, intendedRuns.get(testName), testRun));
                        toDo.remove(testName);

                        revealedRuns.remove(testName);
                    } else {
                        revealedRuns.putIfAbsent(testName, testRun);
                    }
                }
            }
        });

        for (final String dt : toDo) {
            if (intendedRuns.containsKey(dt) && revealedRuns.containsKey(dt)) {
                result.add(new DependentTest(dt, intendedRuns.get(dt), revealedRuns.get(dt)));
            }
        }

        System.out.println("[INFO] " + result.size() + " dependent tests found.");

        return new DependentTestList(result);
    }

    private PairStream<String, TestRun> gatherTestRuns(final Path resultsFolder) throws IOException {
        return new TestRunParser(resultsFolder).testResults().flatMap((output, trr) -> {
            final ListEx<String> testOrder = new ListEx<>(trr.testOrder());

            final ListEx<String> testNames = new ListEx<>();
            final ListEx<TestRun> testRuns = new ListEx<>();

            for (final String dt : fullDtList) {
                if (trr.results().containsKey(dt)) {
                    testNames.add(dt);
                    testRuns.add(addTestRun(trr, testOrder, dt));
                }
            }

            return PairStream.zip(testNames, testRuns);
        });
    }

    private TestRun addTestRun(final TestRunResult trr,
                               final ListEx<String> testOrder,
                               final String dt) {
        final ListEx<String> testPrefix = testOrder.beforeInc(dt);
        final Result testResult = trr.results().get(dt).result();
        return new TestRun(testPrefix, testResult, trr.id());
    }

    private void save(final Path resultsFolder, final DependentTestList dependentTestList) throws IOException {
        Files.write(resultsFolder.resolve(DetectorPathManager.DETECTION_RESULTS).resolve(DetectorPathManager.DT_LIST_PATH),
                new Gson().toJson(dependentTestList).getBytes());
    }
}
