package edu.illinois.cs.dt.tools.detection.analysis;

import com.google.gson.Gson;
import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.util.StandardMain;
import edu.illinois.cs.dt.tools.detection.DetectionRound;
import edu.illinois.cs.dt.tools.detection.DetectorPathManager;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;
import edu.illinois.cs.dt.tools.runner.data.DependentTestList;
import edu.illinois.cs.dt.tools.analysis.ResultDirVisitor;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class BuildDTLists extends StandardMain {
    private final List<String> fullDtList;
    private final Path datasetPath;
    private final Path outputPath;

    private BuildDTLists(final String[] args) throws IOException {
        super(args);

        this.fullDtList = Files.readAllLines(Paths.get(getArgRequired("full-dt-list")));
        this.datasetPath = Paths.get(getArgRequired("dataset"));
        this.outputPath = Paths.get(getArgRequired("output"));
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
            final Path p = resultsFolder.resolve(DetectorPathManager.DETECTION_RESULTS);
            if (Files.isDirectory(p)) {
                System.out.println("[INFO] Gathering detection rounds from: " + p);
                save(p, buildDtLists(gatherDetectionRounds(p)));
            }
        }
    }

    private DependentTestList buildDtLists(final Stream<DetectionRound> detectionRounds) {
        return new DependentTestList(detectionRounds
                .flatMap(dr -> dr.filteredTests().dts().stream()
                    .filter(dt -> fullDtList.contains(dt.name())))
                .collect(Collectors.toList()));
    }

    private Stream<DetectionRound> gatherDetectionRounds(final Path resultsFolder) throws IOException {
        return Files.walk(resultsFolder)
                .filter(p -> Files.isRegularFile(p) && p.getFileName().toString().startsWith("round"))
                .flatMap(FileUtil::safeReadFile)
                .map(s -> new Gson().fromJson(s, DetectionRound.class))
                .filter(Objects::nonNull);
    }

    private void save(final Path resultsFolder, final DependentTestList dependentTestList) throws IOException {
        final String moduleName = resultsFolder.getParent().getFileName().toString();
        final String outputFileName = String.format("%s-%s", moduleName, DetectorPathManager.FLAKY_LIST_PATH.getFileName().toString());
        final Path outputFile = outputPath.resolve(outputFileName);

        if (Files.exists(outputFile)) {
            try {
                final DependentTestList l = new Gson().fromJson(FileUtil.readFile(outputFile), DependentTestList.class);

                if (l != null) {
                    for (final DependentTest dependentTest : l.dts()) {
                        if (dependentTestList.dts().stream().noneMatch(dt -> dt.name().equals(dependentTest.name()))) {
                            dependentTestList.dts().add(dependentTest);
                        }
                    }
                }
            } catch (Exception ignored) {}
        }

        System.out.println("[INFO] Writing dt list to (" + dependentTestList.size() + " tests) to: " + outputFile);

        Files.write(outputFile, new Gson().toJson(dependentTestList).getBytes());
    }
}
