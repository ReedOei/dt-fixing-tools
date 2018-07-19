package edu.illinois.cs.dt.tools.diagnosis;

import com.google.common.base.Preconditions;
import com.reedoei.eunomia.collections.SetUtil;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.subject.Subject;
import com.reedoei.eunomia.subject.SubjectFactory;
import com.reedoei.eunomia.util.ProcessUtil;
import com.reedoei.eunomia.util.StandardMain;
import com.reedoei.eunomia.util.Util;
import edu.illinois.cs.dt.tools.diagnosis.detection.Detector;
import edu.illinois.cs.dt.tools.diagnosis.detection.ExecutingDetector;
import edu.illinois.cs.dt.tools.diagnosis.detection.DetectorFactory;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.Instrumentation;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestList;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.runner.SimpleRunner;
import edu.illinois.cs.dt.tools.utility.TestFinder;
import edu.washington.cs.dt.tools.UnitTestFinder;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Stream;

public class Diagnoser extends StandardMain {
    private static final Path STATIC_FIELD_INFO_PATH = Paths.get("static-field-info").toAbsolutePath();
    private final Subject subject;
    private final String classpath;
    private final Path javaAgent;

    private Diagnoser(final String[] args) throws IOException {
        super(args);

        subject = SubjectFactory.forPath(Paths.get(".").toAbsolutePath().toRealPath());
        classpath = getArg("cp", "classpath").orElse(subject.classpath());
        javaAgent = Paths.get(getArgRequired("javaagent"));
    }

    public static void main(final String[] args) {
        try {
            new Diagnoser(args).run();
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.exit(0);
    }

    @Override
    protected void run() throws Exception {
        results().forEach(diagnose(staticFieldInfo()));
    }

    public Consumer<MinimizeTestsResult> diagnose(final Map<String, Set<String>> staticFieldInfo) {
        return result -> new TestDiagnoser(classpath, javaAgent, staticFieldInfo, result, subject).run();
    }

    private Stream<MinimizeTestsResult> results() throws Exception {
        // TODO: Refactor into provider or factory or something.
        if (getArg("minimized").isPresent()) {
            final Path minimized = Paths.get(getArgRequired("minimized"));
            return Stream.of(MinimizeTestsResult.fromPath(minimized));
        } else if (getArg("dtFolder").isPresent()) {
            final Path dtFolder = Paths.get(getArgRequired("dtFolder"));
            return new MinimizeTestList(classpath).runDependentTestFolder(dtFolder);
        } else {
            return detect();
        }
    }

    private Stream<MinimizeTestsResult> detect() throws Exception {
        final Path dtFolder = Files.createDirectories(Paths.get("detection-results"));
        final Path dtFile = dtFolder.resolve(ExecutingDetector.DT_LISTS_PATH);

        if (!Files.exists(dtFile)) {
            final List<String> tests = new TestFinder(classpath, subject).get();

            final Detector detector = DetectorFactory.makeDetector(classpath, tests);
            System.out.println("[INFO] Created dependent test detector (" + detector.getClass() + ").");
            detector.writeTo(dtFolder);
        }

        return new MinimizeTestList(classpath).runDependentTestFile(dtFile);
    }

    private Map<String, Set<String>> staticFieldInfo() throws Exception {
        if (!Files.exists(STATIC_FIELD_INFO_PATH) || Files.list(STATIC_FIELD_INFO_PATH).count() == 0) {
            Files.createDirectories(STATIC_FIELD_INFO_PATH);
            generateStaticFieldInfo(STATIC_FIELD_INFO_PATH);
        }

        final Map<String, Set<String>> result = new HashMap<>();

        Files.walk(STATIC_FIELD_INFO_PATH).forEach(path -> {
            if (Files.isRegularFile(path)) {
                try {
                    result.put(path.getFileName().toString(), SetUtil.read(FileUtil.readFile(path)));
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        });

        return result;
    }

    private void generateStaticFieldInfo(final Path staticFieldInfoPath) throws Exception {
        System.out.println("[INFO] Instrumenting to get lists of static fields.");

        if (FileUtil.isEmpty(Paths.get("sootOutput"))) {
            final String sootCp = subject.classpath() + Util.buildClassPath(System.getProperty("java.home") + "/lib/*");

            System.out.println("[INFO] Instrumenting test classes.");
            Instrumentation.instrument(sootCp, subject.testClasses(), staticFieldInfoPath);
            System.out.println("[INFO] Instrumenting classes.");
            Instrumentation.instrument(sootCp, subject.classes(), staticFieldInfoPath);
        }

        final String sootOutputCp =
                Util.buildClassPath(
                        Paths.get("").resolve("sootOutput").toAbsolutePath().toString(),
                        subject.dependencies() + "/*") + File.pathSeparator +
                System.getProperty("java.class.path");

        System.out.println("[INFO] Running dts.");
        ProcessUtil.runClass(sootOutputCp, SimpleRunner.class, "--dts", subject.testClasses().toString())
        .waitFor();
    }
}
