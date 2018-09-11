package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.collections.StreamUtil;
import com.reedoei.eunomia.subject.Subject;
import com.reedoei.eunomia.subject.SubjectFactory;
import com.reedoei.eunomia.util.StandardMain;
import edu.illinois.cs.dt.tools.diagnosis.detection.Detector;
import edu.illinois.cs.dt.tools.diagnosis.detection.DetectorFactory;
import edu.illinois.cs.dt.tools.diagnosis.detection.ExecutingDetector;
import edu.illinois.cs.dt.tools.diagnosis.pollution.PollutionContainer;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestList;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.utility.TestFinder;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Stream;

// TODO: Make all files cache inside of a dir like .dtfixingtools
public class Diagnoser extends StandardMain {
    private final Subject subject;
    private final String classpath;
    private final Path javaAgent;

    private Diagnoser(final String[] args) throws IOException {
        super(args);

        subject = SubjectFactory.forPath(Paths.get(".").toAbsolutePath().toRealPath());
        classpath = getArg("cp", "classpath").orElse(subject.classpath());
        javaAgent = Paths.get(getArgRequired("javaagent"));
    }

    public Diagnoser(final Subject subject, final Path javaAgent) {
        this(subject, subject.classpath(), javaAgent);
    }

    public Diagnoser(final Subject subject, final String classpath, final Path javaAgent) {
        super(new String[0]);

        this.subject = subject;
        this.classpath = classpath;
        this.javaAgent = javaAgent;
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
        StreamUtil.seq(diagnose());
    }

    public Stream<PollutionContainer> diagnose() throws Exception {
        return results().map(result -> new TestDiagnoser(classpath, javaAgent, result, subject).run());
    }

    private Stream<MinimizeTestsResult> results() throws Exception {
        if (Files.exists(Paths.get("minimized"))) {
            return Files.walk(Paths.get("minimized")).flatMap(p -> {
                try {
                    return Stream.of(MinimizeTestsResult.fromPath(p));
                } catch (IOException ignored) {}

                return Stream.empty();
            });
        } else if (getArg("minimized").isPresent()) {
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
}
