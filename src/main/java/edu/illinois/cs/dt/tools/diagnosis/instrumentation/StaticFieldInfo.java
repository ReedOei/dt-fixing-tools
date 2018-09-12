package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import com.reedoei.eunomia.data.caching.FileCache;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.subject.classpath.Classpath;
import com.reedoei.eunomia.util.RuntimeThrower;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory$;
import com.reedoei.testrunner.testobjects.TestLocator;
import com.reedoei.testrunner.util.MavenClassLoader;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import org.apache.maven.project.MavenProject;
import org.checkerframework.checker.nullness.qual.NonNull;
import scala.collection.immutable.Stream;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

public class StaticFieldInfo extends FileCache<Map<String, StaticTracer>> {
    public static final Path STATIC_FIELD_INFO_PATH = Paths.get("static-field-info").toAbsolutePath();

    private final MavenProject project;
    private final MinimizeTestsResult minimized;

    public StaticFieldInfo(final MavenProject project, final MinimizeTestsResult minimized) {
        this.project = project;
        this.minimized = minimized;
    }

    @Override
    public @NonNull Path path() {
        return STATIC_FIELD_INFO_PATH;
    }

    @Override
    protected Map<String, StaticTracer> load() {
        final Map<String, StaticTracer> result = new HashMap<>();

        new RuntimeThrower<>(() -> {
            Files.walk(STATIC_FIELD_INFO_PATH).forEach(path -> {
                    if (Files.isRegularFile(path)) {
                        try {
                            if (path.getFileName().toString().equals(minimized.dependentTest())) {
                                result.put(path.getFileName().toString(), StaticTracer.from(path));
                            }
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                });

                return null;
        }).run();

        return result;
    }

    @Override
    protected void save() {
        // Intentionally left empty. It will automatically save on generation.
    }

    @Override
    protected @NonNull Map<String, StaticTracer> generate() {
        return new RuntimeThrower<>(() -> {
            generateStaticFieldInfo(STATIC_FIELD_INFO_PATH);
            return load();
        }).run();
    }

    private void generateStaticFieldInfo(final Path staticFieldInfoPath) throws Exception {
        Files.createDirectories(STATIC_FIELD_INFO_PATH);

        System.out.println("[INFO] Instrumenting to get lists of static fields.");

        if (FileUtil.isEmpty(Paths.get("sootOutput"))) {
            final String sootCp = new MavenClassLoader(project).classpath() + Classpath.build(System.getProperty("java.home") + "/lib/*");

            System.out.println("[INFO] Instrumenting test classes.");
            Instrumentation.instrument(sootCp, Paths.get(project.getBuild().getTestOutputDirectory()), staticFieldInfoPath);
            System.out.println("[INFO] Instrumenting classes.");
            Instrumentation.instrument(sootCp, Paths.get(project.getBuild().getOutputDirectory()), staticFieldInfoPath);
        }

        final String sootOutputCp =
                Classpath.build(
                        Paths.get("").resolve("sootOutput").toAbsolutePath().toString(),
                        project.getBuild().getDirectory() + "/dependency/*") + File.pathSeparator +
                        System.getProperty("java.class.path");

        System.out.println("[INFO] Running dts.");

        final Runner runner = RunnerFactory$.MODULE$.from(project).get();
        final Stream<String> tests = TestLocator.tests(project);
        runner.runWithCp(sootOutputCp, tests);
    }
}
