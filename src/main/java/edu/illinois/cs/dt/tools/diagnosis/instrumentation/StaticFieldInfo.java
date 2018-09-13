package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import com.reedoei.eunomia.data.caching.FileCache;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.subject.classpath.Classpath;
import com.reedoei.eunomia.util.RuntimeThrower;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.util.MavenClassLoader;
import edu.illinois.cs.dt.tools.diagnosis.Diagnoser;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import org.apache.commons.io.FileUtils;
import org.apache.maven.project.MavenProject;
import org.checkerframework.checker.nullness.qual.NonNull;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;

public class StaticFieldInfo extends FileCache<StaticTracer> {
    // NOTE: If this is changed, then will have to instrument everything
    public static final Path STATIC_FIELD_INFO_PATH = Paths.get("static-field-info").toAbsolutePath();

    private final MavenProject project;
    private final Runner runner;
    private final MinimizeTestsResult minimized;

    public StaticFieldInfo(final MavenProject project, final Runner runner, final MinimizeTestsResult minimized) {
        this.project = project;
        this.runner = runner;
        this.minimized = minimized;
    }

    @Override
    public @NonNull Path path() {
        return Paths.get(STATIC_FIELD_INFO_PATH.toString() + "-" + String.valueOf(TracerMode.TRACK))
                .resolve(minimized.dependentTest());
    }

    @Override
    protected StaticTracer load() {
        return new RuntimeThrower<>(() -> StaticTracer.from(path())).run();
    }

    @Override
    protected void save() {
        // Intentionally left empty. It will automatically save on generation.
    }

    @Override
    protected @NonNull StaticTracer generate() {
        return new RuntimeThrower<>(() -> {
            generateStaticFieldInfo();
            return load();
        }).run();
    }

    private void generateStaticFieldInfo() throws Exception {
        FileUtils.deleteDirectory(StaticFieldInfo.STATIC_FIELD_INFO_PATH.toFile());
        Files.createDirectories(STATIC_FIELD_INFO_PATH);
        Files.createDirectories(path().getParent());

        System.out.println("[INFO] Instrumenting to get lists of static fields.");
        Instrumentation.instrumentProject(project);

        final String sootOutputCp =
                Classpath.build(
                        Paths.get("").resolve("sootOutput").toAbsolutePath().toString(),
                        project.getBuild().getDirectory() + "/dependency/*") + File.pathSeparator +
                        Diagnoser.cp();

        System.out.println("[INFO] Running tests.");

        StaticTracer.inMode(TracerMode.TRACK, () -> {
            runner.runListWithCp(sootOutputCp, Collections.singletonList(minimized.dependentTest()));

            return null;
        });

        Files.move(STATIC_FIELD_INFO_PATH.resolve(minimized.dependentTest()), path());
    }
}
