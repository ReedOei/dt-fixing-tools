package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import com.reedoei.eunomia.collections.SetUtil;
import com.reedoei.eunomia.data.caching.FileCache;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.subject.Subject;
import com.reedoei.eunomia.util.ProcessUtil;
import com.reedoei.eunomia.util.RuntimeThrower;
import com.reedoei.eunomia.util.Util;
import edu.illinois.cs.dt.tools.runner.SimpleRunner;
import org.checkerframework.checker.nullness.qual.NonNull;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class StaticFieldInfo extends FileCache<Map<String, Set<String>>> {
    private static final Path STATIC_FIELD_INFO_PATH = Paths.get("static-field-info").toAbsolutePath();
    private final Subject subject;

    public StaticFieldInfo(final Subject subject) {
        this.subject = subject;
    }

    @Override
    public @NonNull Path path() {
        return STATIC_FIELD_INFO_PATH;
    }

    @Override
    protected Map<String, Set<String>> load() {
        final Map<String, Set<String>> result = new HashMap<>();

        new RuntimeThrower<>(() -> {
            Files.walk(STATIC_FIELD_INFO_PATH).forEach(path -> {
                    if (Files.isRegularFile(path)) {
                        try {
                            result.put(path.getFileName().toString(), SetUtil.read(FileUtil.readFile(path)));
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
    protected Map<String, Set<String>> generate() {
        return new RuntimeThrower<>(() -> {
            generateStaticFieldInfo(STATIC_FIELD_INFO_PATH);
            return load();
        }).run();
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
