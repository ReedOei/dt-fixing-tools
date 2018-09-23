package edu.illinois.cs.dt.tools.diagnosis.pollution;

import com.github.javaparser.utils.StringEscapeUtils;
import com.reedoei.eunomia.data.caching.FileCache;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.diagnosis.DiffContainer;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticAccessInfo;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticFieldInfo;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticTracer;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.TracerMode;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.runner.data.TestResult;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.checkerframework.checker.nullness.qual.NonNull;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiConsumer;

public class Pollution extends FileCache<Map<String, DiffContainer.Diff>> {
    private static final Path POLLUTION_STORAGE_PATH = Paths.get("pollution-data");

    private final Path path;

    private final Runner runner;
    private final MinimizeTestsResult minimized;

    public Pollution(final Runner runner, final MinimizeTestsResult minimized) {
        this.runner = runner;
        this.minimized = minimized;

        this.path = Paths.get("pollution-data").resolve(minimized.dependentTest() + "-" + minimized.expected() + ".xml");
    }

    @Override
    public @NonNull Path path() {
        return path;
    }

    @Override
    protected Map<String, DiffContainer.Diff> load() {
        // This is actually safe...sorry
        return (Map<String, DiffContainer.Diff>) TestResult.getXStreamInstance().fromXML(path().toFile());
    }

    @Override
    protected void save() {
        final String s = TestResult.getXStreamInstance().toXML(get());

        try {
            Files.createDirectories(path().getParent());
            Files.write(path(), s.getBytes());
        } catch (IOException e) {
            e.printStackTrace(System.err);
        }
    }

    @Override
    protected @NonNull Map<String, DiffContainer.Diff> generate() {
        try {
            Files.createDirectories(POLLUTION_STORAGE_PATH);
            FileUtils.deleteDirectory(StaticFieldInfo.STATIC_FIELD_INFO_PATH.toFile());
            Files.createDirectories(StaticFieldInfo.STATIC_FIELD_INFO_PATH);

            final Path withDeps = POLLUTION_STORAGE_PATH.resolve(minimized.getPath("with-deps"));
            final Path withoutDeps = POLLUTION_STORAGE_PATH.resolve(minimized.getPath("without-deps"));

            // Run with dependencies and monitor first access, then run without and monitor first access
            // If they values of some fields are different, then that's likely the source of the
            // difference in behavior
            StaticTracer.inMode(TracerMode.FIRST_ACCESS, () -> {
                Configuration.config().properties().setProperty("statictracer.first_access.test", minimized.dependentTest());

                runner.runList(minimized.withDeps());
                Files.move(StaticFieldInfo.STATIC_FIELD_INFO_PATH.resolve(minimized.dependentTest()), withDeps);

                runner.runList(Collections.singletonList(minimized.dependentTest()));
                Files.move(StaticFieldInfo.STATIC_FIELD_INFO_PATH.resolve(minimized.dependentTest()), withoutDeps);

                return null;
            });

            FileUtils.deleteDirectory(StaticFieldInfo.STATIC_FIELD_INFO_PATH.toFile());

            final Map<String, String> before = StaticTracer.from(withDeps).firstAccessVals();
            final Map<String, String> after = StaticTracer.from(withoutDeps).firstAccessVals();

            return new DiffContainer(minimized.dependentTest(), before, after).getDiffs();
        } catch (Exception e) {
            e.printStackTrace();
        }

        return new HashMap<>();
    }

    public void forEachFiltered(final BiConsumer<String, DiffContainer.Diff> consumer) {
        get().forEach((fieldName, diff) -> {
            if (shouldConsume(fieldName, diff)) {
                consumer.accept(fieldName, diff);
            }
        });
    }

    private boolean shouldConsume(final String fieldName, final DiffContainer.Diff diff) {
        if (fieldName.startsWith("com.thoughtworks.xstream") ||
                fieldName.startsWith("java.") ||
                fieldName.startsWith("javax.") ||
                fieldName.startsWith("jdk.") ||
                fieldName.startsWith("sun.")) {
            return false;
        }

        final String[] split = fieldName.split(".");

        if (split.length == 0) {
            return true;
        }

        final String varName = split[split.length - 1];

        // e.g. FIELDS_NAMED_LIKE_THIS shouldn't be considered, because they're probably constants.
        return !varName.toUpperCase().equals(varName);
    }

    public static String formatDiff(final String fieldName, final DiffContainer.Diff diff) {
        return String.format("%s: (%s, %s)", fieldName,
                StringEscapeUtils.escapeJava(StringUtils.abbreviate(String.valueOf(diff.getBefore()), 30)),
                StringEscapeUtils.escapeJava(StringUtils.abbreviate(String.valueOf(diff.getAfter()), 30)));
    }

    private boolean different(final DiffContainer.Diff diff) {
        // If getBefore or getAfter is null, then that means there is no value for either one
        // If the value itself IS null, then getBefore or getAfter will give "<null/>"
        if (diff.getBefore() != null && diff.getAfter() != null) {
            return !diff.getBefore().equals(diff.getAfter());
        } else {
            return false;
        }
    }

    public Map<String, DiffContainer.Diff> findPollutions(final Map<String, StaticAccessInfo> fieldList) {
        final Map<String, DiffContainer.Diff> pollutions = new HashMap<>();

        forEachFiltered((fieldName, diff) -> {
            if (fieldList.containsKey(fieldName)) {
                if (different(diff)) {
                    System.out.println("-----------------------------------------------------------");
                    System.out.println("-- Polluted: " + formatDiff(fieldName, diff));
                    System.out.println("-----------------------------------------------------------");

                    fieldList.get(fieldName).stackTrace().forEach(System.out::println);

                    pollutions.put(fieldName, diff);
                }
            }
        });

        return pollutions;
    }
}
