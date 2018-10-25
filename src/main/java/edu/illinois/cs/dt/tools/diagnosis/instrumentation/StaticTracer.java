package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import com.google.gson.Gson;
import com.reedoei.eunomia.collections.SetUtil;
import com.reedoei.eunomia.functional.Cons;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.testrunner.configuration.Configuration;
import edu.illinois.cs.dt.tools.runner.data.TestResult;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.codehaus.plexus.util.StringUtils;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class StaticTracer {
    private static StaticTracer tracer = new StaticTracer();
    private static Map<TracerMode, Consumer<String>> tracerModes = new ConcurrentHashMap<>();

    static {
        tracerModes.put(TracerMode.NONE, Cons.ignore());
        tracerModes.put(TracerMode.FIRST_ACCESS, StaticTracer::monitorFirstAccess);
        tracerModes.put(TracerMode.REWRITE, StaticTracer::rewrite);
        tracerModes.put(TracerMode.TRACK, StaticTracer::track);
    }

    public static <T> T inMode(final TracerMode mode, final Callable<T> c) throws Exception {
        final TracerMode currentMode = mode();

        Configuration.config().properties().setProperty("statictracer.mode", String.valueOf(mode));

        final T t = c.call();

        Configuration.config().properties().setProperty("statictracer.mode", String.valueOf(currentMode));

        return t;
    }

    public static StaticTracer from(final Path path) throws IOException {
        return new Gson().fromJson(FileUtil.readFile(path), StaticTracer.class);
    }

    private final Map<String, StaticAccessInfo> staticFields = new ConcurrentHashMap<>();
    private final Map<String, String> firstAccessVals = new ConcurrentHashMap<>();
    private final Set<String> rewrittenProperties = Collections.synchronizedSet(new HashSet<>());

    public static StaticTracer tracer() {
        return tracer;
    }

    public StaticTracer() {
    }

    public StaticTracer(final Map<String, StaticAccessInfo> staticFields) {
        this.staticFields.putAll(staticFields);
    }

    public static TracerMode mode() {
        return TracerMode.valueOf(
                Configuration.config().getProperty("statictracer.mode", String.valueOf(TracerMode.NONE)));
    }

    public Map<String, String> firstAccessVals() {
        return firstAccessVals;
    }

    public Set<String> rewrittenProperties() {
        return rewrittenProperties;
    }

    public Map<String, StaticAccessInfo> staticFields() {
        return staticFields;
    }

    public static void logStatic(final String fieldName) {
        tracerModes
            .getOrDefault(mode(), Cons.ignore())
            .accept(fieldName);
    }

    private static void track(final String fieldName) {
        tracer().staticFields().computeIfAbsent(fieldName, k -> {
            StackTraceElement[] stackTrace;
            try {
                stackTrace = Thread.currentThread().getStackTrace();
            } catch (Throwable ignored) {
                // This can happen if there is a security exception (meaning we aren't allowed to get the stack trace.
                stackTrace = new StackTraceElement[0];
            }

            final StackTraceElement[] elements =
                    Arrays.stream(stackTrace)
                            .filter(StaticTracer::stackTraceFilter)
                            .toArray(StackTraceElement[]::new);

            return new StaticAccessInfo(fieldName, elements);
        });
    }

    private static boolean stackTraceFilter(final StackTraceElement stackTraceElement) {
        final String clzName = stackTraceElement.getClassName();

        return !clzName.startsWith("edu.illinois.cs.dt.") &&
               !clzName.startsWith("org.junit.") &&
               !clzName.startsWith("scala.") &&
               !clzName.startsWith("com.reedoei.testrunner.");
    }

    private static void rewrite(final String fieldName) {
        final @NonNull String currentTest = Configuration.config().getProperty("testrunner.current_test", "");

        // Using "none" as default so the two defaults don't match
        final @NonNull String testToCheck = Configuration.config().getProperty("statictracer.rewrite.test_name", "none");

        if (!currentTest.equals(testToCheck)) {
            return;
        }

        if (fieldName.equals(Configuration.config().getProperty("statictracer.rewrite.field", null))) {
            if (!tracer().rewrittenProperties.contains(fieldName)) {
                final String rewriteValue = Configuration.config().getProperty("statictracer.rewrite.value", null);
                System.out.println("Rewriting " + fieldName + " using value " + StringUtils.abbreviate(rewriteValue, 50));
                final Object o = TestResult.getXStreamInstance().fromXML(
                        rewriteValue);

                FieldAccessorFactory.accessorFor(fieldName).ifPresent(accessor -> {
                    accessor.set(o);
                });

                tracer().rewrittenProperties.add(fieldName);
            }
        }
    }

    private static void monitorFirstAccess(final String fieldName) {
        final @NonNull String currentTest = Configuration.config().getProperty("testrunner.current_test", "");

        // Using "none" as default so the two defaults don't match
        final @NonNull String testToCheck = Configuration.config().getProperty("statictracer.first_access.test", "none");

        if (currentTest.equals(testToCheck)) {
            if (!tracer().firstAccessVals().containsKey(fieldName)) {
                FieldAccessorFactory.accessorFor(fieldName).ifPresent(accessor -> {
                    final String serialized = sanitizeXmlChars(TestResult.getXStreamInstance().toXML(accessor.get()));
                    tracer().firstAccessVals().putIfAbsent(fieldName, serialized);
                });
            }
        }
    }

    // Note: We use a String here rather than a Path simply for ease of inserting the method with Soot.
    public static void output(final String path) {
        try {
            Files.write(Paths.get(path), new Gson().toJson(tracer()).getBytes());
            tracer().staticFields().clear();
            tracer().firstAccessVals().clear();
            tracer().rewrittenProperties().clear();
        } catch (IOException ignored) {}
    }

    public static String concat(final String a, final String b) {
        return a + b;
    }

    public Optional<StaticAccessInfo> get(final String fieldName) {
        return Optional.ofNullable(staticFields().get(fieldName));
    }

    /**
     * Takes in a string and removes problematic characters.
     *
     * @param  in  the input string to be filtered
     * @return     the input string with the unparsable characters removed
     */
    public static String sanitizeXmlChars(String in) {
        in = in.replaceAll("&#", "&amp;#");
        StringBuilder out = new StringBuilder();

        if ("".equals(in)) {
            return "";
        }

        for (int i = 0; i < in.length(); i++) {
            char current = in.charAt(i);
            if ((current == 0x9) ||
                (current == 0xA) ||
                (current == 0xD) ||
                ((current >= 0x20) && (current <= 0xD7FF)) ||
                ((current >= 0xE000) && (current <= 0xFFFD)) ||
                ((current >= 0x10000) && (current <= 0x10FFFF))) {
                out.append(current);
            }
        }

        return out.toString();
    }
}
