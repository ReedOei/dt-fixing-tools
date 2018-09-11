package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import com.google.gson.Gson;
import com.reedoei.eunomia.collections.SetUtil;
import com.reedoei.eunomia.functional.Cons;
import com.reedoei.eunomia.io.files.FileUtil;
import edu.illinois.cs.dt.tools.runner.data.TestResult;
import org.checkerframework.checker.nullness.qual.NonNull;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
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

    private final Set<StaticAccessInfo> staticFields = Collections.synchronizedSet(new HashSet<>());
    private final Map<String, String> firstAccessVals = new ConcurrentHashMap<>();
    private final Set<String> rewrittenProperties = Collections.synchronizedSet(new HashSet<>());

    public static StaticTracer tracer() {
        return tracer;
    }

    public StaticTracer() {
    }

    public StaticTracer(final Set<StaticAccessInfo> staticFields) {
        this.staticFields.addAll(staticFields);
    }

    public static StaticTracer from(final Path path) throws IOException {
        @NonNull final String json = FileUtil.readFile(path);

        // If it's the old format, then it's just a list of static fields
        if (json.startsWith("[")) {
            final Set<StaticAccessInfo> staticFields =
                    SetUtil.read(json).stream()
                            .map(fieldName -> new StaticAccessInfo(fieldName, Collections.emptyList()))
                            .collect(Collectors.toSet());
            return new StaticTracer(staticFields);
        } else {
            return new Gson().fromJson(json, StaticTracer.class);
        }
    }

    public Set<StaticAccessInfo> staticFields() {
        return staticFields;
    }

    private static Properties defaultProperties() {
        final Properties properties = new Properties();

        properties.setProperty("statictracer.mode", String.valueOf(TracerMode.NONE));

        return properties;
    }

    private static Properties properties() {
        try (final FileInputStream fileInputStream = new FileInputStream("statictracer.properties")) {
            final Properties properties = new Properties();
            properties.load(fileInputStream);
            return properties;
        } catch (IOException e) {
            e.printStackTrace();
        }

        return defaultProperties();
    }

    public static void logStatic(final String fieldName) {
        final Properties properties = properties();

        tracerModes
            .getOrDefault(TracerMode.valueOf(properties.getProperty("statictracer.mode")), Cons.ignore())
            .accept(fieldName);
    }

    private static void track(final String fieldName) {
        StackTraceElement[] stackTrace;
        try {
            stackTrace = Thread.currentThread().getStackTrace();
        } catch (Throwable ignored) {
            // This can happen if there is a security exception (meaning we aren't allowed to get the stack trace.
            stackTrace = new StackTraceElement[0];
        }

        tracer().staticFields().add(new StaticAccessInfo(fieldName, stackTrace));
    }

    private static void rewrite(final String fieldName) {
        if (fieldName.equals(System.getProperty("statictracer.rewrite.field"))) {
            if (!tracer().rewrittenProperties.contains(fieldName)) {
                final Object o = TestResult.getXStreamInstance().fromXML(System.getProperty("statictracer.rewrite.value"));

                FieldAccessorFactory.accessorFor(fieldName).ifPresent(accessor -> accessor.set(o));

                tracer().rewrittenProperties.add(fieldName);
            }
        }
    }

    private static void monitorFirstAccess(final String fieldName) {
        // TODO: Extract first access value tracker to another phase (for tracking "correct" value of field)
        FieldAccessorFactory.accessorFor(fieldName).ifPresent(accessor -> {
            final String serialized = sanitizeXmlChars(TestResult.getXStreamInstance().toXML(accessor.get()));

            if (!tracer().firstAccessVals.containsKey(fieldName)) {
                System.out.printf("REED %s: %s\n", fieldName, serialized);
            }

            tracer().firstAccessVals.putIfAbsent(fieldName, serialized);
        });
    }

    // Note: We use a String here rather than a Path simply for ease of inserting the method with Soot.
    public static void output(final String path) {
        try {
            Files.write(Paths.get(path), new Gson().toJson(tracer()).getBytes());
            tracer().staticFields().clear();
        } catch (IOException ignored) {}
    }

    public static String concat(final String a, final String b) {
        return a + b;
    }

    public Optional<StaticAccessInfo> get(final String fieldName) {
        return staticFields().stream().filter(s -> s.fieldName().equals(fieldName)).findFirst();
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
