package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import com.google.gson.Gson;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

public class StaticTracer {
    private static StaticTracer tracer = new StaticTracer();
    private final Set<StaticAccessInfo> staticFields = Collections.synchronizedSet(new HashSet<>());

    public static StaticTracer tracer() {
        return tracer;
    }

    public Set<StaticAccessInfo> staticFields() {
        return staticFields;
    }

    public static void logStatic(final String fieldName) {
        StackTraceElement[] stackTrace;
        try {
            stackTrace = Thread.currentThread().getStackTrace();
        } catch (Throwable ignored) {
            // This can happen if there is a security exception (meaning we aren't allowed to get the stack trace.
            stackTrace = new StackTraceElement[0];
        }

        tracer().staticFields().add(new StaticAccessInfo(fieldName, stackTrace));
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
}
