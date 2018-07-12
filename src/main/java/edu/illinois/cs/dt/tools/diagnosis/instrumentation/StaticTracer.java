package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class StaticTracer {
    private static final Set<String> staticFields = Collections.synchronizedSet(new HashSet<>());

    public static void logStatic(final String fieldName) {
        staticFields.add(fieldName);
    }

    // Note: We use a String here rather than a Path simply for ease of inserting the method with Soot.
    public static void output(final String path) {
        try {
            Files.write(Paths.get(path), staticFields.toString().getBytes());
            staticFields.clear();
        } catch (IOException ignored) {}

        System.out.println("Hi");
    }

    public static String concat(final String a, final String b) {
        return a + b;
    }
}
