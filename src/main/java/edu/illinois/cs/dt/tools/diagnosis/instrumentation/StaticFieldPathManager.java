package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import edu.illinois.cs.dt.tools.utility.PathManager;
import org.apache.commons.io.FileUtils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class StaticFieldPathManager extends PathManager {
    // NOTE: If this is changed, then will have to instrument everything
    public static final Path STATIC_FIELD_INFO_PATH = Paths.get("static-field-info");

    public static Path modePath(final TracerMode mode) {
        return path(Paths.get(STATIC_FIELD_INFO_PATH.toString() + "-" + mode));
    }

    public static Path infoFor(final TracerMode mode, final String testName) {
        return modePath(mode).resolve(testName);
    }

    public static Path createEmptyModePath(final TracerMode mode) throws IOException {
        FileUtils.deleteDirectory(modePath(mode).toFile());
        return Files.createDirectories(modePath(mode));
    }

    public static Path createModePath(final TracerMode mode) throws IOException {
        return Files.createDirectories(modePath(mode));
    }
}
