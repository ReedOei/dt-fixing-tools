package edu.illinois.cs.dt.tools.fixer;

import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import edu.illinois.cs.dt.tools.utility.PathManager;
import org.apache.commons.io.FilenameUtils;

import java.nio.file.Path;
import java.nio.file.Paths;

public class CleanerPathManager extends PathManager {
    public static final String BACKUP_EXTENSION = ".orig";

    public static Path backupPath(final Path path) {
        if (path.getParent() == null) {
            return Paths.get(path.getFileName().toString() + BACKUP_EXTENSION);
        } else {
            return path.getParent().resolve(path.getFileName().toString() + BACKUP_EXTENSION);
        }
    }

    // TODO: Move to a utility class (and eunomia)
    public static Path changeExtension(final Path path, final String newExtension) {
        final String extToAdd = newExtension.startsWith(".") ? newExtension : "." + newExtension;

        return path.toAbsolutePath().getParent().resolve(FilenameUtils.removeExtension(path.getFileName().toString()) + extToAdd);
    }

    public static Path compiledPath(final Path sourcePath) {
        final Path testSrcDir = Paths.get(TestPluginPlugin.mavenProject().getBuild().getTestSourceDirectory());
        final Path testBinDir = Paths.get(TestPluginPlugin.mavenProject().getBuild().getTestOutputDirectory());

        final Path relative = testSrcDir.relativize(sourcePath.toAbsolutePath());

        return testBinDir.resolve(changeExtension(relative, "class"));
    }
}