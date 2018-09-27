package edu.illinois.cs.dt.tools.utility;

import com.google.common.base.Preconditions;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;

import java.nio.file.Path;

public class PathManager {
    public static Path modulePath() {
        return TestPluginPlugin.mavenProject().getBasedir().toPath();
    }

    public static Path path(final Path relative) {
        Preconditions.checkState(!relative.isAbsolute(),
                "PathManager.path: Cache paths must be relative, not absolute (%s)", relative);

        return modulePath().resolve(".dtfixingtools").resolve(relative);
    }
}
