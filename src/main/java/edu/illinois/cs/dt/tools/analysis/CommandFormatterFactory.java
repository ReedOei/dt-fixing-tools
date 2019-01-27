package edu.illinois.cs.dt.tools.analysis;

import org.checkerframework.checker.nullness.qual.Nullable;

import java.nio.file.Path;
import java.util.Map;

public class CommandFormatterFactory {
    private final Map<String, Double> commandValues;
    private final String commandPrefix;
    private final LatexTools tools;
    private final SQLite sqlite;

    public CommandFormatterFactory(final Map<String, Double> commandValues, final String commandPrefix,
                                   final LatexTools tools, final SQLite sqlite) {
        this.commandValues = commandValues;
        this.commandPrefix = commandPrefix;
        this.tools = tools;
        this.sqlite = sqlite;
    }

    public CommandFormatter create(@Nullable final Path path) {
        return new CommandFormatter(commandValues, commandPrefix, tools, sqlite, path);
    }
}
