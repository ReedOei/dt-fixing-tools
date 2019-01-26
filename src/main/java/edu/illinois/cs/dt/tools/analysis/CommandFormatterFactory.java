package edu.illinois.cs.dt.tools.analysis;

import org.checkerframework.checker.nullness.qual.Nullable;

import java.nio.file.Path;

public class CommandFormatterFactory {
    private final LatexTools tools;
    private final SQLite sqlite;

    public CommandFormatterFactory(final LatexTools tools, final SQLite sqlite) {
        this.tools = tools;
        this.sqlite = sqlite;
    }

    public CommandFormatter create(@Nullable final Path path) {
        return new CommandFormatter(tools, sqlite, path);
    }
}
