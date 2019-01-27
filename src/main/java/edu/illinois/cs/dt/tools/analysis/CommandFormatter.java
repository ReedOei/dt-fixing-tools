package edu.illinois.cs.dt.tools.analysis;

import com.reedoei.eunomia.collections.ListEx;
import org.checkerframework.checker.nullness.qual.Nullable;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public class CommandFormatter {
    private static final Path COMMAND_LISTS = Paths.get("command-lists");

    private final Map<String, Double> commandValues;
    private final String commandPrefix;
    private final LatexTools tools;
    private final SQLite sqlite;
    private final @Nullable Path path;

    public CommandFormatter(final Map<String, Double> commandValues, final String commandPrefix,
                            final LatexTools tools, final SQLite sqlite, final @Nullable Path path) {
        this.commandValues = commandValues;
        this.commandPrefix = commandPrefix;
        this.tools = tools;
        this.sqlite = sqlite;
        this.path = path;
    }

    public CommandFormatter count(final String name, final Object... params) throws IOException, SQLException {
        if (path == null) {
            throw new IllegalStateException("Tried to create a command using a null path for the procedure!");
        }

        final ListEx<ListEx<String>> table = sqlite.statement(path, params).tableQuery().table();

        Files.createDirectories(COMMAND_LISTS);

        if (table.isEmpty()) {
            return printAndWrite(name, new ArrayList<>());
        } else {
            return printAndWrite(name, ListEx.transpose(table).get(0));
        }
    }

    public CommandFormatter printAndWrite(final String name, final Collection<String> collection) throws IOException {
        return printAndWrite(name, "", collection);
    }

    public CommandFormatter printAndWrite(final String name, final String suffix, final Collection<String> collection)
            throws IOException {
        commandValues.put(commandPrefix + name + suffix, (double) collection.size());
        System.out.println(tools.command(commandPrefix + name + suffix, String.valueOf(collection.size())));

        Files.createDirectories(COMMAND_LISTS);
        Files.write(COMMAND_LISTS.resolve(name + suffix), collection);

        return this;
    }

    public CommandFormatter print(final String name, final Object... params) throws SQLException {
        if (path == null) {
            throw new IllegalStateException("Tried to create a command using a null path for the procedure!");
        }

        final int n = tools.query(sqlite.statement(path, params));
        commandValues.put(commandPrefix + name, (double) n);
        System.out.println(tools.command(commandPrefix + name, String.valueOf(n)));

        return this;
    }

    public CommandFormatter finishGroup() {
        System.out.println();
        return this;
    }
}
