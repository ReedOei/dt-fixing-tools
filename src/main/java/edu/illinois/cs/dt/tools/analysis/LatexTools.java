package edu.illinois.cs.dt.tools.analysis;

import java.nio.file.Path;
import java.sql.SQLException;

public class LatexTools {
    private final SQLite sqlite;
    private final String commandPrefix;

    public LatexTools(final SQLite sqlite, final String commandPrefix) {
        this.sqlite = sqlite;
        this.commandPrefix = commandPrefix;
    }

    public int query(final Path path) throws SQLException {
        return query(sqlite.statement(path));
    }

    public int query(final Procedure procedure) throws SQLException {
        return Integer.parseInt(procedure.tableQuery().table().get(0).get(0));
    }

    public String commandQuery(final String commandName, final Path path) throws SQLException {
        return commandQuery(commandName, sqlite.statement(path));
    }

    public String commandQuery(final String commandName, final Procedure procedure) throws SQLException {
        return commandQuery(commandName, "", procedure);
    }

    public String commandQuery(final String commandName, final String postStr, final Procedure procedure) throws SQLException {
        return commandQuery(commandName, "", postStr, procedure);
    }

    public String commandQuery(final String commandName, final String preStr,
                                final String postStr, final Procedure procedure) throws SQLException {
        return command(commandName, String.format("%s%s%s", preStr, procedure.tableQuery().table().get(0).get(0), postStr));
    }

    public String command(final String commandName, final String val) {
        return String.format("\\newcommand{\\%s%s}{%s\\xspace}", commandPrefix, commandName, val);
    }

    public void printCommand(final String name, final String value) {
        System.out.println(command(name, value));
    }

    public void printQuery(final String name, final Path path) throws SQLException {
        System.out.println(commandQuery(name, path));
    }
}
