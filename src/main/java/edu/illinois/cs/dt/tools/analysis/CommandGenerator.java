package edu.illinois.cs.dt.tools.analysis;

import com.reedoei.eunomia.util.StandardMain;

import java.nio.file.Paths;
import java.sql.SQLException;
import java.text.NumberFormat;

public class CommandGenerator extends StandardMain {
    private final SQLite sqlite;
    private final String commandPrefix;
    private final LatexTools tools;

    private CommandGenerator(final String[] args) throws SQLException {
        super(args);

        this.sqlite = new SQLite(Paths.get(getArgRequired("db")));
        this.commandPrefix = getArg("prefix").orElse("");
        this.tools = new LatexTools(sqlite, commandPrefix);
    }

    public static void main(final String[] args) {
        try {
            new CommandGenerator(args).run();
        } catch (Exception e) {
            e.printStackTrace();

            System.exit(1);
        }

        System.exit(0);
    }

    @Override
    protected void run() throws Exception {
        final NumberFormat percentInstance = NumberFormat.getPercentInstance();
        percentInstance.setMaximumFractionDigits(1);

        System.out.println(tools.commandQuery("numBrittleTests",
                sqlite.statement(SQLStatements.COUNT_OD_TYPE).param("brittle")));
        System.out.println(tools.commandQuery("numVictimTests",
                sqlite.statement(SQLStatements.COUNT_OD_TYPE).param("victim")));
        System.out.println(tools.commandQuery("numBothTests",
                sqlite.statement(SQLStatements.COUNT_OD_TYPE).param("both")));

        // TODO: Could display a table showing how many have each number (though atm all have just 1)
        System.out.println(tools.commandQuery("numWithSingleDep",
                sqlite.statement(SQLStatements.COUNT_DEPENDENCIES).param(1)));

        // TODO: Number of tests that have any dependencies
        // TODO: Number of tests with more than one dependency
        // TODO: Number of tests without any dependencies

        // TODO: Number of tests with a single field
        // TODO: Number of tests with any fields
        // TODO: Number of tests with more than one field

        // TODO: Number of lines of code to fix
        // TODO: Number of times that fixer works

        // TODO: Number of cleaners, number of polluters, number of setters
    }
}
