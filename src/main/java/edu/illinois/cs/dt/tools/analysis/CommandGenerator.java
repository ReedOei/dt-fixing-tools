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
                sqlite.statement(SQLStatements.COUNT_DEPENDENCIES).param(1).param(1)));
        System.out.println(tools.commandQuery("numWithAnyDeps",
                sqlite.statement(SQLStatements.COUNT_DEPENDENCIES).param(1).param(1000000000))); // Use as "infinity"
        System.out.println(tools.commandQuery("numWithMoreThanOneDep",
                sqlite.statement(SQLStatements.COUNT_DEPENDENCIES).param(2).param(1000000000))); // Use as "infinity"
        System.out.println(tools.commandQuery("numWithNoDeps",
                sqlite.statement(SQLStatements.COUNT_DEPENDENCIES).param(0).param(0)));

        System.out.println(tools.commandQuery("numWithSingleCleaner",
                sqlite.statement(SQLStatements.COUNT_CLEANERS).param(1).param(1)));
        System.out.println(tools.commandQuery("numWithAnyCleaner",
                sqlite.statement(SQLStatements.COUNT_CLEANERS).param(1).param(1000000000))); // Use as "infinity"
        System.out.println(tools.commandQuery("numWithMoreThanOneCleaner",
                sqlite.statement(SQLStatements.COUNT_CLEANERS).param(2).param(1000000000))); // Use as "infinity"
        System.out.println(tools.commandQuery("numWithNoCleaner",
                sqlite.statement(SQLStatements.COUNT_CLEANERS).param(0).param(0)));

        // TODO: Number of tests with a single field
        // TODO: Number of tests with any fields
        // TODO: Number of tests with more than one field

        // TODO: Number of lines of code to fix
        // TODO: Number of times that fixer works

        // TODO: Number of polluters vs. number of setters
    }
}
