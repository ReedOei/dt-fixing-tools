package edu.illinois.cs.dt.tools.analysis;

import com.reedoei.eunomia.util.StandardMain;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.text.NumberFormat;

public class CommandGenerator extends StandardMain {
    private final SQLite sqlite;

    private CommandGenerator(final String[] args) throws SQLException {
        super(args);

        this.sqlite = new SQLite(Paths.get(getArgRequired("db")));
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
        percentInstance.setMaximumIntegerDigits(1);

        System.out.println(commandQuery("numTests", SQLStatements.COUNT_TESTS));
        System.out.println(commandQuery("numModules", SQLStatements.COUNT_MODULES));
        System.out.println(commandQuery("numModulesResults", SQLStatements.COUNT_MODULES_RESULTS));
        System.out.println(commandQuery("numProjsResults", SQLStatements.COUNT_PROJECTS_RESULTS));

        System.out.println(commandQuery("numProjODTests",
                sqlite.statement(SQLStatements.COUNT_PROJECTS_WITH_FLAKY).param("random")));
        System.out.println(commandQuery("numModuleODTests",
                sqlite.statement(SQLStatements.COUNT_MODULES_WITH_FLAKY).param("random")));

        System.out.println(commandQuery("numProjNOTests",
                sqlite.statement(SQLStatements.COUNT_PROJECTS_WITH_FLAKY).param("flaky")));
        System.out.println(commandQuery("numModuleNOTests",
                sqlite.statement(SQLStatements.COUNT_MODULES_WITH_FLAKY).param("flaky")));

        System.out.println(commandQuery("numProjODNOTests", SQLStatements.COUNT_PROJECTS_WITH_ODNO));
        System.out.println(commandQuery("numModuleODNOTests", SQLStatements.COUNT_PROJECTS_WITH_ODNO));

        final int numOdTests = query(sqlite.statement(SQLStatements.COUNT_FLAKY).param("random"));
        final int numNoTests = query(sqlite.statement(SQLStatements.COUNT_FLAKY).param("flaky"));
        System.out.println(command("numODTests", String.valueOf(numOdTests)));
        System.out.println(command("numNOTests", String.valueOf(numNoTests)));

        System.out.println(command("numFlakyTests", String.valueOf(numOdTests + numNoTests)));

        System.out.println(command("percODTests",
                percentInstance.format((double)numOdTests / (numOdTests + numNoTests)).replace("%", "\\%")));
        System.out.println(command("percNOTests",
                percentInstance.format((double)numNoTests / (numOdTests + numNoTests)).replace("%", "\\%")));

        System.out.println(commandQuery("numODNOTests", SQLStatements.COUNT_ODNO_TESTS));

        System.out.println(commandQuery("percODTestFailOne", "\\%",
                sqlite.statement(SQLStatements.PROBABILITY_FAILURE).param(1)));
        System.out.println(commandQuery("percODTestFailTen", "\\%",
                sqlite.statement(SQLStatements.PROBABILITY_FAILURE).param(10)));
        System.out.println(commandQuery("percODTestFailTwenty", "\\%",
                sqlite.statement(SQLStatements.PROBABILITY_FAILURE).param(20)));

        System.out.println(commandQuery("numNOTestAllRandOrig", SQLStatements.TOTAL_NO_ORIG_AND_RANDOM));

        System.out.println(commandQuery("percODTestsRandom", "\\%",
                sqlite.statement(SQLStatements.PROBABILITY_FIND_RANDOM)));
        System.out.println(commandQuery("percNOTestsRandom", "\\%",
                sqlite.statement(SQLStatements.PROBABILITY_FIND_FLAKY_NO_ORIGINAL)));
        System.out.println(commandQuery("percNOTestsAll", "\\%",
                sqlite.statement(SQLStatements.PROBABILITY_FIND_FLAKY)));

        System.out.println(commandQuery("percBestODOrder", "\\%", sqlite.statement(SQLStatements.PROBABILITY_BEST_RANDOM)));
        System.out.println(commandQuery("percBestNOOrder", "\\%", sqlite.statement(SQLStatements.PROBABILITY_BEST_FLAKY)));

        final int numNOTestOrig = query(sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("flaky").param("flaky"));
        final int numNOTestRandClassMethod = query(sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("flaky").param("random"));
        final int numNOTestRandClass = query(sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("flaky").param("random-class"));
        final int numNOTestReverse = query(sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("flaky").param("reverse"));
        final int numNOTestReverseClass = query(sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("flaky").param("reverse-class"));
        System.out.println(command("numNOTestOrig", String.valueOf(numNOTestOrig)));
        System.out.println(command("numNOTestRandClassMethod", String.valueOf(numNOTestRandClassMethod)));
        System.out.println(command("numNOTestRandClass", String.valueOf(numNOTestRandClass)));
        System.out.println(command("numNOTestReverse", String.valueOf(numNOTestReverse)));
        System.out.println(command("numNOTestReverseClass", String.valueOf(numNOTestReverseClass)));

        final int numODTestReverse = query(sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("random").param("reverse"));
        final int numODTestReverseClass = query(sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("random").param("reverse-class"));
        final int numODTestRandClassMethod = query(sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("random").param("random"));
        final int numODTestRandClass = query(sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("random").param("random-class"));
        System.out.println(command("numODTestReverse", String.valueOf(numODTestReverse)));
        System.out.println(command("numODTestReverseClass", String.valueOf(numODTestReverseClass)));
        System.out.println(command("numODTestRandClassMethod", String.valueOf(numODTestRandClassMethod)));
        System.out.println(command("numODTestRandClass", String.valueOf(numODTestRandClass)));

        System.out.println(command("numFlakyTestRandClassMethod", String.valueOf(numODTestRandClassMethod + numNOTestRandClassMethod)));
        System.out.println(command("numFlakyTestRandClass", String.valueOf(numODTestRandClass + numNOTestRandClass)));
        System.out.println(command("numFlakyTestReverse", String.valueOf(numODTestReverse + numNOTestReverse)));
        System.out.println(command("numFlakyTestReverseClass", String.valueOf(numODTestReverseClass + numNOTestReverseClass)));

        System.out.println(commandQuery("numProjFlakyTests", SQLStatements.COUNT_PROJECT_FLAKY_TESTS));
        System.out.println(commandQuery("numModuleFlakyTests", SQLStatements.COUNT_MODULE_FLAKY_TESTS));
    }

    private int query(final Path path) throws SQLException {
        return query(sqlite.statement(path));
    }

    private int query(final Procedure procedure) throws SQLException {
        return Integer.parseInt(procedure.tableQuery().table().get(0).get(0));
    }

    private String commandQuery(final String commandName, final Path path) throws SQLException {
        return commandQuery(commandName, sqlite.statement(path));
    }

    private String commandQuery(final String commandName, final Procedure procedure) throws SQLException {
        return commandQuery(commandName, "", procedure);
    }

    private String commandQuery(final String commandName, final String postStr, final Procedure procedure) throws SQLException {
        return commandQuery(commandName, "", postStr, procedure);
    }

    private String commandQuery(final String commandName, final String preStr,
                                final String postStr, final Procedure procedure) throws SQLException {
        return command(commandName, String.format("%s%s%s", preStr, procedure.tableQuery().table().get(0).get(0), postStr));
    }

    private String command(final String commandName, final String val) {
        return String.format("\\newcommand{\\%s}{%s\\xspace}", commandName, val);
    }
}
