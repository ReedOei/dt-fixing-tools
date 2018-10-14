package edu.illinois.cs.dt.tools.analysis;

import com.reedoei.eunomia.util.StandardMain;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;

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
        System.out.println(commandQuery("numTests", SQLStatements.COUNT_TESTS));
        System.out.println(commandQuery("numModules", SQLStatements.COUNT_MODULES));
        System.out.println(commandQuery("numProjsResults", SQLStatements.COUNT_MODULES_RESULTS));

        System.out.println(commandQuery("numProjODTests",
                sqlite.statement(SQLStatements.COUNT_PROJECTS_WITH_FLAKY).param("random")));
        System.out.println(commandQuery("numModuleODTests",
                sqlite.statement(SQLStatements.COUNT_MODULES_WITH_FLAKY).param("random")));

        System.out.println(commandQuery("numProjNOTests",
                sqlite.statement(SQLStatements.COUNT_PROJECTS_WITH_FLAKY).param("flaky")));
        System.out.println(commandQuery("numModuleNOTests",
                sqlite.statement(SQLStatements.COUNT_MODULES_WITH_FLAKY).param("flaky")));

        // TODO: Figure out exaclty how this should work
//        System.out.println(commandQuery("numProjODNOTests", SQLStatements.COUNT_PROJECTS_WITH_ODNO));
//        System.out.println(commandQuery("numModuleODNOTests", SQLStatements.COUNT_PROJECTS_WITH_ODNO));

        System.out.println(commandQuery("numODTests",
                sqlite.statement(SQLStatements.COUNT_FLAKY).param("random")));
        System.out.println(commandQuery("numNOTests",
                sqlite.statement(SQLStatements.COUNT_FLAKY).param("flaky")));

        // TODO: Figure out exaclty how this should work
//        System.out.println(commandQuery("numODNOTests", SQLStatements.COUNT_PROJECTS_WITH_ODNO));

        System.out.println(commandQuery("percODTestFailOne", "\\%",
                sqlite.statement(SQLStatements.PROBABILITY_FAILURE).param(1)));
        System.out.println(commandQuery("percODTestFailTen", "\\%",
                sqlite.statement(SQLStatements.PROBABILITY_FAILURE).param(10)));
        System.out.println(commandQuery("percODTestFailTwenty", "\\%",
                sqlite.statement(SQLStatements.PROBABILITY_FAILURE).param(20)));

        System.out.println(commandQuery("numNOTestAllRandOrig", SQLStatements.TOTAL_NO_ORIG_AND_RANDOM));

        System.out.println(commandQuery("percODTests", "\\%",
                sqlite.statement(SQLStatements.PROBABILITY_FIND_FLAKY)));
        // probability of running the order that revealed the most unique number of OD tests (probably 1/100 in most cases) [\newcommand{\percBestODOrder}{5\%\xspace}]
        System.out.println(commandQuery("percBestODOrder", "\\%", sqlite.statement(SQLStatements.PROBABILITY_BEST_RANDOM)));
        System.out.println(commandQuery("percBestNOOrder", "\\%", sqlite.statement(SQLStatements.PROBABILITY_BEST_FLAKY)));


        // TODO: Do the same for NO. Do we want to count all runs as reruns, or just original runs?
//        System.out.println(commandQuery("percODTests",
//                sqlite.statement(SQLStatements.PROBABILITY_FIND_FLAKY)));

        // TODO:  What exaclty does this mean?
        // System.out.println("Does any random order reveal all the ones in original? \n")

        System.out.println(commandQuery("numNOTestOrig",
                sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("flaky").param("flaky")));
        System.out.println(commandQuery("numNOTestRandClassMethod",
                sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("flaky").param("random")));
        System.out.println(commandQuery("numNOTestRandClass",
                sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("flaky").param("random-class")));

        System.out.println(commandQuery("numODTestReverse",
                sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("random").param("reverse")));
        System.out.println(commandQuery("numODTestReverseClass",
                sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("random").param("reverse-class")));
        System.out.println(commandQuery("numODTestRandClassMethod",
                sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("random").param("random")));
        System.out.println(commandQuery("numODTestRandClass",
                sqlite.statement(SQLStatements.COUNT_TESTS_BY_ROUND_TYPE).param("random").param("random-class")));
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
        return String.format("\\newcommand{\\%s}{%s%s%s\\xspace}",
                commandName, preStr,
                procedure.tableQuery().table().get(0).get(0),
                postStr);
    }
}
