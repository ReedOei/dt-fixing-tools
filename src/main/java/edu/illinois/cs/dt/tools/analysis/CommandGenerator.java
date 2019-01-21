package edu.illinois.cs.dt.tools.analysis;

import com.reedoei.eunomia.util.StandardMain;
import edu.illinois.cs.dt.tools.analysis.data.SameClassPackageCounter;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

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

        print(SQLStatements.COUNT_OD_TYPE, "numBrittleTests", "brittle");
        print(SQLStatements.COUNT_OD_TYPE, "numVictimTests", "victim");
        print(SQLStatements.COUNT_OD_TYPE, "numBothTests", "both");

        // TODO: Could display a plot showing how many have each number (though atm all have just 1)
        print(SQLStatements.COUNT_DEPENDENCIES, "numWithSingleDep", 1, 1);
        print(SQLStatements.COUNT_DEPENDENCIES, "numWithAnyDep", 1, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_DEPENDENCIES, "numWithMoreThanOneDep", 2, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_DEPENDENCIES, "numWithNoDep", 0, 0);
        print(SQLStatements.AVERAGE_DEPENDENCIES, "avgNumDep", 1);
        print(SQLStatements.AVERAGE_DEPENDENCIES, "avgNumDepWith0", 0);

        print(SQLStatements.COUNT_CLEANERS, "numWithSingleCleaner", 1, 1);
        print(SQLStatements.COUNT_CLEANERS, "numWithAnyCleaner", 1, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_CLEANERS, "numWithMoreThanOneCleaner", 2, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_CLEANERS, "numWithNoCleaner", 0, 0);
        print(SQLStatements.AVERAGE_CLEANERS, "avgNumCleaners", 1);
        print(SQLStatements.AVERAGE_CLEANERS, "avgNumCleanersWith0", 0);

        print(SQLStatements.COUNT_POLLUTERS, "numWithSinglePolluter", 1, 1);
        print(SQLStatements.COUNT_POLLUTERS, "numWithAnyPolluter", 1, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_POLLUTERS, "numWithMoreThanOnePolluter", 2, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_POLLUTERS, "numWithNoPolluter", 0, 0);
        print(SQLStatements.AVERAGE_POLLUTERS, "avgNumPolluters", 1);
        print(SQLStatements.AVERAGE_POLLUTERS, "avgNumPollutersWith0", 0);

        print(SQLStatements.COUNT_SETTERS, "numWithSingleSetter", 1, 1);
        print(SQLStatements.COUNT_SETTERS, "numWithAnySetter", 1, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_SETTERS, "numWithMoreThanOneSetter", 2, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_SETTERS, "numWithNoSetter", 0, 0);
        print(SQLStatements.AVERAGE_SETTERS, "avgNumSetters", 1);
        print(SQLStatements.AVERAGE_SETTERS, "avgNumSettersWith0", 0);

        final Map<String, String> odTests = queryOdTests();
        final Map<String, List<String>> cleaners = queryCleaners();
        final Map<String, List<String>> dependencies = queryDependencies();

        showCounters("numCleaner", count(cleaners, t -> true));
        showCounters("numDependency", count(dependencies, t -> true));
        showCounters("numPolluter", count(dependencies, t -> odTests.get(t).equals("victim")));
        showCounters("numSetter", count(dependencies, t -> odTests.get(t).equals("brittle")));

        // TODO: Number of tests with a single field
        // TODO: Number of tests with any fields
        // TODO: Number of tests with more than one field

        // TODO: Number of lines of code to fix
        // TODO: Number of times that fixer works
    }

    private void showCounters(final String commandNamePrefix, final Map<String, SameClassPackageCounter> cleanerCounters) {
        final long anySameClassCount = cleanerCounters.values().stream().filter(SameClassPackageCounter::anyClassSame).count();
        final long allSameClassCount = cleanerCounters.values().stream().filter(SameClassPackageCounter::allClassSame).count();
        final long anySamePackageCount = cleanerCounters.values().stream().filter(SameClassPackageCounter::anyPackageSame).count();
        final long allSamePackageCount = cleanerCounters.values().stream().filter(SameClassPackageCounter::allPackageSame).count();

        System.out.println(tools.command(commandNamePrefix + "AnySameClass", String.valueOf(anySameClassCount)));
        System.out.println(tools.command(commandNamePrefix + "AllSameClass", String.valueOf(allSameClassCount)));
        System.out.println(tools.command(commandNamePrefix + "AnySamePackage", String.valueOf(anySamePackageCount)));
        System.out.println(tools.command(commandNamePrefix + "AllSamePackage", String.valueOf(allSamePackageCount)));
    }

    private Map<String, SameClassPackageCounter> count(final Map<String, List<String>> tests, final Predicate<String> pred) {
        final Map<String, SameClassPackageCounter> result = new HashMap<>();

        for (final String testName : tests.keySet()) {
            if (pred.test(testName)) {
                final SameClassPackageCounter counter = new SameClassPackageCounter(testName);
                tests.get(testName).forEach(counter::count);
                result.put(testName, counter);
            }
        }

        return result;
    }

    private <U, V> Map<U, V> mapQuery(final Path path,
                                      final Function<LinkedHashMap<String, String>, U> keyMapper,
                                      final Function<LinkedHashMap<String, String>, V> valueMapper)
            throws SQLException {
        final Map<U, V> result = new HashMap<>();
        final QueryResult queryResult = sqlite.statement(path).tableQuery();

        for (final LinkedHashMap<String, String> row : queryResult.rows()) {
            result.put(keyMapper.apply(row), valueMapper.apply(row));
        }

        return result;
    }

    private Map<String, List<String>> queryCleaners() throws SQLException {
        return mapQuery(SQLStatements.CLEANERS_BY_TEST,
                r -> r.get("test_name"),
                r -> Arrays.asList(r.get("tests").split(",")));
    }

    private Map<String, List<String>> queryDependencies() throws SQLException {
        return mapQuery(SQLStatements.DEPENDENCIES_BY_TEST,
                r -> r.get("test_name"),
                r -> Arrays.asList(r.get("tests").split(",")));
    }

    private Map<String, String> queryOdTests() throws SQLException {
        return mapQuery(SQLStatements.OD_TESTS_GET, r -> r.get("test_name"), r -> r.get("od_type"));
    }

    private void print(final Path path, final String name, final Object... params) throws SQLException {
        System.out.println(tools.commandQuery(name, sqlite.statement(path, params)));
    }
}
