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

        print(SQLStatements.COUNT_OD_TYPE, "numBrittleTests", "brittle", "brittle");
        print(SQLStatements.COUNT_OD_TYPE, "numVictimTests", "victim", "victim");
        print(SQLStatements.COUNT_OD_TYPE, "numDepTests", "any", "any");
        print(SQLStatements.COUNT_OD_TYPE, "numBothTests", "both", "both");

        // TODO: Could display a plot showing how many have each number (though atm all have just 1)
        print(SQLStatements.COUNT_DEPENDENCIES, "numWithSingleDep", 1, 1);
        print(SQLStatements.COUNT_DEPENDENCIES, "numWithAnyDep", 1, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_DEPENDENCIES, "numWithMoreThanOneDep", 2, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_DEPENDENCIES, "numWithNoDep", 0, 0);
        print(SQLStatements.AVERAGE_DEPENDENCIES, "avgNumDep", 1);
        print(SQLStatements.AVERAGE_DEPENDENCIES, "avgNumDepWithZero", 0);

        print(SQLStatements.COUNT_CLEANERS, "numWithSingleCleaner", 1, 1);
        print(SQLStatements.COUNT_CLEANERS, "numWithAnyCleaner", 1, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_CLEANERS, "numWithMoreThanOneCleaner", 2, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_CLEANERS, "numWithNoCleaner", 0, 0);
        print(SQLStatements.AVERAGE_CLEANERS, "avgNumCleaners", 1);
        print(SQLStatements.AVERAGE_CLEANERS, "avgNumCleanersWithZero", 0);

        print(SQLStatements.COUNT_CLEANER_GROUP_SIZE, "numCleanerGroupWithSingleTest", 1, 1);
        print(SQLStatements.COUNT_CLEANER_GROUP_SIZE, "numCleanerGroupWithAnyTest", 1, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_CLEANER_GROUP_SIZE, "numCleanerGroupWithMoreThanOneTest", 2, Integer.MAX_VALUE);
        print(SQLStatements.AVERAGE_CLEANER_GROUP_SIZE, "avgCleanerGroupSize", 1, Integer.MAX_VALUE);
        print(SQLStatements.AVERAGE_CLEANER_GROUP_SIZE, "avgCleanerGroupSizeWithZero", 0, Integer.MAX_VALUE);

        print(SQLStatements.COUNT_POLLUTERS, "numWithSinglePolluter", 1, 1);
        print(SQLStatements.COUNT_POLLUTERS, "numWithAnyPolluter", 1, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_POLLUTERS, "numWithMoreThanOnePolluter", 2, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_POLLUTERS, "numWithNoPolluter", 0, 0);
        print(SQLStatements.AVERAGE_POLLUTERS, "avgNumPolluters", 1);
        print(SQLStatements.AVERAGE_POLLUTERS, "avgNumPollutersWithZero", 0);

        print(SQLStatements.COUNT_SETTERS, "numWithSingleSetter", 1, 1);
        print(SQLStatements.COUNT_SETTERS, "numWithAnySetter", 1, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_SETTERS, "numWithMoreThanOneSetter", 2, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_SETTERS, "numWithNoSetter", 0, 0);
        print(SQLStatements.AVERAGE_SETTERS, "avgNumSetters", 1);
        print(SQLStatements.AVERAGE_SETTERS, "avgNumSettersWithZero", 0);

        final Map<String, String> odTests = queryOdTests();
        final Map<String, List<String>> cleaners = queryCleaners();
        final Map<String, List<String>> dependencies = queryDependencies();

        showCounters("numCleaner", count(cleaners, t -> true));
        showCounters("numDependency", count(dependencies, t -> true));
        showCounters("numPolluter", count(dependencies, t -> odTests.get(t).equals("victim")));
        showCounters("numSetter", count(dependencies, t -> odTests.get(t).equals("brittle")));

        // TODO: Get this stuff working
        final Map<String, List<String>> cleanersByDependency = queryCleanerByDependency("any");
        final Map<String, List<String>> cleanersByPolluter = queryCleanerByDependency("victim");
        final Map<String, List<String>> cleanersBySetter = queryCleanerByDependency("brittle");

        System.out.println(tools.command("numDependencyWithCleaner", String.valueOf(cleanersByDependency.size())));
        System.out.println(tools.command("numPolluterWithCleaner", String.valueOf(cleanersByPolluter.size())));
        System.out.println(tools.command("numSetterWithCleaner", String.valueOf(cleanersBySetter.size())));

        // Note that these numbers may seem weird, but are correct (a single test may be the same dependency for many cleaners)
        showCounters("numCleanerDependency", count(cleanersByDependency, t -> true));
        showCounters("numCleanerPolluter", count(cleanersByPolluter, t -> true));
        showCounters("numCleanerSetter", count(cleanersBySetter, t -> true));

        // Not a typo. All these arguments must be passed twice, due to the way the query is written
        print(SQLStatements.COUNT_DEPENDENCY_TOTAL, "numDependencyTotal", "any", "any");
        print(SQLStatements.COUNT_DEPENDENCY_TOTAL, "numPolluterTotal", "victim", "victim");
        print(SQLStatements.COUNT_DEPENDENCY_TOTAL, "numSetterTotal", "brittle", "brittle");
        print(SQLStatements.COUNT_CLEANER_TOTAL, "numCleanerTotal", "any", "any");
        print(SQLStatements.COUNT_CLEANER_TOTAL, "numCleanerPolluterTotal", "victim", "victim");
        print(SQLStatements.COUNT_CLEANER_TOTAL, "numCleanerSetterTotal", "brittle", "brittle");

        print(SQLStatements.COUNT_FIXER_RESULTS, "numSuccessFix", 1, "%", "%");
        print(SQLStatements.COUNT_FIXER_RESULTS, "numSuccessVictimFix", 1, "victim", "%");
        print(SQLStatements.COUNT_FIXER_RESULTS, "numSuccessVictimNoCleanerFix", 1, "victim", "no_cleaner");
        print(SQLStatements.COUNT_FIXER_RESULTS, "numSuccessVictimCleanerFix", 1, "victim", "has_cleaner");
        print(SQLStatements.COUNT_FIXER_RESULTS, "numSuccessBrittleFix", 1, "brittle", "%");

        print(SQLStatements.COUNT_FIXER_RESULTS, "numFailFix", 0, "%", "%");
        print(SQLStatements.COUNT_FIXER_RESULTS, "numFailVictimFix", 0, "victim", "%");
        print(SQLStatements.COUNT_FIXER_RESULTS, "numFailVictimNoCleanerFix", 0, "victim", "no_cleaner");
        print(SQLStatements.COUNT_FIXER_RESULTS, "numFailVictimCleanerFix", 0, "victim", "has_cleaner");
        print(SQLStatements.COUNT_FIXER_RESULTS, "numFailBrittleFix", 0, "brittle", "%");

        print(SQLStatements.COUNT_FIXED, "numFixed", "%", "%", 1);
        print(SQLStatements.COUNT_FIXED, "numVictimFixed", "victim", "%", 1);
        print(SQLStatements.COUNT_FIXED, "numVictimNoCleanerFixed", "victim", "no_cleaner", 1);
        print(SQLStatements.COUNT_FIXED, "numVictimCleanerFixed", "victim", "has_cleaner", 1);
        print(SQLStatements.COUNT_FIXED, "numBrittleFixed", "brittle", "%", 1);

        print(SQLStatements.COUNT_FIXED, "numNotFixed", 0, "%", "%");
        print(SQLStatements.COUNT_FIXED, "numVictimNotFixed", 0, "victim", "%");
        print(SQLStatements.COUNT_FIXED, "numVictimNoCleanerNotFixed", 0, "victim", "no_cleaner");
        print(SQLStatements.COUNT_FIXED, "numVictimCleanerNotFixed", 0, "victim", "has_cleaner");
        print(SQLStatements.COUNT_FIXED, "numBrittleNotFixed", 0, "brittle", "%");

        print(SQLStatements.COUNT_FIXABLE, "numFixable", "%", "%");
        print(SQLStatements.COUNT_FIXABLE, "numVictimFixable", "victim", "%");
        print(SQLStatements.COUNT_FIXABLE, "numVictimNoCleanerFixable", "victim", "no_cleaner");
        print(SQLStatements.COUNT_FIXABLE, "numVictimCleanerFixable", "victim", "has_cleaner");
        print(SQLStatements.COUNT_FIXABLE, "numBrittleFixable", "brittle", "%");

        print(SQLStatements.AVERAGE_FIX_SIZE, "avgFixSize", "%", "%");
        print(SQLStatements.AVERAGE_FIX_SIZE, "avgVictimFixSize", "victim", "%");
        print(SQLStatements.AVERAGE_FIX_SIZE, "avgVictimNoCleanerFixSize", "victim", "no_cleaner");
        print(SQLStatements.AVERAGE_FIX_SIZE, "avgVictimCleanerFixSize", "victim", "has_cleaner");
        print(SQLStatements.AVERAGE_FIX_SIZE, "avgBrittleFixSize", "brittle", "%");

        print(SQLStatements.COUNT_FIX_STATUS, "numFixNA", "N/A");
        print(SQLStatements.COUNT_FIX_STATUS, "numFixUnknownError", "UNKNOWN ERROR");
        print(SQLStatements.COUNT_FIX_STATUS, "numFixNotFailingOrder", "NOT FAILING ORDER");
        print(SQLStatements.COUNT_FIX_STATUS, "numNoCleanerFixes", "NO CLEANER FIXES");
        print(SQLStatements.COUNT_FIX_STATUS, "numPriorPatchFixed", "PRIOR PATCH FIXED%");
        print(SQLStatements.COUNT_FIX_STATUS, "numInlineSuccessful", "INLINE SUCCESSFUL");
        print(SQLStatements.COUNT_FIX_STATUS, "numInlineFail", "INLINE FAIL");

        print(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithSingleField", "%", 1, 1);
        print(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithSingleFieldVictim", "victim", 1, 1);
        print(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithSingleFieldBrittle", "brittle", 1, 1);

        print(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithAnyField", "%", 1, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithAnyFieldVictim", "victim", 1, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithAnyFieldBrittle", "brittle", 1, Integer.MAX_VALUE);

        print(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithMoreThanOneField", "%", 2, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithMoreThanOneFieldVictim", "victim", 2, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithMoreThanOneFieldBrittle", "brittle", 2, Integer.MAX_VALUE);

        print(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithNoField", "%", 0, 0);
        print(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithNoFieldVictim", "victim", 0, 0);
        print(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithNoFieldBrittle", "brittle", 0, 0);

        print(SQLStatements.AVERAGE_DIAGNOSED_FIELDS, "avgNumFields", "%", 1, Integer.MAX_VALUE);
        print(SQLStatements.AVERAGE_DIAGNOSED_FIELDS, "avgNumFieldsVictim", "victim", 1, Integer.MAX_VALUE);
        print(SQLStatements.AVERAGE_DIAGNOSED_FIELDS, "avgNumFieldsBrittle", "brittle", 1, Integer.MAX_VALUE);

        print(SQLStatements.AVERAGE_DIAGNOSED_FIELDS, "avgNumFieldsWithZero", "%", 0, Integer.MAX_VALUE);
        print(SQLStatements.AVERAGE_DIAGNOSED_FIELDS, "avgNumFieldsWithZeroVictim", "victim", 0, Integer.MAX_VALUE);
        print(SQLStatements.AVERAGE_DIAGNOSED_FIELDS, "avgNumFieldsWithZeroBrittle", "brittle", 0, Integer.MAX_VALUE);
    }

    private Map<String, List<String>> queryCleanerByDependency(final String type) throws SQLException {
        return mapQuery(sqlite.statement(SQLStatements.CLEANERS_BY_DEPENDENCY, type, type), // pass twice, not a typo
                r -> r.get("test_name"),
                r -> Arrays.asList(r.get("tests").split(",")));
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
        return mapQuery(sqlite.statement(path), keyMapper, valueMapper);
    }

    private <U, V> Map<U, V> mapQuery(final Procedure procedure,
                                      final Function<LinkedHashMap<String, String>, U> keyMapper,
                                      final Function<LinkedHashMap<String, String>, V> valueMapper)
            throws SQLException {
        final Map<U, V> result = new HashMap<>();
        final QueryResult queryResult = procedure.tableQuery();

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
