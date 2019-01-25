package edu.illinois.cs.dt.tools.analysis;

import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.eunomia.util.StandardMain;
import edu.illinois.cs.dt.tools.analysis.data.SameClassPackageCounter;
import org.checkerframework.checker.initialization.qual.Initialized;
import org.checkerframework.checker.nullness.qual.UnknownKeyFor;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class CommandGenerator extends StandardMain {
    private static final Path COMMAND_LISTS = Paths.get("command-lists");
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

        count(SQLStatements.COUNT_OD_TYPE, "numBrittleTests", "brittle", "brittle");
        count(SQLStatements.COUNT_OD_TYPE, "numVictimTests", "victim", "victim");
        count(SQLStatements.COUNT_OD_TYPE, "numDepTests", "any", "any");
        count(SQLStatements.COUNT_OD_TYPE, "numBothTests", "both", "both");

        // TODO: Could display a plot showing how many have each number (though atm all have just 1)
        count(SQLStatements.COUNT_DEPENDENCIES, "numWithSingleDep", 1, 1);
        count(SQLStatements.COUNT_DEPENDENCIES, "numWithAnyDep", 1, Integer.MAX_VALUE);
        count(SQLStatements.COUNT_DEPENDENCIES, "numWithMoreThanOneDep", 2, Integer.MAX_VALUE);
        count(SQLStatements.COUNT_DEPENDENCIES, "numWithNoDep", 0, 0);
        print(SQLStatements.AVERAGE_DEPENDENCIES, "avgNumDep", 1);
        print(SQLStatements.AVERAGE_DEPENDENCIES, "avgNumDepWithZero", 0);

        count(SQLStatements.COUNT_CLEANERS, "numWithSingleCleaner", 1, 1);
        count(SQLStatements.COUNT_CLEANERS, "numWithAnyCleaner", 1, Integer.MAX_VALUE);
        count(SQLStatements.COUNT_CLEANERS, "numWithMoreThanOneCleaner", 2, Integer.MAX_VALUE);
        count(SQLStatements.COUNT_CLEANERS, "numWithNoCleaner", 0, 0);
        print(SQLStatements.AVERAGE_CLEANERS, "avgNumCleaners", 1);
        print(SQLStatements.AVERAGE_CLEANERS, "avgNumCleanersWithZero", 0);

        print(SQLStatements.COUNT_CLEANER_GROUP_SIZE, "numCleanerGroupWithSingleTest", 1, 1);
        print(SQLStatements.COUNT_CLEANER_GROUP_SIZE, "numCleanerGroupWithAnyTest", 1, Integer.MAX_VALUE);
        print(SQLStatements.COUNT_CLEANER_GROUP_SIZE, "numCleanerGroupWithMoreThanOneTest", 2, Integer.MAX_VALUE);
        print(SQLStatements.AVERAGE_CLEANER_GROUP_SIZE, "avgCleanerGroupSize", 1, Integer.MAX_VALUE);
        print(SQLStatements.AVERAGE_CLEANER_GROUP_SIZE, "avgCleanerGroupSizeWithZero", 0, Integer.MAX_VALUE);

        count(SQLStatements.COUNT_POLLUTERS, "numWithSinglePolluter", 1, 1);
        count(SQLStatements.COUNT_POLLUTERS, "numWithAnyPolluter", 1, Integer.MAX_VALUE);
        count(SQLStatements.COUNT_POLLUTERS, "numWithMoreThanOnePolluter", 2, Integer.MAX_VALUE);
        count(SQLStatements.COUNT_POLLUTERS, "numWithNoPolluter", 0, 0);
        print(SQLStatements.AVERAGE_POLLUTERS, "avgNumPolluters", 1);
        print(SQLStatements.AVERAGE_POLLUTERS, "avgNumPollutersWithZero", 0);

        count(SQLStatements.COUNT_SETTERS, "numWithSingleSetter", 1, 1);
        count(SQLStatements.COUNT_SETTERS, "numWithAnySetter", 1, Integer.MAX_VALUE);
        count(SQLStatements.COUNT_SETTERS, "numWithMoreThanOneSetter", 2, Integer.MAX_VALUE);
        count(SQLStatements.COUNT_SETTERS, "numWithNoSetter", 0, 0);
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

        printAndWrite("numDependencyWithCleaner", "", new ArrayList<>(cleanersByDependency.keySet()));
        printAndWrite("numPolluterWithCleaner", cleanersByPolluter.keySet());
        printAndWrite("numSetterWithCleaner", cleanersBySetter.keySet());

        // Note that these numbers may seem weird, but are correct (a single test may be the same dependency for many cleaners)
        showCounters("numCleanerDependency", count(cleanersByDependency, t -> true));
        showCounters("numCleanerPolluter", count(cleanersByPolluter, t -> true));
        showCounters("numCleanerSetter", count(cleanersBySetter, t -> true));

        print(SQLStatements.COUNT_DEPENDENCY_TOTAL, "numDependencyTotal", "%");
        print(SQLStatements.COUNT_DEPENDENCY_TOTAL, "numPolluterTotal", "victim");
        print(SQLStatements.COUNT_DEPENDENCY_TOTAL, "numSetterTotal", "brittle");
        print(SQLStatements.COUNT_CLEANER_TOTAL, "numCleanerTotal", "%");
        print(SQLStatements.COUNT_CLEANER_TOTAL, "numCleanerPolluterTotal", "victim");
        print(SQLStatements.COUNT_CLEANER_TOTAL, "numCleanerSetterTotal", "brittle");

        count(SQLStatements.COUNT_FIXER_RESULTS, "numSuccessFix", 1, "%", "%");
        count(SQLStatements.COUNT_FIXER_RESULTS, "numSuccessVictimFix", 1, "victim", "%");
        count(SQLStatements.COUNT_FIXER_RESULTS, "numSuccessVictimNoCleanerFix", 1, "victim", "no_cleaner");
        count(SQLStatements.COUNT_FIXER_RESULTS, "numSuccessVictimCleanerFix", 1, "victim", "has_cleaner");
        count(SQLStatements.COUNT_FIXER_RESULTS, "numSuccessBrittleFix", 1, "brittle", "%");

        count(SQLStatements.COUNT_FIXER_RESULTS, "numFailFix", 0, "%", "%");
        count(SQLStatements.COUNT_FIXER_RESULTS, "numFailVictimFix", 0, "victim", "%");
        count(SQLStatements.COUNT_FIXER_RESULTS, "numFailVictimNoCleanerFix", 0, "victim", "no_cleaner");
        count(SQLStatements.COUNT_FIXER_RESULTS, "numFailVictimCleanerFix", 0, "victim", "has_cleaner");
        count(SQLStatements.COUNT_FIXER_RESULTS, "numFailBrittleFix", 0, "brittle", "%");

        count(SQLStatements.COUNT_FIXED, "numFixed", 1, "%", "%");
        count(SQLStatements.COUNT_FIXED, "numVictimFixed", 1, "victim", "%");
        count(SQLStatements.COUNT_FIXED, "numVictimNoCleanerFixed", 1, "victim", "no_cleaner");
        count(SQLStatements.COUNT_FIXED, "numVictimCleanerFixed", 1, "victim", "has_cleaner");
        count(SQLStatements.COUNT_FIXED, "numBrittleFixed", 1, "brittle", "%");

        count(SQLStatements.COUNT_FIXED, "numNotFixed", 0, "%", "%");
        count(SQLStatements.COUNT_FIXED, "numVictimNotFixed", 0, "victim", "%");
        count(SQLStatements.COUNT_FIXED, "numVictimNoCleanerNotFixed", 0, "victim", "no_cleaner");
        count(SQLStatements.COUNT_FIXED, "numVictimCleanerNotFixed", 0, "victim", "has_cleaner");
        count(SQLStatements.COUNT_FIXED, "numBrittleNotFixed", 0, "brittle", "%");

        count(SQLStatements.COUNT_FIXABLE, "numFixable", "%", "%");
        count(SQLStatements.COUNT_FIXABLE, "numVictimFixable", "victim", "%");
        count(SQLStatements.COUNT_FIXABLE, "numVictimNoCleanerFixable", "victim", "no_cleaner");
        count(SQLStatements.COUNT_FIXABLE, "numVictimCleanerFixable", "victim", "has_cleaner");
        count(SQLStatements.COUNT_FIXABLE, "numBrittleFixable", "brittle", "%");

        print(SQLStatements.AVERAGE_FIX_SIZE, "avgFixSize", "%", "%");
        print(SQLStatements.AVERAGE_FIX_SIZE, "avgVictimFixSize", "victim", "%");
        print(SQLStatements.AVERAGE_FIX_SIZE, "avgVictimNoCleanerFixSize", "victim", "no_cleaner");
        print(SQLStatements.AVERAGE_FIX_SIZE, "avgVictimCleanerFixSize", "victim", "has_cleaner");
        print(SQLStatements.AVERAGE_FIX_SIZE, "avgBrittleFixSize", "brittle", "%");

        count(SQLStatements.COUNT_FIX_STATUS, "numFixNA", "N/A");
        count(SQLStatements.COUNT_FIX_STATUS, "numFixUnknownError", "UNKNOWN ERROR");
        count(SQLStatements.COUNT_FIX_STATUS, "numFixNotFailingOrder", "NOT FAILING ORDER");
        count(SQLStatements.COUNT_FIX_STATUS, "numNoCleanerFixes", "NO CLEANER FIXES");
        count(SQLStatements.COUNT_FIX_STATUS, "numPriorPatchFixed", "PRIOR PATCH FIXED%");
        count(SQLStatements.COUNT_FIX_STATUS, "numInlineSuccessful", "INLINE SUCCESSFUL");
        count(SQLStatements.COUNT_FIX_STATUS, "numInlineFail", "INLINE FAIL");

        count(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithSingleField", "%", 1, 1);
        count(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithSingleFieldVictim", "victim", 1, 1);
        count(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithSingleFieldBrittle", "brittle", 1, 1);

        count(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithAnyField", "%", 1, Integer.MAX_VALUE);
        count(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithAnyFieldVictim", "victim", 1, Integer.MAX_VALUE);
        count(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithAnyFieldBrittle", "brittle", 1, Integer.MAX_VALUE);

        count(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithMoreThanOneField", "%", 2, Integer.MAX_VALUE);
        count(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithMoreThanOneFieldVictim", "victim", 2, Integer.MAX_VALUE);
        count(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithMoreThanOneFieldBrittle", "brittle", 2, Integer.MAX_VALUE);

        count(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithNoField", "%", 0, 0);
        count(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithNoFieldVictim", "victim", 0, 0);
        count(SQLStatements.COUNT_DIAGNOSED_FIELDS, "numWithNoFieldBrittle", "brittle", 0, 0);

        print(SQLStatements.AVERAGE_DIAGNOSED_FIELDS, "avgNumFields", "%", 1, Integer.MAX_VALUE);
        print(SQLStatements.AVERAGE_DIAGNOSED_FIELDS, "avgNumFieldsVictim", "victim", 1, Integer.MAX_VALUE);
        print(SQLStatements.AVERAGE_DIAGNOSED_FIELDS, "avgNumFieldsBrittle", "brittle", 1, Integer.MAX_VALUE);

        print(SQLStatements.AVERAGE_DIAGNOSED_FIELDS, "avgNumFieldsWithZero", "%", 0, Integer.MAX_VALUE);
        print(SQLStatements.AVERAGE_DIAGNOSED_FIELDS, "avgNumFieldsWithZeroVictim", "victim", 0, Integer.MAX_VALUE);
        print(SQLStatements.AVERAGE_DIAGNOSED_FIELDS, "avgNumFieldsWithZeroBrittle", "brittle", 0, Integer.MAX_VALUE);
    }

    private void count(final Path path, final String name, final Object... params)
            throws SQLException, IOException {
        final ListEx<ListEx<String>> table = sqlite.statement(path, params).tableQuery().table();

        Files.createDirectories(COMMAND_LISTS);

        if (table.isEmpty()) {
            printAndWrite(name, new ArrayList<>());
        } else {
            printAndWrite(name, ListEx.transpose(table).get(0));
        }
    }

    private Map<String, List<String>> queryCleanerByDependency(final String type) throws SQLException {
        return mapQuery(sqlite.statement(SQLStatements.CLEANERS_BY_DEPENDENCY, type, type), // pass twice, not a typo
                r -> r.get("test_name"),
                r -> Arrays.asList(r.get("tests").split(",")));
    }

    private void showCounters(final String commandNamePrefix, final Map<String, SameClassPackageCounter> counters)
            throws IOException {
        printAndWrite(commandNamePrefix, "AnySameClass", collect(counters, SameClassPackageCounter::anyClassSame));
        printAndWrite(commandNamePrefix, "AllSameClass", collect(counters, SameClassPackageCounter::allClassSame));
        printAndWrite(commandNamePrefix, "AnySamePackage", collect(counters, SameClassPackageCounter::anyPackageSame));
        printAndWrite(commandNamePrefix, "AllSamePackage", collect(counters, SameClassPackageCounter::allPackageSame));
    }

    private List<String> collect(final Map<String, SameClassPackageCounter> counters,
                                 final Predicate<SameClassPackageCounter> pred) {
        return counters.values().stream()
                .filter(pred)
                .map(SameClassPackageCounter::testName)
                .collect(Collectors.toList());
    }

    private void printAndWrite(final String name, final Collection<String> collection) throws IOException {
        printAndWrite(name, "", collection);
    }

    private void printAndWrite(final String name, final String suffix, final Collection<String> collection)
            throws IOException {
        System.out.println(tools.command(name + suffix, String.valueOf(collection.size())));

        Files.createDirectories(COMMAND_LISTS);
        Files.write(COMMAND_LISTS.resolve(name + suffix), collection);
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
