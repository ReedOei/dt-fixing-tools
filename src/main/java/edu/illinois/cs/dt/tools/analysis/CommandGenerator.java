package edu.illinois.cs.dt.tools.analysis;

import com.reedoei.eunomia.util.StandardMain;
import edu.illinois.cs.dt.tools.analysis.data.SameClassPackageCounter;

import java.io.IOException;
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
import java.util.stream.Collectors;

public class CommandGenerator extends StandardMain {
    private final SQLite sqlite;
    private final String commandPrefix;
    private final LatexTools tools;
    private final CommandFormatterFactory factory;
    private final Map<String, Double> commandValues = new HashMap<>();
    private final NumberFormat percentInstance;
    private final NumberFormat ratioInstance;

    private CommandGenerator(final String[] args) throws SQLException {
        super(args);

        this.sqlite = new SQLite(Paths.get(getArgRequired("db")));
        this.commandPrefix = getArg("prefix").orElse("");
        this.tools = new LatexTools(sqlite, commandPrefix);
        this.factory = new CommandFormatterFactory(commandValues, commandPrefix, tools, sqlite);

        this.percentInstance = NumberFormat.getPercentInstance();
        percentInstance.setMaximumFractionDigits(1);
        this.ratioInstance = NumberFormat.getNumberInstance();
        ratioInstance.setMaximumFractionDigits(1);
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
        factory.create(SQLStatements.COUNT_OD_TYPE)
                .count("numBrittleTests", "brittle", "brittle")
                .count("numVictimTests", "victim", "victim")
                .count("numDepTests", "any", "any")
                .count("numBothTests", "both", "both")
                .finishGroup();

        factory.create(SQLStatements.COUNT_DEPENDENCIES)
                .count("numWithSingleDep", 1, 1)
                .count("numWithAnyDep", 1, Integer.MAX_VALUE)
                .count("numWithMoreThanOneDep", 2, Integer.MAX_VALUE)
                .count("numWithNoDep", 0, 0)
                .finishGroup();

        factory.create(SQLStatements.AVERAGE_DEPENDENCIES)
                .print("avgNumDep", 1)
                .print("avgNumDepZero", 0)
                .finishGroup();

        factory.create(SQLStatements.COUNT_CLEANERS)
                .count("numWithSingleCleaner", 1, 1)
                .count("numWithAnyCleaner", 1, Integer.MAX_VALUE)
                .count("numWithMoreThanOneCleaner", 2, Integer.MAX_VALUE)
                .count("numWithNoCleaner", 0, 0)
                .finishGroup();

        factory.create(SQLStatements.AVERAGE_CLEANERS)
                .print("avgNumCleaners", 1)
                .print("avgNumCleanersWithZero", 0)
                .finishGroup();

        factory.create(SQLStatements.COUNT_CLEANER_GROUP_SIZE)
                .count("numCleanerGroupWithSingleTest", 1, 1)
                .count("numCleanerGroupWithAnyTest", 1, Integer.MAX_VALUE)
                .count("numCleanerGroupWithMoreThanOneTest", 2, Integer.MAX_VALUE)
                .finishGroup();

        factory.create(SQLStatements.COUNT_DEPENDENCY_GROUP_SIZE)
                .count("numDepGroupWithSingleTest", "%", 1, 1)
                .count("numDepGroupWithAnyTest", "%", 1, Integer.MAX_VALUE)
                .count("numDepGroupWithMoreThanOneTest", "%", 2, Integer.MAX_VALUE)
                .finishGroup()
                .count("numPolluterGroupWithSingleTest", "victim", 1, 1)
                .count("numPolluterGroupWithAnyTest", "victim", 1, Integer.MAX_VALUE)
                .count("numPolluterGroupWithMoreThanOneTest", "victim", 2, Integer.MAX_VALUE)
                .finishGroup()
                .count("numSetterGroupWithSingleTest", "brittle", 1, 1)
                .count("numSetterGroupWithAnyTest", "brittle", 1, Integer.MAX_VALUE)
                .count("numSetterGroupWithMoreThanOneTest", "brittle", 2, Integer.MAX_VALUE)
                .finishGroup();

        factory.create(SQLStatements.AVERAGE_CLEANER_GROUP_SIZE)
                .print("avgNonSingletonCleanerGroupSize", 2)
                .print("avgCleanerGroupSize", 1)
                .print("avgCleanerGroupSizeWithZero", 0)
                .finishGroup();

        factory.create(SQLStatements.COUNT_POLLUTERS)
                .count("numWithSinglePolluter", 1, 1)
                .count("numWithAnyPolluter", 1, Integer.MAX_VALUE)
                .count("numWithMoreThanOnePolluter", 2, Integer.MAX_VALUE)
                .count("numWithNoPolluter", 0, 0)
                .finishGroup();

        factory.create(SQLStatements.AVERAGE_POLLUTERS)
                .print("avgNumPolluters", 1)
                .print("avgNumPollutersWithZero", 0)
                .finishGroup();

        // TODO: Add averages (for any, more than zero, more than one) for all of these
        factory.create(SQLStatements.COUNT_DEPENDENCY_GROUP)
                .count("numODSingleDepGroup", "%", 0, Integer.MAX_VALUE, 1, 1)
                .count("numODAnyDepGroup", "%", 0, Integer.MAX_VALUE, 1, Integer.MAX_VALUE)
                .count("numODMoreThanOneDepGroup", "%", 0, Integer.MAX_VALUE, 2, Integer.MAX_VALUE)
                .count("numODNoDepGroup", "%", 0, Integer.MAX_VALUE, 0, 0)
                .finishGroup()
                .count("numVictimSinglePolluterGroup", "victim", 0, Integer.MAX_VALUE, 1, 1)
                .count("numVictimAnyPolluterGroup", "victim", 0, Integer.MAX_VALUE, 1, Integer.MAX_VALUE)
                .count("numVictimMoreThanOnePolluterGroup", "victim", 0, Integer.MAX_VALUE, 2, Integer.MAX_VALUE)
                .count("numVictimNoPolluterGroup", "victim", 0, Integer.MAX_VALUE, 0, 0)
                .finishGroup()
                .count("numBrittleSingleSetterGroup", "brittle", 0, Integer.MAX_VALUE, 1, 1)
                .count("numBrittleAnySetterGroup", "brittle", 0, Integer.MAX_VALUE, 1, Integer.MAX_VALUE)
                .count("numBrittleMoreThanOneSetterGroup", "brittle", 0, Integer.MAX_VALUE, 2, Integer.MAX_VALUE)
                .count("numBrittleNoSetterGroup", "brittle", 0, Integer.MAX_VALUE, 0, 0)
                .finishGroup()
                .count("numODWithSingletonDepGroup", "%", 1, 1, 0, Integer.MAX_VALUE)
                .count("numODWithNonSingletonDepGroup", "%", 2, Integer.MAX_VALUE, 0, Integer.MAX_VALUE)
                .finishGroup()
                .count("numVictimSingletonPolluterGroup", "victim", 1, 1, 0, Integer.MAX_VALUE)
                .count("numVictimNonSingletonPolluterGroup", "victim", 2, Integer.MAX_VALUE, 0, Integer.MAX_VALUE)
                .finishGroup()
                .count("numBrittleSingletonSetterGroup", "brittle", 1, 1, 0, Integer.MAX_VALUE)
                .count("numBrittleNonSingletonSetterGroup", "brittle", 2, Integer.MAX_VALUE, 0, Integer.MAX_VALUE)
                .finishGroup();

        factory.create(SQLStatements.AVERAGE_DEP_GROUP_SIZE)
                .print("avgNumDepInNonSingletonGroup", "%", 2, Integer.MAX_VALUE)
                .print("avgNumPolluterInNonSingletonGroup", "victim", 2, Integer.MAX_VALUE)
                .print("avgNumSetterInNonSingletonGroup", "brittle", 2, Integer.MAX_VALUE)
                .finishGroup();

        factory.create(SQLStatements.COUNT_CLEANER_BY_POLLUTER)
                .print("numPolluterSingleCleanerGroup", 0, Integer.MAX_VALUE, 1, 1)
                .print("numPolluterAnyCleanerGroup", 0, Integer.MAX_VALUE, 1, Integer.MAX_VALUE)
                .print("numPolluterMultipleCleanerGroup", 0, Integer.MAX_VALUE, 2, Integer.MAX_VALUE)
                .print("numPolluterNoCleanerGroup", 0, Integer.MAX_VALUE, 0, 0)
                .finishGroup()
                .print("numPolluterSingletonCleanerGroup", 1, 1, 0, Integer.MAX_VALUE)
                .print("numPolluterNonSingletonCleanerGroup", 2, Integer.MAX_VALUE, 0, Integer.MAX_VALUE)
                .finishGroup();

        factory.create(SQLStatements.COUNT_CLEANER_BY_VICTIM)
                .count("numVictimSingleCleanerGroup", 0, Integer.MAX_VALUE, 1, 1)
                .count("numVictimAnyCleanerGroup", 0, Integer.MAX_VALUE, 1, Integer.MAX_VALUE)
                .count("numVictimMultipleCleanerGroup", 0, Integer.MAX_VALUE, 2, Integer.MAX_VALUE)
                .count("numVictimNoCleanerGroup", 0, Integer.MAX_VALUE, 0, 0)
                .finishGroup()
                .count("numVictimSingletonCleanerGroup", 1, 1, 0, Integer.MAX_VALUE)
                .count("numVictimNonSingletonCleanerGroup", 2, Integer.MAX_VALUE, 0, Integer.MAX_VALUE)
                .finishGroup();

        factory.create(SQLStatements.COUNT_SETTERS)
                .count("numWithSingleSetter", 1, 1)
                .count("numWithAnySetter", 1, Integer.MAX_VALUE)
                .count("numWithMoreThanOneSetter", 2, Integer.MAX_VALUE)
                .count("numWithNoSetter", 0, 0)
                .finishGroup();

        factory.create(SQLStatements.AVERAGE_SETTERS)
                .print("avgNumSetters", 1)
                .print("avgNumSettersWithZero", 0)
                .finishGroup();

        final Map<String, String> odTests = queryOdTests();
        final Map<String, List<String>> cleaners = queryCleaners();
        final Map<String, List<String>> dependencies = queryDependencies();

        showCounters("numCleaner", count(cleaners, t -> true));
        showCounters("numDependency", count(dependencies, t -> true));
        showCounters("numPolluter", count(dependencies, t -> odTests.get(t).equals("victim")));
        showCounters("numSetter", count(dependencies, t -> odTests.get(t).equals("brittle")));

        final Map<String, List<String>> cleanersByDependency = queryCleanerByDependency("%");
        final Map<String, List<String>> cleanersByPolluter = queryCleanerByDependency("victim");
        final Map<String, List<String>> cleanersBySetter = queryCleanerByDependency("brittle");

        // Note that these numbers may seem weird, but are correct (a single test may be the same dependency for many cleaners)
        showCounters("numCleanerDependency", count(cleanersByDependency, t -> true));
        showCounters("numCleanerPolluter", count(cleanersByPolluter, t -> true));
        showCounters("numCleanerSetter", count(cleanersBySetter, t -> true));

        factory.create(SQLStatements.COUNT_DEPENDENCY_TOTAL)
                .print("numDependencyTotal", "%")
                .print("numPolluterTotal", "victim")
                .print("numSetterTotal", "brittle")
                .finishGroup();

        factory.create(SQLStatements.COUNT_CLEANER_TOTAL)
                .print("numCleanerTotal", "%")
                .print("numCleanerPolluterTotal", "victim")
                .print("numCleanerSetterTotal", "brittle")
                .finishGroup();

        factory.create(SQLStatements.COUNT_FIXER_RESULTS)
                .count("numSuccessFix", 1, "%", "%")
                .count("numSuccessVictimFix", 1, "victim", "%")
                .count("numSuccessVictimNoCleanerFix", 1, "victim", "no_cleaner")
                .count("numSuccessVictimCleanerFix", 1, "victim", "has_cleaner")
                .count("numSuccessBrittleFix", 1, "brittle", "%")
                .finishGroup()
                .count("numFailFix", 0, "%", "%")
                .count("numFailVictimFix", 0, "victim", "%")
                .count("numFailVictimNoCleanerFix", 0, "victim", "no_cleaner")
                .count("numFailVictimCleanerFix", 0, "victim", "has_cleaner")
                .count("numFailBrittleFix", 0, "brittle", "%")
                .finishGroup();

        factory.create(SQLStatements.COUNT_FIXED)
                .count("numFixed", 1, "%", "%")
                .count("numVictimFixed", 1, "victim", "%")
                .count("numVictimNoCleanerFixed", 1, "victim", "no_cleaner")
                .count("numVictimCleanerFixed", 1, "victim", "has_cleaner")
                .count("numBrittleFixed", 1, "brittle", "%")
                .finishGroup()
                .count("numNotFixed", 0, "%", "%")
                .count("numVictimNotFixed", 0, "victim", "%")
                .count("numVictimNoCleanerNotFixed", 0, "victim", "no_cleaner")
                .count("numVictimCleanerNotFixed", 0, "victim", "has_cleaner")
                .count("numBrittleNotFixed", 0, "brittle", "%")
                .finishGroup();

        factory.create(SQLStatements.COUNT_FIXABLE)
                .count("numFixable", "%", "%")
                .count("numVictimFixable", "victim", "%")
                .count("numVictimNoCleanerFixable", "victim", "no_cleaner")
                .count("numVictimCleanerFixable", "victim", "has_cleaner")
                .count("numBrittleFixable", "brittle", "%")
                .finishGroup();

        factory.create(SQLStatements.AVERAGE_FIX_SIZE)
                .print("avgFixSize", "%", "%")
                .print("avgVictimFixSize", "victim", "%")
                .print("avgVictimNoCleanerFixSize", "victim", "no_cleaner")
                .print("avgVictimCleanerFixSize", "victim", "has_cleaner")
                .print("avgBrittleFixSize", "brittle", "%")
                .finishGroup();

        factory.create(SQLStatements.COUNT_FIX_STATUS)
                .count("numFixNA", "N/A")
                .count("numFixUnknownError", "UNKNOWN ERROR")
                .count("numFixNotFailingOrder", "NOT FAILING ORDER")
                .count("numNoCleanerFixes", "NO CLEANER FIXES")
                .count("numPriorPatchFixed", "PRIOR PATCH FIXED%")
                .count("numInlineSuccessful", "INLINE SUCCESSFUL")
                .count("numInlineFail", "INLINE FAIL")
                .finishGroup();

        factory.create(SQLStatements.COUNT_DIAGNOSED_FIELDS)
                .count("numODRequireSingleField", "%", 1, 1)
                .count("numODRequireAnyField", "%", 1, Integer.MAX_VALUE)
                .count("numODRequireMoreThanOneField", "%", 2, Integer.MAX_VALUE)
                .count("numODNoField", "%", 0, 0)
                .finishGroup()
                .count("numVictimRequireSingleField", "victim", 1, 1)
                .count("numVictimRequireAnyField", "victim", 1, Integer.MAX_VALUE)
                .count("numVictimRequireMoreThanOneField", "victim", 2, Integer.MAX_VALUE)
                .count("numVictimNoField", "victim", 0, 0)
                .finishGroup()
                .count("numBrittleRequireSingleField", "brittle", 1, 1)
                .count("numBrittleRequireAnyField", "brittle", 1, Integer.MAX_VALUE)
                .count("numBrittleRequireMoreThanOneField", "brittle", 2, Integer.MAX_VALUE)
                .count("numBrittleNoField", "brittle", 0, 0)
                .finishGroup();

        factory.create(SQLStatements.COUNT_DIAGNOSED_FIELDS_NUM)
                .count("numODSingleField", "%", 1, 1)
                .count("numODMultipleField", "%", 2, Integer.MAX_VALUE)
                .finishGroup()
                .count("numVictimSingleField", "victim", 1, 1)
                .count("numVictimMultipleField", "victim", 2, Integer.MAX_VALUE)
                .finishGroup()
                .count("numBrittleSingleField", "brittle", 1, 1)
                .count("numBrittleMultipleField", "brittle", 2, Integer.MAX_VALUE)
                .finishGroup();

        factory.create(SQLStatements.AVERAGE_DIAGNOSED_FIELDS)
                .print("avgNumFields", "%", 1, Integer.MAX_VALUE)
                .print("avgNumFieldsVictim", "victim", 1, Integer.MAX_VALUE)
                .print("avgNumFieldsBrittle", "brittle", 1, Integer.MAX_VALUE)
                .finishGroup()
                .print("avgNumFieldsWithZero", "%", 0, Integer.MAX_VALUE)
                .print("avgNumFieldsWithZeroVictim", "victim", 0, Integer.MAX_VALUE)
                .print("avgNumFieldsWithZeroBrittle", "brittle", 0, Integer.MAX_VALUE)
                .finishGroup();

        // TODO: How many tests that we didn't fix or didn't have a cleaner/setter have any fields
    }

    private void printRatio(final String name, final String n, final String d) {
        printDerived(name, n, d, ratioInstance);
    }

    private void printPercentage(final String name, final String n, final String d) {
        printDerived(name, n, d, percentInstance);
    }

    private void printDerived(final String name, final String n, final String d, final NumberFormat percentInstance) {
        final double nVal = commandValues.get(n);
        final double dVal = commandValues.get(d);

        System.out.println(tools.command(name, percentInstance.format(nVal / dVal)));
    }

    private Map<String, List<String>> queryCleanerByDependency(final String type) throws SQLException {
        return mapQuery(sqlite.statement(SQLStatements.CLEANERS_BY_DEPENDENCY, type), // pass twice, not a typo
                r -> r.get("cleaners"),
                r -> Arrays.asList(r.get("deps").split(",")));
    }

    private void showCounters(final String commandNamePrefix, final Map<String, SameClassPackageCounter> counters)
            throws IOException {
        factory.create(null)
                .printAndWrite(commandNamePrefix, "AnySameClass", collect(counters, SameClassPackageCounter::anyClassSame))
                .printAndWrite(commandNamePrefix, "AllSameClass", collect(counters, SameClassPackageCounter::allClassSame))
                .printAndWrite(commandNamePrefix, "AnySamePackage", collect(counters, SameClassPackageCounter::anyPackageSame))
                .printAndWrite(commandNamePrefix, "AllSamePackage", collect(counters, SameClassPackageCounter::allPackageSame))
                .finishGroup();
    }

    private List<String> collect(final Map<String, SameClassPackageCounter> counters,
                                 final Predicate<SameClassPackageCounter> pred) {
        return counters.values().stream()
                .filter(pred)
                .map(SameClassPackageCounter::testName)
                .collect(Collectors.toList());
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
}
