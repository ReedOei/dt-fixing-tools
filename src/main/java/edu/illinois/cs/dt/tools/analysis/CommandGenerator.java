package edu.illinois.cs.dt.tools.analysis;

import com.reedoei.eunomia.util.StandardMain;
import edu.illinois.cs.dt.tools.analysis.data.SameClassPackageCounter;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class CommandGenerator extends StandardMain {
    private final SQLite sqlite;
    private final String commandPrefix;
    private final LatexTools tools;
    private final CommandFormatterFactory factory;
    private final Map<String, Double> commandValues = new HashMap<>();
    private final CommandCalculator calc;

    private CommandGenerator(final String[] args) throws SQLException {
        super(args);

        this.sqlite = new SQLite(Paths.get(getArgRequired("db")));
        this.commandPrefix = getArg("prefix").orElse("");
        this.tools = new LatexTools(sqlite, commandPrefix);
        this.factory = new CommandFormatterFactory(commandValues, commandPrefix, tools, sqlite);
        this.calc = new CommandCalculator(commandValues, tools);
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
        factory.create(SQLStatements.COUNT_NO_TEST)
                .count("numNOTest")
                .finishGroup();

        factory.create(SQLStatements.COUNT_PR_TESTS)
                .count("numPRTests")
                .finishGroup();

        factory.create(SQLStatements.COUNT_FIXED_METHOD_ORDER_TESTS)
                .count("numFixedMethodOrderTests")
                .finishGroup();

        factory.create(SQLStatements.COUNT_UNFINISHED_TESTS)
                .count("numUnfinishedTests")
                .finishGroup();

        factory.create(SQLStatements.COUNT_INCOMPATIBLE_TESTS)
                .count("numIncompatibleTests")
                .finishGroup();

        factory.create(SQLStatements.COUNT_SEPARATE_JVM_TESTS)
                .count("numSeparateJVMTests")
                .finishGroup();

        factory.create(SQLStatements.COUNT_FIXED_ONLY_PATCHER)
                .count("numFixedOnlyPatcher", 1, "%", "%")
                .finishGroup();

        factory.create(SQLStatements.COUNT_FIXED_ONLY_PRS)
                .count("numFixedOnlyPRs", 1, "%", "%")
                .finishGroup();

        factory.create(SQLStatements.COUNT_FIXED_BOTH)
                .count("numFixedBoth", 1, "%", "%")
                .finishGroup();

        factory.create(SQLStatements.COUNT_MODULE_WITH_OD)
                .count("numModulesWithOD")
                .finishGroup();

        factory.create(SQLStatements.COUNT_PROJECT_WITH_OD)
                .count("numProjectsWithOD")
                .finishGroup();

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
                .printDouble("avgNumDep", 1)
                .printDouble("avgNumDepZero", 0)
                .finishGroup();

        factory.create(SQLStatements.COUNT_CLEANERS)
                .count("numWithSingleCleaner", 1, 1)
                .count("numWithAnyCleaner", 1, Integer.MAX_VALUE)
                .count("numWithMoreThanOneCleaner", 2, Integer.MAX_VALUE)
                .count("numWithNoCleaner", 0, 0)
                .finishGroup();

        factory.create(SQLStatements.AVERAGE_CLEANERS)
                .printDouble("avgNumCleaners", 1)
                .printDouble("avgNumCleanersWithZero", 0)
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
//                .printDouble("avgNonSingletonCleanerGroupSize", 2)
                .printDouble("avgCleanerGroupSize", 1)
                .printDouble("avgCleanerGroupSizeWithZero", 0)
                .finishGroup();

        factory.create(SQLStatements.COUNT_POLLUTERS)
                .count("numWithSinglePolluter", 1, 1)
                .count("numWithAnyPolluter", 1, Integer.MAX_VALUE)
                .count("numWithMoreThanOnePolluter", 2, Integer.MAX_VALUE)
                .count("numWithNoPolluter", 0, 0)
                .finishGroup();

        factory.create(SQLStatements.AVERAGE_POLLUTERS)
                .printDouble("avgNumPolluters", 1)
                .printDouble("avgNumPollutersWithZero", 0)
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
                .printDouble("avgNumDepInNonSingletonGroup", "%", 2, Integer.MAX_VALUE)
                .printDouble("avgNumPolluterInNonSingletonGroup", "victim", 2, Integer.MAX_VALUE)
                .printDouble("avgNumSetterInNonSingletonGroup", "brittle", 2, Integer.MAX_VALUE)
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

        final Map<String, String> testToCleanerCount = moduleToPRs(SQLStatements.COUNT_MAX_CLEANER, "test_name",
                                                                   "num", 0,
                                                                   Integer.MAX_VALUE, 2, Integer.MAX_VALUE);
        printMedianMax(testToCleanerCount, "Cleaner");

        final Map<String, String> testToPolluterCount = moduleToPRs(SQLStatements.COUNT_MAX_POLLUTER_VICTIM, "test_name",
                                                                   "num", "victim", 0,
                                                                    Integer.MAX_VALUE, 2, Integer.MAX_VALUE);
        printMedianMax(testToPolluterCount , "Polluter");

        final Map<String, String> testToSetterCount = moduleToPRs(SQLStatements.COUNT_MAX_POLLUTER_VICTIM, "test_name",
                                                                    "num", "brittle", 0,
                                                                  Integer.MAX_VALUE, 2, Integer.MAX_VALUE);
        printMedianMax(testToSetterCount , "Setter");

        factory.create(SQLStatements.COUNT_SETTERS)
                .count("numWithSingleSetter", 1, 1)
                .count("numWithAnySetter", 1, Integer.MAX_VALUE)
                .count("numWithMoreThanOneSetter", 2, Integer.MAX_VALUE)
                .count("numWithNoSetter", 0, 0)
                .finishGroup();

        factory.create(SQLStatements.AVERAGE_SETTERS)
                .printDouble("avgNumSetters", 1)
                .printDouble("avgNumSettersWithZero", 0)
                .finishGroup();


        final Map<String, String> moduleToOpenedPRs = moduleToPRs(SQLStatements.PRS_GET_STATUS, "subject_name","pCount", "Opened");
        final Map<String, String> moduleToAcceptedPRs = moduleToPRs(SQLStatements.PRS_GET_STATUS, "subject_name","pCount", "Accepted");

        final Map<String, String> moduleToOpenedTests = moduleToPRs(SQLStatements.PRS_GET_TESTS, "subject_name","tCount", "Opened");
        final Map<String, String> moduleToAcceptedTests = moduleToPRs(SQLStatements.PRS_GET_TESTS, "subject_name","tCount", "Accepted");

        final Map<String, String> moduleToPatchedTests = moduleToPRs(SQLStatements.FIXED_TESTS_BY_MOD, "subject_name","tCount", 1, "%", "%");

        Set<String> keys = new HashSet<>(moduleToOpenedPRs.keySet());
        keys.addAll(moduleToAcceptedPRs.keySet());
        keys.addAll(moduleToPatchedTests.keySet());

        int openedPRCount = 0;
        int acceptedPRCount = 0;
        int openedTestCount = 0;
        int acceptedTestCount = 0;
        int patchedTestCount = 0;

        System.out.println("% Commands for PR table");
        for (String moduleName : keys) {
            String openedPR = moduleToOpenedPRs.getOrDefault(moduleName, "0");
            String acceptedPR = moduleToAcceptedPRs.getOrDefault(moduleName, "0");

            String openedTests = moduleToOpenedTests.getOrDefault(moduleName, "0");
            String acceptedTests = moduleToAcceptedTests.getOrDefault(moduleName, "0");

            String patchedTests = moduleToPatchedTests.getOrDefault(moduleName, "0");

            String prettyName = moduleName.replace("-", "").toLowerCase();
            System.out.println(tools.command(prettyName + "OpenedPRs", openedPR));
            System.out.println(tools.command(prettyName + "AcceptedPRs", acceptedPR));
            System.out.println(tools.command(prettyName + "OpenedTests", openedTests));
            System.out.println(tools.command(prettyName + "AcceptedTests", acceptedTests));
            System.out.println(tools.command(prettyName + "PatchedTests", patchedTests));

            openedPRCount += Integer.valueOf(openedPR);
            acceptedPRCount += Integer.valueOf(acceptedPR);
            openedTestCount += Integer.valueOf(openedTests);
            acceptedTestCount += Integer.valueOf(acceptedTests);
            patchedTestCount += Integer.valueOf(patchedTests);
        }
        System.out.println(tools.command("totalOpenedPRs", String.valueOf(openedPRCount)));
        System.out.println(tools.command("totalAcceptedPRs", String.valueOf(acceptedPRCount)));
        System.out.println(tools.command("totalPRs", String.valueOf(acceptedPRCount+openedPRCount)));
        System.out.println(tools.command("totalOpenedTests", String.valueOf(openedTestCount)));
        System.out.println(tools.command("totalAcceptedTests", String.valueOf(acceptedTestCount)));
        System.out.println(tools.command("totalPatchedTests", String.valueOf(patchedTestCount)));

        System.out.println("% End for PR table");

        String[] values = new String[]{"test_count", "totalCount", "victim_count", "brittle_count", "polluter_count",
                "cleaner_count", "setter_count", "vic_with_clean_count"};
        final Map<String, List<String>> subjectToRow = subjectRows(SQLStatements.SUBJECT_INFO, "subject_name", values);

        System.out.println("% Commands for subject info table");
        for (String subjectName : subjectToRow.keySet()) {
            List<String> results = subjectToRow.get(subjectName);
            String prettyName = subjectName.replace("-", "").replace("/","").toLowerCase();

            System.out.println(tools.command(prettyName + "TestCount", results.get(0)));
            System.out.println(tools.command(prettyName + "TotalCount", results.get(1)));
            System.out.println(tools.command(prettyName + "VictimCount", results.get(2)));
            System.out.println(tools.command(prettyName + "BrittleCount", results.get(3)));
            System.out.println(tools.command(prettyName + "PolluterCount", results.get(4)));
            System.out.println(tools.command(prettyName + "CleanerCount", results.get(5)));
            System.out.println(tools.command(prettyName + "SetterCount", results.get(6)));
            System.out.println(tools.command(prettyName + "VicWithCleanCount", results.get(7)));
        }

        System.out.println("% End for subject info table");


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
                .printDouble("avgFixSize", "%", "%")
                .printDouble("avgVictimFixSize", "victim", "%")
                .printDouble("avgVictimNoCleanerFixSize", "victim", "no_cleaner")
                .printDouble("avgVictimCleanerFixSize", "victim", "has_cleaner")
                .printDouble("avgBrittleFixSize", "brittle", "%")
                .finishGroup();

        factory.create(SQLStatements.MIN_FIX_SIZE)
                .print("minFixSize", "%", "%")
                .print("minVictimFixSize", "victim", "%")
                .print("minVictimNoCleanerFixSize", "victim", "no_cleaner")
                .print("minVictimCleanerFixSize", "victim", "has_cleaner")
                .print("minBrittleFixSize", "brittle", "%")
                .finishGroup();

        factory.create(SQLStatements.MAX_FIX_SIZE)
                .print("maxFixSize", "%", "%")
                .print("maxVictimFixSize", "victim", "%")
                .print("maxVictimNoCleanerFixSize", "victim", "no_cleaner")
                .print("maxVictimCleanerFixSize", "victim", "has_cleaner")
                .print("maxBrittleFixSize", "brittle", "%")
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
                .printDouble("avgNumFields", "%", 1, Integer.MAX_VALUE)
                .printDouble("avgNumFieldsVictim", "victim", 1, Integer.MAX_VALUE)
                .printDouble("avgNumFieldsBrittle", "brittle", 1, Integer.MAX_VALUE)
                .finishGroup()
                .printDouble("avgNumFieldsWithZero", "%", 0, Integer.MAX_VALUE)
                .printDouble("avgNumFieldsWithZeroVictim", "victim", 0, Integer.MAX_VALUE)
                .printDouble("avgNumFieldsWithZeroBrittle", "brittle", 0, Integer.MAX_VALUE)
                .finishGroup();

        factory.create(SQLStatements.COUNT_DIAGNOSED_NON_FIXABLE)
                .count("numDiagnosedNonFixable", "%")
                .count("numDiagnosedVictimNonFixable", "victim")
                .count("numDiagnosedBrittleNonFixable", "brittle")
                .finishGroup();

        factory.create(SQLStatements.COUNT_DIAGNOSED_UNFIXED)
                .count("numDiagnosedUnfixed", "%")
                .count("numDiagnosedVictimUnfixed", "victim")
                .count("numDiagnosedBrittleUnfixed", "brittle")
                .finishGroup();

        final int numNonFixable = (int) calc.sub("numDepTests", "numFixable");
        System.out.println(tools.command("numNonFixable", String.valueOf(numNonFixable)));
        calc.printPercentage("percFixed", "numFixed", "numFixable");
        calc.printPercentage("percNotFixed", "numNotFixed", "numFixable");
        calc.printPercentage("percFixedAll", "numSuccessFix", "numDepTests");
        calc.printPercentage("percNotFixedAll", "numFailFix", "numDepTests");

        // TODO: automated sanity checks
        // TODO: Union of any polluter OR cleaner in the same test class as the victim
    }

    private void printMedianMax(Map<String, String> testToCount, String type) {
        List<Integer> counts = testToCount.values().stream().map(Integer::parseInt).collect(Collectors.toList());
        Collections.sort(counts);
        int lastIndex = counts.size() - 1;
        int medianIndex = lastIndex / 2;
        System.out.println(tools.command("num" + type + "MaxForVictims", String.valueOf(counts.get(lastIndex))));
        System.out.println(tools.command("num" + type + "MedianForVictims", String.valueOf(counts.get(medianIndex))));
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
                                      final Function<LinkedHashMap<String, String>, V> valueMapper,
                                      final Object... params)
            throws SQLException {
        return mapQuery(sqlite.statement(path, params), keyMapper, valueMapper);
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

    private <U> Map<U, LinkedHashMap<String, String>> mapQuery(final Procedure procedure,
                                      final Function<LinkedHashMap<String, String>, U> keyMapper)
            throws SQLException {
        final Map<U, LinkedHashMap<String, String>> result = new HashMap<>();
        final QueryResult queryResult = procedure.tableQuery();

        for (final LinkedHashMap<String, String> row : queryResult.rows()) {
            result.put(keyMapper.apply(row), row);
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

    private Map<String, String> moduleToPRs(Path query, String key, String typeToMapTo, final Object... params) throws SQLException {
        return mapQuery(query, r -> r.get(key), r -> r.get(typeToMapTo), params);
    }

    private Map<String, List<String>> subjectRows(Path query, String key, String[] values, final Object... params) throws SQLException {
        Map<String, List<String>> subjectToResults = new LinkedHashMap<>();
        Map<String, LinkedHashMap<String, String>> subjectToRow = mapQuery(sqlite.statement(query, params), r -> r.get(key));

        for (String subject : subjectToRow.keySet()) {
            LinkedHashMap<String, String> row = subjectToRow.get(subject);
            List<String> results = new ArrayList<>();
            for (String value : values) {
                results.add(row.getOrDefault(value, ""));
            }
            subjectToResults.put(subject, results);
        }

        return subjectToResults;
    }

}
