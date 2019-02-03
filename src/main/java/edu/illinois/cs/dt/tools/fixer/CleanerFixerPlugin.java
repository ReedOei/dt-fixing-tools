package edu.illinois.cs.dt.tools.fixer;

import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.expr.AnnotationExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.MemberValuePair;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NormalAnnotationExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.SimpleName;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.CatchClause;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.TryStmt;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory;
import edu.illinois.cs.dt.tools.detection.DetectorPathManager;
import edu.illinois.cs.dt.tools.detection.DetectorPlugin;
import edu.illinois.cs.dt.tools.minimizer.FlakyClass;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.minimizer.MinimizerPlugin;
import edu.illinois.cs.dt.tools.minimizer.PolluterData;
import edu.illinois.cs.dt.tools.minimizer.cleaner.CleanerGroup;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.utility.ErrorLogger;
import edu.illinois.cs.dt.tools.utility.OperationTime;
import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.invoker.DefaultInvocationRequest;
import org.apache.maven.shared.invoker.DefaultInvoker;
import org.apache.maven.shared.invoker.InvocationRequest;
import org.apache.maven.shared.invoker.InvocationResult;
import org.apache.maven.shared.invoker.Invoker;
import org.apache.maven.shared.invoker.MavenInvocationException;
import org.apache.maven.shared.invoker.PrintStreamHandler;
import scala.Option;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CleanerFixerPlugin extends TestPlugin {
    public static final String PATCH_LINE_SEP = "==========================";

    private String classpath;
    private MavenProject project;
    private InstrumentingSmartRunner runner;

    private List<Patch> patches;

    private URLClassLoader projectClassLoader;

    // Don't delete. Need a default constructor for TestPlugin
    public CleanerFixerPlugin() {
    }

    private boolean testOrderPasses(final List<String> tests) {
        return new FailingTestDetector(runner).notPassingTests(tests).orElse(new HashSet<>()).isEmpty();
    }

    private String classpath() throws DependencyResolutionRequiredException {
        final List<String> elements = new ArrayList<>(project.getCompileClasspathElements());
        elements.addAll(project.getRuntimeClasspathElements());
        elements.addAll(project.getTestClasspathElements());

        return String.join(File.pathSeparator, elements);
    }

    @Override
    public void execute(final MavenProject project) {
        this.project = project;

        final Option<Runner> runnerOption = RunnerFactory.from(project);
        final ErrorLogger logger = new ErrorLogger(project);

        this.patches = new ArrayList<>();

        System.out.println("DIAGNOSER_MODULE_COORDINATES: " + logger.coordinates());

        logger.runAndLogError(() -> {
            logger.writeSubjectProperties();
            this.classpath = classpath();

            // Get the project classpath, it will be useful for many things
            List<URL> urlList = new ArrayList();
            for (String cp : this.classpath.split(":")) {
                try {
                    urlList.add(new File(cp).toURL());
                } catch (MalformedURLException mue) {
                    TestPluginPlugin.error("Classpath element " + cp + " is malformed!");
                }
            }
            URL[] urls = urlList.toArray(new URL[urlList.size()]);
            this.projectClassLoader = URLClassLoader.newInstance(urls);

            if (runnerOption.isDefined()) {
                this.runner = InstrumentingSmartRunner.fromRunner(runnerOption.get());

                if (!Files.exists(DetectorPathManager.originalOrderPath())) {
                    Files.write(DetectorPathManager.originalOrderPath(), DetectorPlugin.getOriginalOrder(project));
                }

                // First apply the results from passing orders, fix brittles first
                detect()
                        .filter(minimized -> minimized.expected().equals(Result.PASS))
                        .collect(Collectors.toList())
                        .forEach(minimized -> {
                            try {
                                setupAndApplyFix(minimized);
                            } catch (Exception e) {
                                e.printStackTrace();
                                throw new RuntimeException(e);
                            }
                        });
                // After fixing brittles, try the polluters (though possible some get fixed due to fixing brittles)
                detect()
                        .filter(minimized -> !minimized.expected().equals(Result.PASS))
                        .forEach(minimized -> {
                            try {
                                setupAndApplyFix(minimized);
                            } catch (Exception e) {
                                e.printStackTrace();
                                throw new RuntimeException(e);
                            }
                        });
            } else {
                final String errorMsg = "Module is not using a supported test framework (probably not JUnit).";
                TestPluginPlugin.info(errorMsg);
                logger.writeError(errorMsg);
            }

            return null;
        });
    }

    private Stream<MinimizeTestsResult> detect() throws Exception {
        if (!Files.exists(DetectorPathManager.detectionFile())) {
            if (Configuration.config().getProperty("diagnosis.run_detection", true)) {
                new DetectorPlugin(DetectorPathManager.detectionResults(), runner).execute(project);
            } else {
                throw new NoSuchFileException("File " + DetectorPathManager.detectionFile() + " does not exist and diagnosis.run_detection is set to false");
            }
        }

        return new MinimizerPlugin(runner).runDependentTestFile(DetectorPathManager.detectionFile());
    }

    private boolean sameTestClass(String test1, String test2) {
        return test1.substring(0, test1.lastIndexOf('.')).equals(test2.substring(0, test2.lastIndexOf('.')));
    }

    private void setupAndApplyFix(final MinimizeTestsResult minimized) throws Exception {
        // Check that the minimized is not some NOD, in which case we do not proceed
        if (minimized.flakyClass() == FlakyClass.NOD) {
            TestPluginPlugin.info("Will not patch discovered NOD test " + minimized.dependentTest());
            return;
        }

        // Get all test source files
        final List<Path> testFiles = testSources();

        // All minimized orders passed in should have some polluters before (or setters in the case of the order passing)
        if (minimized.polluters().isEmpty()) {
            TestPluginPlugin.error("No polluters for: " + minimized.dependentTest());
            return;
        }

        TestPluginPlugin.info("Beginning to fix dependent test " + minimized.dependentTest());

        // TODO: Handle the case where there are multiple polluting/cleaning groups
        List<PolluterData> polluterDataOrder = new ArrayList<PolluterData>();
        boolean prepend;

        // If in a passing order and there are multiple potential setters, then prioritize the one in the same test class as dependent test
        if (minimized.expected().equals(Result.PASS)) {
            List<PolluterData> pdWithSameTestClass = new ArrayList<>();
            List<PolluterData> pdWithDiffTestClass = new ArrayList<>();
            for (PolluterData pd : minimized.polluters()) {
                // Only care about case of one polluter
                if (pd.deps().size() == 1) {
                    String setter = pd.deps().get(0);
                    // Want the one in the same test class
                    if (sameTestClass(setter, minimized.dependentTest())) {
                        pdWithSameTestClass.add(pd);
                    } else {
                        pdWithDiffTestClass.add(pd);
                    }
                }
            }
            // Add first in same test class ones, then the remaining ones
            polluterDataOrder.addAll(pdWithSameTestClass);
            polluterDataOrder.addAll(pdWithDiffTestClass);
            prepend = true;
        } else {
            // If case of failing order with polluters, best bet is one that has a cleaner, and in same test class as victim
            List<PolluterData> pdNoCleaner = new ArrayList<>();
            List<PolluterData> pdWithCleaner = new ArrayList<>();
            List<PolluterData> pdWithSingleCleaner = new ArrayList<>();
            List<PolluterData> pdWithSingleCleanerSameTestClassVictim = new ArrayList<>();
            List<PolluterData> pdWithSingleCleanerSameTestClassPolluter = new ArrayList<>();
            for (PolluterData pd : minimized.polluters()) {
                // Consider if has a cleaner
                if (!pd.cleanerData().cleaners().isEmpty()) {
                    pdWithCleaner.add(pd);
                    String polluter = pd.deps().get(pd.deps().size() - 1);  // If we're going to modify polluter, do it with the last one
                    // Would be best to have a cleaner group that is only one test
                    for (CleanerGroup cleanerGroup : pd.cleanerData().cleaners()) {
                        if (cleanerGroup.cleanerTests().size() == 1) {
                            pdWithSingleCleaner.add(pd);
                            // Even more ideal, if the cleaner is in the same test class as victim
                            String cleaner = cleanerGroup.cleanerTests().get(0);
                            if (sameTestClass(cleaner, minimized.dependentTest())) {
                                pdWithSingleCleanerSameTestClassVictim.add(pd);
                            }
                            // Also valid is if in the same test class as the polluter
                            if (sameTestClass(cleaner, polluter)) {
                                pdWithSingleCleanerSameTestClassPolluter.add(pd);
                            }
                        }
                    }
                } else {
                    pdNoCleaner.add(pd);
                }
            }
            // Remove from each level duplicates
            pdWithCleaner.removeAll(pdWithSingleCleaner);
            pdWithSingleCleaner.removeAll(pdWithSingleCleanerSameTestClassVictim);
            pdWithSingleCleaner.removeAll(pdWithSingleCleanerSameTestClassPolluter);
            pdWithSingleCleanerSameTestClassVictim.removeAll(pdWithSingleCleanerSameTestClassPolluter);
            // Prioritize based on those levels
            polluterDataOrder.addAll(pdWithSingleCleanerSameTestClassPolluter);
            polluterDataOrder.addAll(pdWithSingleCleanerSameTestClassVictim);
            polluterDataOrder.addAll(pdWithSingleCleaner);
            polluterDataOrder.addAll(pdWithCleaner);
            polluterDataOrder.addAll(pdNoCleaner);

            // If more than one polluter for dependent test, then favor fixing the dependent test
            if (polluterDataOrder.size() > 1) {
                prepend = true;
            } else {
                prepend = false;
            }
        }

        for (PolluterData polluterData : polluterDataOrder) {
            // Apply fix using specific passed in polluter data
            setupAndApplyFix(minimized, polluterData, testFiles, prepend);
        }
    }

    private void setupAndApplyFix(final MinimizeTestsResult minimized,
                                  final PolluterData polluterData,
                                  final List<Path> testFiles,
                                  boolean prepend) throws Exception {
        String polluterTestName;
        Optional<JavaMethod> polluterMethodOpt;
        List<String> failingOrder;
        List<String> fullFailingOrder;

        List<String> cleanerTestNames = new ArrayList<>();  // Can potentially work with many cleaners, try them all

        String victimTestName = minimized.dependentTest();
        Optional<JavaMethod> victimMethodOpt = JavaMethod.find(victimTestName, testFiles, classpath);

        // If dealing with a case of result with failure, then get standard cleaner logic from it
        if (!minimized.expected().equals(Result.PASS)) {
            // Failing order has both the dependent test and the dependencies
            failingOrder = polluterData.withDeps(minimized.dependentTest());

            polluterTestName = polluterData.deps().get(polluterData.deps().size() - 1); // If more than one polluter, want to potentially modify last one
            polluterMethodOpt = JavaMethod.find(polluterTestName, testFiles, classpath);

            if (polluterData.cleanerData().cleaners().isEmpty()) {
                TestPluginPlugin.info("Found polluters for " + victimTestName + " but no cleaners.");
                TestPluginPlugin.info("Trying prior patches to see if now is fixed.");
                if (applyPatchesAndRun(failingOrder, victimMethodOpt.get(), polluterMethodOpt.get())) {
                    TestPluginPlugin.info("Dependent test " + victimTestName + " can pass with patches from before.");
                } else {
                    TestPluginPlugin.info("Prior patches do not allow " + victimTestName + " to pass.");
                    writePatch(victimMethodOpt.get(), 0, null, 0, null, null, polluterMethodOpt.orElse(null), 0, "NO CLEANERS");
                }
                return;
            }

            for (CleanerGroup cleanerGroup : polluterData.cleanerData().cleaners()) {
                // Only handle cleaner groups that have one test each
                if (cleanerGroup.cleanerTests().size() == 1) {
                    cleanerTestNames.add(cleanerGroup.cleanerTests().get(0));   // TODO: Handle cleaner group with more than one test
                }
            }
            fullFailingOrder = minimized.expectedRun().testOrder(); // Also grab the full failing order from the expected run's test order


        } else {
            // "Cleaner" when result is passing is the "polluting" test(s)
            // TODO: Handler group of setters with more than one test
            if (polluterData.deps().size() > 1) {
                TestPluginPlugin.error("There is more than one setter test (currently unsupported)");
                return;
            }
            polluterTestName = null;    // No polluter if minimized order is passing
            polluterMethodOpt = Optional.ofNullable(null);

            cleanerTestNames.add(polluterData.deps().get(0));   // Assume only one, get first...

            // Failing order should be just the dependent test by itself (as is the full failing order (for now))
            failingOrder = Collections.singletonList(minimized.dependentTest());
            fullFailingOrder = failingOrder;
        }

        if (polluterTestName != null && !polluterMethodOpt.isPresent()) {
            TestPluginPlugin.error("Could not find polluter method " + polluterTestName);
            TestPluginPlugin.error("Tried looking in: " + testFiles);
            return;
        }

        if (!victimMethodOpt.isPresent()) {
            TestPluginPlugin.error("Could not find victim method " + victimTestName);
            TestPluginPlugin.error("Tried looking in: " + testFiles);
            return;
        }

        // Give up if cannot find valid cleaner (single test that makes the order pass)
        if (cleanerTestNames.isEmpty()) {
            TestPluginPlugin.error("Could not get a valid cleaner for " + victimTestName);
            return;
        }

        // Check if we pass in isolation before fix
        TestPluginPlugin.info("Running victim test with polluter before adding code from cleaner.");
        final List<Double> elapsedTime = new ArrayList<>();
        boolean failingOrderPasses = OperationTime.runOperation(() -> {
            return testOrderPasses(failingOrder);
        }, (passes, time) -> {
            elapsedTime.add(time.elapsedSeconds());
            return passes;
        });
        if (failingOrderPasses) {
            TestPluginPlugin.error("Failing order doesn't fail.");
            writePatch(victimMethodOpt.get(), 0, null, 0, null, null, polluterMethodOpt.orElse(null), elapsedTime.get(0), "NOT FAILING ORDER");
            return;
        }
        elapsedTime.clear();

        // Try to apply fix with all cleaners, but if one of them works, then we are good
        for (String cleanerTestName : cleanerTestNames) {
            // Reload methods
            victimMethodOpt = JavaMethod.find(victimTestName, testFiles, classpath);
            if (polluterMethodOpt.isPresent()) {
                polluterMethodOpt = JavaMethod.find(polluterTestName, testFiles, classpath);
            }
            Optional<JavaMethod> cleanerMethodOpt = JavaMethod.find(cleanerTestName, testFiles, classpath);
            if (!cleanerMethodOpt.isPresent()) {
                TestPluginPlugin.error("Could not find cleaner method " + cleanerTestName);
                TestPluginPlugin.error("Tried looking in: " + testFiles);
                continue;
            }
            // TODO: applyFix should take in a location for where to output the Java file that contains the
            //       "fixed" code or an option to directly replace the existing test source file.
            TestPluginPlugin.info("Applying code from " + cleanerMethodOpt.get().methodName() + " to make " + victimMethodOpt.get().methodName() + " pass.");
            boolean fixSuccess = applyFix(failingOrder, fullFailingOrder, polluterMethodOpt.orElse(null), cleanerMethodOpt.get(), victimMethodOpt.get(), prepend);
            // A successful patch means we do not need to try all the remaining cleaners for this ordering
            if (fixSuccess) {
                return;
            } else {
                // Otherwise, report that this cleaner somehow did not work with the test
                writePatch(victimMethodOpt.get(), 0, null, 0, null, cleanerMethodOpt.get(), polluterMethodOpt.orElse(null), 0, "CLEANER DOES NOT WORK");
            }
        }
        // If reached here, then no cleaner helped fix this dependent test, so report as such
        TestPluginPlugin.info("No cleaner could help make " + victimMethodOpt.get().methodName() + " pass!");
    }

    private List<Path> testSources() throws IOException {
        final List<Path> testFiles = new ArrayList<>();
        try (final Stream<Path> paths = Files.walk(Paths.get(project.getBuild().getTestSourceDirectory()))) {
            paths.filter(Files::isRegularFile)
                    .forEach(testFiles::add);
        }
        return testFiles;
    }

    private void backup(final JavaFile javaFile) throws IOException {
        final Path path = CleanerPathManager.backupPath(javaFile.path());
        Files.copy(javaFile.path(), path, StandardCopyOption.REPLACE_EXISTING);
    }

    private void restore(final JavaFile javaFile) throws IOException {
        final Path path = CleanerPathManager.backupPath(javaFile.path());
        Files.copy(path, javaFile.path(), StandardCopyOption.REPLACE_EXISTING);
    }

    // Try applying cleaner statements and see if test passes when run after polluter
    // Returns true if passes, returns false if not
    private boolean checkCleanerStmts(final List<String> failingOrder,
                                      final JavaMethod methodToModify,
                                      final NodeList<Statement> cleanerStmts,
                                      boolean prepend, boolean suppressError) throws Exception {
        // If want to prepend set to true, then prepend to victim
        if (prepend) {
            methodToModify.prepend(cleanerStmts);
        } else {
            methodToModify.append(cleanerStmts);
        }
        methodToModify.javaFile().writeAndReloadCompilationUnit();

        // Rebuild and see if tests run properly
        try {
            runMvnInstall(suppressError);
        } catch (Exception ex) {
            TestPluginPlugin.debug("Error building the code, passed in cleaner code does not compile");
            // Reset the change
            if (prepend) {
                methodToModify.removeFirstBlock();
            } else {
                methodToModify.removeLastBlock();
            }
            return false;
        }
        // TODO: Output to result files rather than stdout
        boolean passInFailingOrder = testOrderPasses(failingOrder);

        // Reset the change
        if (prepend) {
            methodToModify.removeFirstBlock();
        } else {
            methodToModify.removeLastBlock();
        }

        return passInFailingOrder;

        // TODO: Make sure our fix doesn't break any other tests
        //  This block of code could be useful when dealing with a case where we add the necessary
        //  setup to a victim test but our "fix" would actually now cause another test to fail.
        //  We will need some additional logic to deal with this case (i.e., a fix reveals another
        //  dependency) than just this block of code, but it may have its uses later.
        // Check if we pass in the whole test class
        // Should check before fix if class is passing
        //        boolean didClassPass = didTestsPass(victimJavaFile.getTestListAsString());

        //        if (didClassPass) {
        //            boolean didClassPassAfterFix = didTestsPass(victimJavaFile.getTestListAsString());
        //            if (!didClassPassAfterFix) {
        //                System.out.println("Fix was unsuccessful. Fix causes some other test in the class to fail.");
        //                return;
        //            }
        //        }
    }

    // The delta debugging logic to return minimal list of statements to add in to make run pass
    // In addition to failing order to run, need also list of statements; assumed statements already make run pass
    // The value n is needed to represent granularity of partitioning the statements to get minimal list
    // The prepend boolean is to decide if to prepend to passed in method or to append
    private NodeList<Statement> deltaDebug(final List<String> failingOrder,
                                           final JavaMethod methodToModify,
                                           final NodeList<Statement> cleanerStmts,
                                           int n, boolean prepend) throws Exception {
        // If n granularity is greater than number of cleaner statements, then finished, simply return cleaner statements
        if (cleanerStmts.size() < n) {
            return cleanerStmts;
        }

        // Cut the statements into n equal chunks and try each chunk
        int chunkSize = (int)Math.round((double)(cleanerStmts.size()) / n);
        List<NodeList<Statement>> chunks = new ArrayList<>();
        for (int i = 0; i < cleanerStmts.size(); i += chunkSize) {
            NodeList<Statement> chunk = NodeList.nodeList();
            NodeList<Statement> otherChunk = NodeList.nodeList();
            // Create chunk starting at this iteration
            int endpoint = Math.min(cleanerStmts.size(), i + chunkSize);
            chunk.addAll(cleanerStmts.subList(i, endpoint));

            // Complement chunk are elements before and after this current chunk
            otherChunk.addAll(cleanerStmts.subList(0, i));
            otherChunk.addAll(cleanerStmts.subList(endpoint, cleanerStmts.size()));

            // Check if applying chunk works
            if (checkCleanerStmts(failingOrder, methodToModify, chunk, prepend, true)) {
                return deltaDebug(failingOrder, methodToModify, chunk, 2, prepend);         // If works, then delta debug some more this chunk
            }
            // Otherwise, check if applying complement chunk works
            if (checkCleanerStmts(failingOrder, methodToModify, otherChunk, prepend, true)) {
                return deltaDebug(failingOrder, methodToModify, otherChunk, 2, prepend);    // If works, then delta debug some more the complement chunk
            }
        }
        // If not chunk/complement work, increase granularity and try again
        return deltaDebug(failingOrder, methodToModify, cleanerStmts, n * 2, prepend);
    }

    private NodeList<Statement> getCodeFromAnnotatedMethod(final String testClassName, final JavaFile javaFile, final String annotation) throws Exception {
        NodeList<Statement> stmts = NodeList.nodeList();

        // Determine super classes, to be used for later looking up helper methods
        Class testClass = this.projectClassLoader.loadClass(testClassName);
        List<Class> superClasses = new ArrayList<>();
        Class currClass = testClass;
        while (currClass != null) {
            superClasses.add(currClass);
            currClass = currClass.getSuperclass();
        }

        // If the test class is a subclass of JUnit 3's TestCase, then there is no annotation, just handle setUp and tearDown
        for (Class clz : superClasses) {
            if (clz.toString().equals("class junit.framework.TestCase")) {
                if (annotation.equals("@org.junit.Before")) {
                    stmts.add(new ExpressionStmt(new MethodCallExpr(null, "setUp")));
                } else if (annotation.equals("@org.junit.After")) {
                    stmts.add(new ExpressionStmt(new MethodCallExpr(null, "tearDown")));
                }
                return stmts;
            }
        }

        // Iterate through super classes going "upwards", starting with this test class, to get annotated methods
        // If already seen a method of the same name, then it is overriden, so do not include
        List<String> annotatedMethods = new ArrayList<>();
        List<String> annotatedMethodsLocal = new ArrayList<>();
        for (Class clazz : superClasses) {
            for (Method meth : clazz.getDeclaredMethods()) {
                for (Annotation anno : meth.getDeclaredAnnotations()) {
                    if (anno.toString().equals(annotation + "()")) {
                        if (!annotatedMethods.contains(meth.getName())) {
                            annotatedMethods.add(meth.getName());
                        }
                        if (clazz.equals(testClass)) {
                            annotatedMethodsLocal.add(meth.getName());
                        }
                    }
                }
            }
        }
        annotatedMethods.removeAll(annotatedMethodsLocal);

        // For Before, go last super class first, then inline the statements in test class
        if (annotation.equals("@org.junit.Before")) {
            for (int i = annotatedMethods.size() - 1; i >= 0; i--) {
                stmts.add(new ExpressionStmt(new MethodCallExpr(null, annotatedMethods.get(i))));
            }
            for (String methName : annotatedMethodsLocal) {
                MethodDeclaration method = javaFile.findMethodDeclaration(testClassName + "." + methName);
                Optional<BlockStmt> body = method.getBody();
                if (body.isPresent()) {
                    stmts.addAll(body.get().getStatements());
                }
            }
        } else {
            // For After, inline the statements in test class, then go first super class first
            for (String methName : annotatedMethodsLocal) {
                MethodDeclaration method = javaFile.findMethodDeclaration(testClassName + "." + methName);
                Optional<BlockStmt> body = method.getBody();
                if (body.isPresent()) {
                    stmts.addAll(body.get().getStatements());
                }
            }
            for (int i = 0; i < annotatedMethods.size() ; i++) {
                stmts.add(new ExpressionStmt(new MethodCallExpr(null, annotatedMethods.get(i))));
            }
        }

        return stmts;
    }

    private boolean applyPatchesAndRun(final List<String> failingOrder,
                                       final JavaMethod victimMethod,
                                       final JavaMethod polluterMethod) throws Exception {
        TestPluginPlugin.info("Applying patches from before to see if order still fails.");
        for (Patch patch : patches) {
            TestPluginPlugin.info("Apply patch for " + patch.methodToPatch().methodName());
            patch.applyPatch();

            // Try the patch out
            runMvnInstall(false);
            boolean passWithPatches = testOrderPasses(failingOrder);
            patch.restore();        // Regardless, restore patch file(s)
            runMvnInstall(false);   // Rebuild again, in preparation for next run
            if (passWithPatches) {
                TestPluginPlugin.info("Failing order no longer fails after patches.");
                // If this is a new dependent test and the patches fix it, then save a file for it
                // just to help indicate that the test has been fixed
                writePatch(victimMethod, 0, null, 0, null, null, polluterMethod, 0, "PRIOR PATCH FIXED (DEPENDENT=" + patch.victimMethod().methodName() + ",CLEANER=" + patch.cleanerMethod().methodName() + ", MODIFIED=" + patch.methodToPatch().methodName() + ")");
                return true;
            }
        }
        return false;
    }

    private ExpressionStmt getHelperCallStmt(JavaMethod cleanerMethod, boolean isSameTestClass) {
        Expression objectCreation = null;
        if (!isSameTestClass) { // If in same test class, then no need to create a new object of that instance
            objectCreation = new ObjectCreationExpr(null, new ClassOrInterfaceType(null, cleanerMethod.getClassName()), NodeList.nodeList());
        }
        Expression helperCall = new MethodCallExpr(objectCreation, "cleanerHelper");
        ExpressionStmt helperCallStmt = new ExpressionStmt(helperCall);
        return helperCallStmt;
    }

    private JavaMethod addHelperMethod(JavaMethod cleanerMethod, JavaMethod methodToModify, boolean isSameTestClass, boolean prepend) throws Exception {
        // The modification is to modify the cleaner class to add a helper, then have the other method call the helper
        ExpressionStmt helperCallStmt = getHelperCallStmt(cleanerMethod, isSameTestClass);
        if (prepend) {
            methodToModify.prepend(NodeList.nodeList(helperCallStmt));
        } else {
            methodToModify.append(NodeList.nodeList(helperCallStmt));
        }
        methodToModify.javaFile().writeAndReloadCompilationUnit();

        String helperName = cleanerMethod.getClassName() + ".cleanerHelper";
        cleanerMethod = JavaMethod.find(cleanerMethod.methodName(), testSources(), classpath).get();    // Reload, just in case
        cleanerMethod.javaFile().addMethod(helperName);
        cleanerMethod.javaFile().writeAndReloadCompilationUnit();
        JavaMethod helperMethod = JavaMethod.find(helperName, testSources(), classpath).get();
        helperMethod.javaFile().writeAndReloadCompilationUnit();

        methodToModify = JavaMethod.find(methodToModify.methodName(), testSources(), classpath).get();   // Reload, just in case

        return helperMethod;
    }

    // Returns if applying the fix was successful or not
    private boolean applyFix(final List<String> failingOrder,
                          final List<String> fullFailingOrder,
                          final JavaMethod polluterMethod,
                          final JavaMethod cleanerMethod,
                          final JavaMethod victimMethod,
                          boolean prepend) throws Exception {
        // If failing order still failing, apply all the patches from before first to see if already fixed
        if (!patches.isEmpty()) {
            boolean passWithPatch = applyPatchesAndRun(failingOrder, victimMethod, polluterMethod);
            if (passWithPatch) {
                return true;
            }
        }

        // If polluter/victim case, check if cleaner is same test class as polluter, so append to polluter
        // Method to modify is based on this decision
        /*boolean prepend = true;
        JavaMethod methodToModify = victimMethod;
        if (polluterMethod != null) {
            if (sameTestClass(cleanerMethod.methodName(), polluterMethod.methodName())) {
                prepend = false;
                methodToModify = polluterMethod;
            }
        }*/
        JavaMethod methodToModify = victimMethod;
        if (!prepend && polluterMethod != null) {
            methodToModify = polluterMethod;
        }

        // Back up the files we are going to modify
        backup(methodToModify.javaFile());
        backup(cleanerMethod.javaFile());

        boolean isSameTestClass = cleanerMethod.getClassName().equals(methodToModify.getClassName());

        // If the cleaner method is annotated such that it is expected to fail, then wrap in try catch
        boolean expected = false;
        for (AnnotationExpr annotExpr : cleanerMethod.method().getAnnotations()) {
            if (annotExpr instanceof NormalAnnotationExpr) {
                NormalAnnotationExpr normalAnnotExpr = (NormalAnnotationExpr) annotExpr;
                for (MemberValuePair memberValuePair : normalAnnotExpr.getPairs()) {
                    if (memberValuePair.getName().equals("expected")) {
                        expected = true;
                        break;
                    }
                }
            }
        }

        // Do our fix using all cleaner code, which includes setup and teardown
        TestPluginPlugin.info("Applying code from cleaner and recompiling.");
        final NodeList<Statement> cleanerStmts = NodeList.nodeList();
        // Note: consider both standard imported version (e.g., @Before) and weird non-imported version (e.g., @org.junit.Before)
        // Only include BeforeClass and Before if in separate classes (for both victim and polluter(s))
        if (!isSameTestClass) {
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.getClassName(), cleanerMethod.javaFile(), "@org.junit.BeforeClass"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.getClassName(), cleanerMethod.javaFile(), "@org.junit.Before"));
        }
        if (expected) {
            cleanerStmts.addAll(cleanerMethod.body().getStatements());
        } else {
            // Wrap the body inside a big try statement to suppress any exceptions
            ClassOrInterfaceType exceptionType = new ClassOrInterfaceType().setName(new SimpleName("Exception"));
            CatchClause catchClause = new CatchClause(new Parameter(exceptionType, "ex"), new BlockStmt());
            cleanerStmts.add(new TryStmt(new BlockStmt(cleanerMethod.body().getStatements()), NodeList.nodeList(catchClause), new BlockStmt()));
        }
        // Only include AfterClass and After if in separate classes (for both victim and polluter(s))
        if (!isSameTestClass) {
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.getClassName(), cleanerMethod.javaFile(), "@org.junit.After"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.getClassName(), cleanerMethod.javaFile(), "@org.junit.AfterClass"));
        }

        // Get the helper method reference
        JavaMethod helperMethod = addHelperMethod(cleanerMethod, methodToModify, isSameTestClass, prepend);

        TestPluginPlugin.info("Trying to modify " + methodToModify.methodName() + " to make failing order pass.");

        // Check if applying these cleaners on the method suffices
        if (!checkCleanerStmts(failingOrder, helperMethod, cleanerStmts, prepend, false)) {
            TestPluginPlugin.error("Applying all of cleaner " + cleanerMethod.methodName() + " to " + methodToModify.methodName() + " does not fix!");
            restore(methodToModify.javaFile());
            restore(helperMethod.javaFile());

            // If does not suffice (and in polluter case), do not abandon hope, try the opposite way as well
            if (polluterMethod != null) {
                if (prepend) {
                    methodToModify = polluterMethod;
                } else {
                    methodToModify = victimMethod;
                }
                backup(methodToModify.javaFile());
                backup(helperMethod.javaFile());
                prepend = !prepend;
                helperMethod = addHelperMethod(cleanerMethod, methodToModify, sameTestClass(cleanerMethod.methodName(), methodToModify.methodName()), prepend);
                if (!checkCleanerStmts(failingOrder, helperMethod, cleanerStmts, prepend, false)) {
                    TestPluginPlugin.error("Applying all of cleaner " + cleanerMethod.methodName() + " to " + methodToModify.methodName() + " does not fix!");
                    writePatch(victimMethod, 0, null, 0, null, cleanerMethod, polluterMethod, 0, "CLEANER DOES NOT FIX");
                    restore(methodToModify.javaFile());
                    restore(helperMethod.javaFile());
                    return false;
                }
            } else {
                return false;
            }
        }

        final JavaMethod finalHelperMethod = helperMethod;
        final boolean finalPrepend = prepend;

        final List<Double> elapsedTime = new ArrayList<>();
        final NodeList<Statement> minimalCleanerStmts = OperationTime.runOperation(() -> {
            // Cleaner is good, so now we can start delta debugging
            return deltaDebug(failingOrder, finalHelperMethod, cleanerStmts, 2, finalPrepend);
        }, (finalCleanerStmts, time) -> {
            elapsedTime.add(time.elapsedSeconds());
            return finalCleanerStmts;
        });

        BlockStmt patchedBlock = new BlockStmt(minimalCleanerStmts);

        // Check that the results are valid
        if (!checkCleanerStmts(failingOrder, finalHelperMethod, minimalCleanerStmts, prepend, false)) {
            TestPluginPlugin.info("Final minimal is not actually working!");
            restore(methodToModify.javaFile());
            restore(finalHelperMethod.javaFile());
            runMvnInstall(false);
            writePatch(victimMethod, 0, patchedBlock, cleanerStmts.size(), methodToModify, cleanerMethod, polluterMethod, elapsedTime.get(0), "BROKEN MINIMAL");
            return false;
        }
        // Also check against the full failing order
        if (!checkCleanerStmts(fullFailingOrder, finalHelperMethod, minimalCleanerStmts, prepend, false)) {
            TestPluginPlugin.info("Final minimal is not actually working for the full failing order!");
            restore(methodToModify.javaFile());
            restore(finalHelperMethod.javaFile());
            runMvnInstall(false);
            writePatch(victimMethod, 0, patchedBlock, cleanerStmts.size(), methodToModify, cleanerMethod, polluterMethod, elapsedTime.get(0), "BROKEN MINIMAL FOR FULL");
            return false;
        }

        // Try to inline these statements into the method
        restore(methodToModify.javaFile());
        methodToModify = JavaMethod.find(methodToModify.methodName(), testSources(), classpath).get();   // Reload, just in case
        boolean inlineSuccessful = checkCleanerStmts(failingOrder, methodToModify, minimalCleanerStmts, prepend, false);
        if (!inlineSuccessful) {
            TestPluginPlugin.info("Inlining patch into " + methodToModify.methodName() + " still not good enough to run.");
        }

        // Write out the changes in the form of a patch
        int startingLine;
        if (prepend) {
            startingLine = methodToModify.beginLine() + 1;  // Shift one, do not include declaration line
        } else {
            startingLine = methodToModify.endLine() - 1;    // Shift one, patch starts before end of method
        }
        String status = inlineSuccessful ? "INLINE SUCCESSFUL" : "INLINE FAIL";
        Path patchFile = writePatch(victimMethod, startingLine, patchedBlock, cleanerStmts.size(), methodToModify, cleanerMethod, polluterMethod, elapsedTime.get(0), status);

        patches.add(new Patch(methodToModify, patchedBlock, prepend, cleanerMethod, victimMethod, testSources(), classpath, inlineSuccessful));

        // Report successful patching, report where the patch is
        TestPluginPlugin.info("Patching successful, patch file for " + victimMethod.methodName() + " found at: " + patchFile);

        // Restore the original file
        restore(methodToModify.javaFile());
        restore(finalHelperMethod.javaFile());
        // Final compile to get state to right place
        runMvnInstall(false);

        return true;
    }

    // Helper method to create a patch file adding in the passed in block
    // Includes a bunch of extra information that may be useful
    private Path writePatch(JavaMethod victimMethod, int begin, BlockStmt blockStmt, int originalsize,
                            JavaMethod modifiedMethod, JavaMethod cleanerMethod,
                            JavaMethod polluterMethod,
                            double elapsedTime, String status) throws IOException {
        List<String> patchLines = new ArrayList<>();
        patchLines.add("STATUS: " + status);
        patchLines.add("MODIFIED: " + (modifiedMethod == null ? "N/A" : modifiedMethod.methodName()));
        patchLines.add("CLEANER: " + (cleanerMethod == null ? "N/A" : cleanerMethod.methodName()));
        patchLines.add("POLLUTER: " + (polluterMethod == null ? "N/A" : polluterMethod.methodName()));
        patchLines.add("ORIGINAL CLEANER SIZE: " + (originalsize == 0 ? "N/A" : String.valueOf(originalsize)));
        patchLines.add("NEW CLEANER SIZE: " + (blockStmt != null ? String.valueOf(blockStmt.getStatements().size()) : "N/A"));
        patchLines.add("ELAPSED TIME: " + elapsedTime);

        // If there is a block to add (where it might not be if in error state and need to just output empty)
        if (blockStmt != null) {
            patchLines.add(PATCH_LINE_SEP);
            String[] lines = blockStmt.toString().split("\n");
            patchLines.add("@@ -" + begin +",0 +" + begin + "," + lines.length + " @@");
            for (String line : lines) {
                patchLines.add("+ " + line);
            }
        }
        Path patchFile = CleanerPathManager.fixer().resolve(victimMethod.methodName() + ".patch");  // The patch file is based on the dependent test
        Files.createDirectories(patchFile.getParent());

        // If the file exists, then need to give it a new name
        if (Files.exists(patchFile)) {
            // Keep adding to a counter to make unique name
            Path newPatchFile;
            int counter = 1;
            while (true) {
                newPatchFile = Paths.get(patchFile + "." + counter);
                if (!Files.exists(newPatchFile)) {  // Found a valid file to write to
                    patchFile = newPatchFile;
                    break;
                }
                counter++;
            }
        }
        Files.write(patchFile, patchLines);
        return patchFile;
    }

    private boolean runMvnInstall(boolean suppressOutput) throws MavenInvocationException {
        // TODO: Maybe support custom command lines/options?
        final InvocationRequest request = new DefaultInvocationRequest();
        request.setGoals(Arrays.asList("install"));
        request.setPomFile(project.getFile());
        request.setProperties(new Properties());
        request.getProperties().setProperty("skipTests", "true");
        request.getProperties().setProperty("rat.skip", "true");
        request.getProperties().setProperty("dependency-check.skip", "true");
        request.getProperties().setProperty("enforcer.skip", "true");

        // TODO: Log the output from the maven process somewhere
        ByteArrayOutputStream baosOutput = new ByteArrayOutputStream();
        PrintStream outputStream = new PrintStream(baosOutput);
        request.setOutputHandler(new PrintStreamHandler(outputStream, true));
        ByteArrayOutputStream baosError = new ByteArrayOutputStream();
        PrintStream errorStream = new PrintStream(baosError);
        request.setErrorHandler(new PrintStreamHandler(errorStream, true));

        final Invoker invoker = new DefaultInvoker();
        final InvocationResult result = invoker.execute(request);

        if (result.getExitCode() != 0) {
            // Print out the contents of the output/error streamed out during evocation, if not suppressed
            if (!suppressOutput) {
                TestPluginPlugin.error(baosOutput.toString());
                TestPluginPlugin.error(baosError.toString());
            }

            if (result.getExecutionException() == null) {
                throw new RuntimeException("Compilation failed with exit code " + result.getExitCode() + " for an unknown reason");
            } else {
                throw new RuntimeException(result.getExecutionException());
            }
        }

        return true;
    }
}
