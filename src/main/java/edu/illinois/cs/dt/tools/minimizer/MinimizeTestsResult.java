package edu.illinois.cs.dt.tools.minimizer;

import com.google.gson.Gson;
import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.io.IOUtil;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.runner.Runner;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class MinimizeTestsResult {
    public static Path path(final String dependentTest, final Result expected, final Path outputPath) {
        return outputPath.resolve(dependentTest + "-" + expected + "-dependencies.json");
    }

    private static final int VERIFY_REPEAT_COUNT = 1;
    private static final int MAX_SUBSEQUENCES = 1000;

    private final Result expected;
    private final String dependentTest;
    private final List<String> deps;

    public static MinimizeTestsResult fromPath(final Path path) throws IOException {
        return new Gson().fromJson(FileUtil.readFile(path), MinimizeTestsResult.class);
    }

    public static MinimizeTestsResult fromString(final String jsonString) {
        return new Gson().fromJson(jsonString, MinimizeTestsResult.class);
    }

    public MinimizeTestsResult(final Result expected, final String dependentTest, final List<String> deps) {
        this.expected = expected;
        this.dependentTest = dependentTest;
        this.deps = deps;
    }

    private boolean isExpected(final Runner runner, final List<String> deps) {
        final List<String> order = new ArrayList<>(deps);
        order.add(dependentTest());

        return runner
                .runList(order)
                .get()
                .results()
                .get(dependentTest()).result().equals(expected());
    }

    public boolean verify(final Runner runner) throws Exception {
        return verify(runner, VERIFY_REPEAT_COUNT);
    }

    public boolean verify(final Runner runner, final int verifyCount) throws Exception {
        for (int i = 0; i < verifyCount; i++) {
            final List<List<String>> depLists = ListUtil.sample(ListUtil.subsequences(deps), MAX_SUBSEQUENCES);
            int check = 1;
            int totalChecks = 2 + depLists.size() - 1;

            IOUtil.printClearLine(String.format("[INFO] Verifying %d of %d. Running check %d of %d.", i + 1, verifyCount, check++, totalChecks));
            // Check that it's correct with the dependencies
            if (!isExpected(runner, deps)) {
                throw new MinimizeTestListException("Got unexpected result when running with all dependencies!");
            }

            // Only run the first check if there are no dependencies.
            if (deps.isEmpty()) {
                continue;
            }

            verifyDependencies(runner, verifyCount, i, depLists, check, totalChecks);
        }

        System.out.println();

        return true;
    }

    private void verifyDependencies(final Runner runner,
                                    final int verifyCount,
                                    final int i,
                                    final List<List<String>> depLists,
                                    int check,
                                    final int totalChecks) throws Exception {
        IOUtil.printClearLine(String.format("[INFO] Verifying %d of %d. Running check %d of %d.", i + 1, verifyCount, check++, totalChecks));
        // Check that it's wrong without dependencies.
        if (isExpected(runner, new ArrayList<>())) {
            throw new MinimizeTestListException("Got expected result even without any dependencies!");
        }

        // Check that for any subsequence that isn't the whole list, it's wrong.
        for (final List<String> depList : depLists) {
            if (depList.equals(deps)) {
                continue;
            }

            IOUtil.printClearLine(String.format("[INFO] Verifying %d of %d. Running check %d of %d.",  i + 1, verifyCount, check++, totalChecks));
            if (isExpected(runner, depList)) {
                throw new MinimizeTestListException("Got expected result without some dependencies! " + depList);
            }
        }
    }

    @Override
    public String toString() {
        return new Gson().toJson(this);
    }

    public Path path(final Path outputPath) {
        return path(dependentTest, expected, outputPath);
    }

    public void print() {
        print(null);
    }

    public void print(final Path outputPath) {
        if (outputPath != null) {
            try {
                Files.createDirectories(outputPath);
                Files.write(path(outputPath), toString().getBytes());
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public List<String> deps() {
        return deps;
    }

    public String dependentTest() {
        return dependentTest;
    }

    public Result expected() {
        return expected;
    }

    public List<String> withDeps() {
        final List<String> order = new ArrayList<>(deps());
        order.add(dependentTest());
        return order;
    }

    public Path getPath() {
        return getPath(null);
    }

    /**
     * @param modifier A string to add to the end of the path. Can be null or blank to specify there is no modifier.
     * @return A (relative) path to be used whenever storing results relevant to this particular dependent test,
     *         usually inside of some other folder.
     */
    public Path getPath(final String modifier) {
        if (modifier == null || modifier.isEmpty()) {
            return Paths.get(String.format("%s-%s", dependentTest(), expected()));
        } else {

            return Paths.get(String.format("%s-%s-%s", dependentTest(), expected(), modifier));
        }
    }
}
