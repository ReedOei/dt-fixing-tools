package edu.illinois.cs.dt.tools.minimizer;

import com.google.common.base.Preconditions;
import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.util.StandardMain;
import edu.washington.cs.dt.RESULT;
import org.apache.commons.io.FilenameUtils;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;

public class MinimizeTestList extends StandardMain {
    private String classpath;

    public static void main(final String[] args) throws Exception {
        new MinimizeTestList(args).run();
    }

    public MinimizeTestList(final String[] args) {
        super(args);
    }

    private Set<TestMinimizer> fromDtList(final Path path) {
        System.out.println("[INFO] Creating minimizers for file: " + path);

        final Set<TestMinimizer> result = new HashSet<>();

        final List<String> lines;
        try {
            lines = Files.readAllLines(path, Charset.defaultCharset());
        } catch (IOException e) {
            e.printStackTrace();
            return result;
        }

        while (!lines.isEmpty()) {
            if (lines.get(0).isEmpty()) {
                lines.remove(0);
                continue;
            }

            // Verify there are enough lines to contain all the information we need.
            Preconditions.checkArgument(lines.size() >= 5);

            final String testLine = lines.remove(0);
            final String intendedLine = lines.remove(0);
            final String originalOrderLine = lines.remove(0);
            final String revealedLine = lines.remove(0);
            final String modifiedOrderLine = lines.remove(0);

            // Make sure the lines look correct (i.e., start with the right text)
            Preconditions.checkArgument(testLine.startsWith("Test: "));
            Preconditions.checkArgument(intendedLine.startsWith("Intended behavior: "));
            Preconditions.checkArgument(originalOrderLine.startsWith("when executed after: "));
            Preconditions.checkArgument(revealedLine.startsWith("The revealed different behavior: "));
            Preconditions.checkArgument(modifiedOrderLine.startsWith("when executed after: "));

            final String test = testLine.replace("Test: ", "");
            final RESULT intended = RESULT.valueOf(intendedLine.replace("Intended behavior: ", ""));
            final List<String> originalOrder =
                    ListUtil.read(originalOrderLine.replace("when executed after: ", ""));
            final RESULT revealed = RESULT.valueOf(revealedLine.replace("The revealed different behavior: ", ""));
            final List<String> modifiedOrder =
                    ListUtil.read(modifiedOrderLine.replace("when executed after: ", ""));

            try {
                // If they're the same, don't both creating two
                if (intended != revealed) {
                    result.add(new TestMinimizer(originalOrder, classpath, test));
                    result.add(new TestMinimizer(modifiedOrder, classpath, test));
                } else {
                    result.add(new TestMinimizer(modifiedOrder, classpath, test));
                }
            } catch (InterruptedException | TimeoutException | ExecutionException e) {
                e.printStackTrace();
            }
        }

        return result;
    }

    @Override
    public void run() throws Exception {
        this.classpath = getArg("classpath").orElse(System.getProperty("java.class.path"));
        if (getArg("dtFolder").isPresent()) {
            Files.list(Paths.get(getArgRequired("dtFolder"))).forEach(this::runDependentTestFile);
        } else if (getArg("dtFile").isPresent()) {
            runDependentTestFile(Paths.get(getArgRequired("dtFile")));
        } else {
            runDefault();
        }
    }

    private void runDependentTestFile(final Path dtFile) {
        final Optional<Path> outputPath = getArg("outputDir").map(Paths::get);

        final Set<TestMinimizer> minimizers = fromDtList(dtFile);
        System.out.println("[INFO] Starting running " + minimizers.size() + " minimizers.");
        System.out.println();
        for (final TestMinimizer minimizer : minimizers) {
            try {
                final String baseName = FilenameUtils.getBaseName(String.valueOf(dtFile.toAbsolutePath()));
                final Optional<Path> path = outputPath.map(p -> p.resolve(baseName));

                if (path.isPresent()) {
                    try {
                        FileUtil.makeDirectoryDestructive(path.get());
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }

                minimizer.run().print(path.orElse(null));
            } catch (MinimizeTestListException | InterruptedException | TimeoutException | ExecutionException e) {
                e.printStackTrace();
            }
        }
    }

    private void runDefault()
            throws IOException, MinimizeTestListException, InterruptedException, ExecutionException, TimeoutException {
        final Path order = Paths.get(getArgRequired("order"));
        final List<String> testOrder = Files.readAllLines(order, Charset.defaultCharset());
        final String dependentTest = getArg("test").orElse(testOrder.get(testOrder.size() - 1));

        new TestMinimizer(testOrder, classpath, dependentTest).run().print();
    }
}
