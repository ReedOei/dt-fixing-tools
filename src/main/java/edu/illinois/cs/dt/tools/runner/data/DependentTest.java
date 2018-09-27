package edu.illinois.cs.dt.tools.runner.data;

import com.google.gson.Gson;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.minimizer.TestMinimizer;
import edu.illinois.cs.dt.tools.minimizer.TestMinimizerBuilder;

import java.nio.file.Path;
import java.util.function.Supplier;
import java.util.stream.Stream;

public class DependentTest {
    private static final boolean VERIFY_DTS = Configuration.config().getProperty("dt.verify", true);

    private final String name;

    private final TestRun intended;
    private final TestRun revealed;

    public DependentTest(final String name, final TestRun intended, final TestRun revealed) {
        this.name = name;
        this.intended = intended;
        this.revealed = revealed;
    }

    public String name() {
        return name;
    }

    public TestRun intended() {
        return intended;
    }

    public TestRun revealed() {
        return revealed;
    }

    @Override
    public String toString() {
        return new Gson().toJson(this);
    }

    public Stream<TestMinimizer> minimizers(final TestMinimizerBuilder builder, final Runner runner) {
        final TestMinimizerBuilder minimizerBuilder = builder.dependentTest(name);

        final Supplier<Stream<TestMinimizer>> intendedMinimizer = () -> minimizer(minimizerBuilder, intended, runner);
        final Supplier<Stream<TestMinimizer>> revealedMinimizer = () -> minimizer(minimizerBuilder, revealed, runner);

        if (intended.result().equals(revealed.result())) {
            if (intended.order().size() < revealed.order().size()) {
                return intendedMinimizer.get();
            } else {
                return revealedMinimizer.get();
            }
        } else {
            return Stream.concat(intendedMinimizer.get(), revealedMinimizer.get());
        }
    }

    private Stream<TestMinimizer> minimizer(final TestMinimizerBuilder builder, final TestRun run, final Runner runner) {
        try {
            final TestMinimizer minimizer = builder.testOrder(run.order()).build();

            if (VERIFY_DTS) {
                if (!run.verify(name, runner, null)) {
                    return Stream.empty();
                }
            }

            return Stream.of(minimizer);
        } catch (Exception e) {
            return Stream.empty();
        }
    }

    public boolean verify(final Runner runner, final Path path) {
        return intended.verify(name, runner, path) && revealed.verify(name, runner, path);
    }
}
