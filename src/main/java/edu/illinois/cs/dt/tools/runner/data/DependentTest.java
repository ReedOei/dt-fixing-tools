package edu.illinois.cs.dt.tools.runner.data;

import com.google.gson.Gson;
import edu.illinois.cs.dt.tools.configuration.Configuration;
import edu.illinois.cs.dt.tools.minimizer.TestMinimizer;
import edu.illinois.cs.dt.tools.minimizer.TestMinimizerBuilder;
import edu.washington.cs.dt.TestExecResultsDelta;

import java.util.function.Supplier;
import java.util.stream.Stream;

public class DependentTest {
    private static final boolean VERIFY_DTS = Configuration.config().getProperty("dt.verify", false);

    public static DependentTest fromDelta(final TestExecResultsDelta delta) {
        return new DependentTest(delta.testName,
                new TestRun(delta.intendedPreTests, delta.intendedResult.result),
                new TestRun(delta.dependentTests, delta.divergentResult.result));
    }

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

    public Stream<TestMinimizer> minimizers(final TestMinimizerBuilder builder) {
        final TestMinimizerBuilder minimizerBuilder = builder.dependentTest(name);

        final Supplier<Stream<TestMinimizer>> intendedMinimizer = () -> minimizer(minimizerBuilder, intended);
        final Supplier<Stream<TestMinimizer>> revealedMinimizer = () -> minimizer(minimizerBuilder, revealed);

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

    private Stream<TestMinimizer> minimizer(final TestMinimizerBuilder builder, final TestRun run) {
        try {
            final TestMinimizer minimizer = builder.build();

            if (VERIFY_DTS) {
                if (!intended.verify(name, builder.classpath()) || !run.result().equals(minimizer.expected())) {
                    return Stream.empty();
                }
            }

            return Stream.of(minimizer);
        } catch (Exception e) {
            return Stream.empty();
        }
    }
}
