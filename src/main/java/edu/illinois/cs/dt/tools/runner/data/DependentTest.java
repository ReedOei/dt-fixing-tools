package edu.illinois.cs.dt.tools.runner.data;

import com.google.gson.Gson;
import edu.illinois.cs.dt.tools.minimizer.TestMinimizer;
import edu.illinois.cs.dt.tools.minimizer.TestMinimizerBuilder;
import edu.washington.cs.dt.TestExecResultsDelta;

import java.util.function.Supplier;
import java.util.stream.Stream;

public class DependentTest {
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

        final Supplier<Stream<TestMinimizer>> intendedMinimizer = () -> minimizer(minimizerBuilder.testOrder(intended.order()));
        final Supplier<Stream<TestMinimizer>> revealedMinimizer = () -> minimizer(minimizerBuilder.testOrder(revealed.order()));

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

    private Stream<TestMinimizer> minimizer(final TestMinimizerBuilder builder) {
        try {
            return Stream.of(builder.build());
        } catch (Exception e) {
            return Stream.empty();
        }
    }
}
