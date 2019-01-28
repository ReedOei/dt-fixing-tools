package edu.illinois.cs.dt.tools.analysis;

import java.text.NumberFormat;
import java.util.Map;
import java.util.function.BinaryOperator;

public class CommandCalculator {
    private final Map<String, Double> commandValues;
    private final LatexTools tools;
    private final NumberFormat percentInstance;
    private final NumberFormat ratioInstance;

    public CommandCalculator(final Map<String, Double> commandValues, final LatexTools tools) {
        this.commandValues = commandValues;
        this.tools = tools;

        this.percentInstance = NumberFormat.getPercentInstance();
        percentInstance.setMaximumFractionDigits(1);
        this.ratioInstance = NumberFormat.getNumberInstance();
        ratioInstance.setMaximumFractionDigits(1);
    }

    public double add(final String a, final String b) {
        return compute(a, b, (x, y) -> x + y);
    }

    public double sub(final String a, final String b) {
        return compute(a, b, (x, y) -> x - y);
    }

    public double mul(final String a, final String b) {
        return compute(a, b, (x, y) -> x * y);
    }

    public double div(final String a, final String b) {
        return compute(a, b, (x, y) -> x / y);
    }

    public double compute(final String a, final String b, final BinaryOperator<Double> op) {
        return op.apply(commandValues.get(a), commandValues.get(b));
    }

    public void printRatio(final String name, final String n, final String d) {
        printDerived(name, n, d, ratioInstance);
    }

    public void printPercentage(final String name, final String n, final String d) {
        printDerived(name, n, d, percentInstance);
    }

    public void printDerived(final String name, final String n, final String d, final NumberFormat percentInstance) {
        System.out.println(tools.command(name, percentInstance.format(div(n, d))));
    }
}
