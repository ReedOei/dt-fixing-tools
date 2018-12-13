package edu.illinois.cs.dt.tools.diagnosis.rewrite;

public class FieldDiff {
    private final String fieldName;
    private final String xpath;
    private final String innerFieldName;
    private final String withDepsVal;
    private final String withoutDepsVal;

    public FieldDiff(final String fieldName, final String xpath, final String innerFieldName,
                     final String withDepsVal, final String withoutDepsVal) {
        this.fieldName = fieldName;
        this.xpath = xpath;
        this.innerFieldName = innerFieldName;
        this.withDepsVal = withDepsVal;
        this.withoutDepsVal = withoutDepsVal;
    }

    public String fieldName() {
        return fieldName;
    }

    public String xpath() {
        return xpath;
    }

    public String innerFieldName() {
        return innerFieldName;
    }

    public String withDepsVal() {
        return withDepsVal;
    }

    public String withoutDepsVal() {
        return withoutDepsVal;
    }
}
