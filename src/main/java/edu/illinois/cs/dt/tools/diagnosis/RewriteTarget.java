package edu.illinois.cs.dt.tools.diagnosis;

import edu.illinois.cs.dt.tools.diagnosis.pollution.PollutedField;

public class RewriteTarget {
    private final String staticFieldName;
    private final String fieldName;
    private final PollutedField field;

    public RewriteTarget(final String staticFieldName, final String fieldName, final PollutedField field) {
        this.staticFieldName = staticFieldName;
        this.fieldName = fieldName;
        this.field = field;
    }

    public String staticFieldName() {
        return staticFieldName;
    }

    public String fieldName() {
        return fieldName;
    }

    public PollutedField field() {
        return field;
    }
}
