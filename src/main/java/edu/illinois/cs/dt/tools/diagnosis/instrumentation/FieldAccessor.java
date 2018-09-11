package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

public interface FieldAccessor {
    void set(final Object o);
    Object get();
}
