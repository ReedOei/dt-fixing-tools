package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import java.util.Optional;

public class SystemPropertyAccessor implements FieldAccessor {
    public static final String SYSTEM_PROPERTY = "System.property.";

    public static Optional<SystemPropertyAccessor> forField(final String fieldName) {
        if (fieldName.startsWith(SYSTEM_PROPERTY)) {
            return Optional.of(new SystemPropertyAccessor(fieldName.substring(SYSTEM_PROPERTY.length())));
        } else {
            return Optional.empty();
        }
    }

    private final String propertyName;

    public SystemPropertyAccessor(final String propertyName) {
        this.propertyName = propertyName;
    }

    @Override
    public void set(final Object o) {
        if (o == null) {
            System.clearProperty(propertyName);
        } else {
            System.setProperty(propertyName, String.valueOf(o));
        }
    }

    @Override
    public Object get() {
        return System.getProperty(propertyName);
    }
}
