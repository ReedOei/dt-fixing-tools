package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import java.util.Optional;

public class FieldAccessorFactory {
    public static Optional<? extends FieldAccessor> accessorFor(final String fieldName) {
        return accessorFor(fieldName, null);
    }

    public static Optional<? extends FieldAccessor> accessorFor(final String fieldName, final Object o) {
        final Optional<SystemPropertyAccessor> accessor = SystemPropertyAccessor.forField(fieldName);

        if (accessor.isPresent()) {
            return accessor;
        } else {
            return StaticFieldAccessor.forField(fieldName, o);
        }
    }
}
