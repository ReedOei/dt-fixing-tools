package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import java.lang.reflect.Field;
import java.util.Optional;

public class StaticFieldAccessor implements FieldAccessor {
    public static Optional<StaticFieldAccessor> forField(final String fieldName) {
        try {
            final int i = fieldName.lastIndexOf(".");
            final String className = fieldName.substring(0, i);

            final Class<?> fieldClz = Class.forName(className);
            final Field field = fieldClz.getDeclaredField(fieldName.substring(i + 1));

            return Optional.of(new StaticFieldAccessor(field));
        } catch (NoSuchFieldException | ClassNotFoundException e) {
            e.printStackTrace();
        }

        return Optional.empty();
    }

    private final Field field;

    private StaticFieldAccessor(final Field field) {
        this.field = field;
    }

    @Override
    public void set(final Object o) {
        try {
            field.setAccessible(true);
            field.set(null, o);
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
    }

    @Override
    public Object get() {
        return null;
    }
}
