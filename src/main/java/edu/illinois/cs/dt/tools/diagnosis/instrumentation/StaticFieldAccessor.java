package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import com.reedoei.testrunner.configuration.Configuration;
import org.apache.commons.lang3.reflect.FieldUtils;

import java.io.File;
import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class StaticFieldAccessor implements FieldAccessor {
    private static URLClassLoader loader;

    public static URLClassLoader loader() {
        if (loader == null) {
            final String str = Configuration.config().getProperty("testrunner.classpath", "");

            final URL[] urls =
                    Arrays.stream(str.split(File.pathSeparator))
                            .flatMap(s -> {
                                try {
                                    return Stream.of(new File(s).toURI().toURL());
                                } catch (MalformedURLException ignored) {}

                                return Stream.empty();
                            })
                            .toArray(URL[]::new);
            loader = new URLClassLoader(urls);
        }

        return loader;
    }

    public static Optional<StaticFieldAccessor> forField(final String fieldName, final Object o) {
        try {
            final int i = fieldName.lastIndexOf(".");
            final String className = fieldName.substring(0, i);

            final Class<?> fieldClz = Class.forName(className);

            // getField can return null if the field does not exist
            return Optional.ofNullable(FieldUtils.getField(fieldClz, fieldName.substring(i + 1), true))
                    .map(f -> new StaticFieldAccessor(f, o));
        } catch (ClassNotFoundException e) {
            System.out.println("Field name: " + fieldName);
            e.printStackTrace();
        }

        return Optional.empty();
    }

    private final Field field;
    private final Object o;

    private StaticFieldAccessor(final Field field, final Object o) {
        this.field = field;
        this.o = o;
    }

    @Override
    public void set(final Object o) {
        try {
            FieldUtils.removeFinalModifier(field, true);
            field.setAccessible(true);
            field.set(this.o, o);
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
    }

    @Override
    public Object get() {
        try {
            field.setAccessible(true);
            return field.get(this.o);
        } catch (IllegalAccessException e) {
            return null;
        }
    }
}
