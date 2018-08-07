package edu.illinois.cs.dt.tools.configuration;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;

public class Configuration {
    private static final Configuration config = new Configuration();

    public static Configuration config() {
        return config;
    }

    private final Properties properties = new Properties();

    public Configuration() {
    }

    public Configuration loadProperties() throws IOException {
        return loadProperties(Paths.get("dtfixingtools.properties"));
    }

    private Configuration loadProperties(final Path path) throws IOException {
        try (final InputStream fileStream = new FileInputStream(path.toFile())) {
            properties.load(fileStream);
        }

        return this;
    }

    public String getProperty(final String s, final String def) {
        if (properties.getProperty(s) == null) {
            try {
                loadProperties();
            } catch (IOException e) {
                properties.setProperty(s, def);
            }
        }

        return properties.getProperty(s, def);
    }

    public double getDoubleProperty(final String s) {
        return Double.parseDouble(getProperty(s, "0.0"));
    }

    public double getDoubleProperty(final String s, final double def) {
        return getProperty(s, def);
    }

    public double getProperty(final String s, final double def) {
        return Double.parseDouble(getProperty(s, String.valueOf(def)));
    }

    public int getIntProperty(final String s) {
        return Integer.parseInt(getProperty(s, "0"));
    }

    public int getIntProperty(final String s, final int def) {
        return getProperty(s, def);
    }

    public int getProperty(final String s, final int def) {
        return Integer.parseInt(getProperty(s, String.valueOf(def)));
    }

    public int getRounds() {
        return Integer.parseInt(getProperty("dt.randomize.rounds", String.valueOf(10)));
    }

    public boolean getProperty(final String s, final boolean b) {
        return Boolean.parseBoolean(getProperty(s, String.valueOf(b)));
    }
}
