package edu.illinois.cs.dt.tools.minimizer;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

public class MinimizeTestsResult {
    @Nonnull
    private final String dependentTest;

    @Nonnull
    private final List<String> deps;

    public MinimizeTestsResult(@Nonnull final String dependentTest,
                               @Nonnull final List<String> deps) {
        this.dependentTest = dependentTest;
        this.deps = deps;
    }

    @Override
    public String toString() {
        return new Gson().toJson(this);
    }

    public void print() {
        print(null);
    }

    public void print(final Path outputPath) {
        if (outputPath != null) {
            try {
                final Path outputFile = outputPath.resolve(dependentTest + "-dependencies.json");
                Files.write(outputFile, toString().getBytes());
                return;
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        System.out.println(toString());
    }

    @Nonnull
    public List<String> getDeps() {
        return deps;
    }
}
