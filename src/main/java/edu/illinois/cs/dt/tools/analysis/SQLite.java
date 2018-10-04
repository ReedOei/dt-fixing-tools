package edu.illinois.cs.dt.tools.analysis;

import com.reedoei.eunomia.io.files.FileUtil;

import java.io.IOException;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

public class SQLite {
    private final Connection connection;
    private final Map<Path, PreparedStatement> statements = new HashMap<>();

    public SQLite(final Connection connection) {
        this.connection = connection;
    }

    public Procedure statement(final Path path) throws IOException, SQLException {
        final PreparedStatement ps = statements.computeIfAbsent(path, p -> {
            try {
                return connection.prepareStatement(FileUtil.readFile(p));
            } catch (SQLException | IOException e) {
                throw new RuntimeException(e);
            }
        });

        return new Procedure(connection, ps);
    }

    public Stream<Procedure> statements(final Path path) throws IOException {
        // Use a stream so we execute can one at a time (don't create a table before creating tables
        // it depends on via foreign keys)
        return Arrays.stream(FileUtil.readFile(path).split(";")).flatMap(s -> {
            final String trimmed = s.trim();

            if (!trimmed.isEmpty()) {
                try {
                    return Stream.of(new Procedure(connection, connection.prepareStatement(trimmed)));
                } catch (SQLException e) {
                    e.printStackTrace();
                }
            }

            return Stream.empty();
        });
    }
}
