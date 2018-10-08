package edu.illinois.cs.dt.tools.analysis;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class SQLStatements {
    public static final Path CREATE_TABLES = Paths.get("src/main/sql/create_tables.sql");

    public static final Path INSERT_SUBJECT = Paths.get("src/main/sql/subject_insert.sql");
    public static final Path GET_SUBJECT = Paths.get("src/main/sql/subject_get.sql");

    public static final Path INSERT_TEST_RUN_RESULT = Paths.get("src/main/sql/test_run_result_insert.sql");
    public static final Path UPDATE_TEST_RUN_RESULT_COUNT = Paths.get("src/main/sql/test_run_result_count_update.sql");
    public static final Path INSERT_TEST_RESULT = Paths.get("src/main/sql/test_result_insert.sql");

    public static final Path INSERT_FLAKY_TEST_LIST = Paths.get("src/main/sql/flaky_test_list_insert.sql");
    public static final Path INSERT_FLAKY_TEST = Paths.get("src/main/sql/flaky_test_insert.sql");
    public static final Path INSERT_DETECTION_ROUND = Paths.get("src/main/sql/detection_round_insert.sql");
    public static final Path INSERT_DETECTION_ROUND_TEST_RUN = Paths.get("src/main/sql/detection_round_insert_test_run.sql");

    public static final Path INSERT_VERIFICATION_ROUND = Paths.get("src/main/sql/verify_round_insert.sql");

    static {
        ensureExists(CREATE_TABLES);

        ensureExists(INSERT_SUBJECT);
        ensureExists(GET_SUBJECT);

        ensureExists(INSERT_TEST_RUN_RESULT);
        ensureExists(UPDATE_TEST_RUN_RESULT_COUNT);
        ensureExists(INSERT_TEST_RESULT);

        ensureExists(INSERT_FLAKY_TEST_LIST);
        ensureExists(INSERT_FLAKY_TEST);
        ensureExists(INSERT_DETECTION_ROUND);
        ensureExists(INSERT_DETECTION_ROUND_TEST_RUN);

        ensureExists(INSERT_VERIFICATION_ROUND);
    }

    private static void ensureExists(final Path p) {
        if (!Files.exists(p)) {
            throw new IllegalStateException(p + " does not exist!");
        }
    }
}
