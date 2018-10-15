package edu.illinois.cs.dt.tools.analysis;

import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.latex.LatexTable;
import com.reedoei.eunomia.util.StandardMain;

import java.nio.file.Paths;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public class TableGenerator extends StandardMain {
    private final SQLite sqlite;

    private TableGenerator(final String[] args) throws SQLException {
        super(args);

        this.sqlite = new SQLite(Paths.get(getArgRequired("db")));
    }

    public static void main(final String[] args) {
        try {
            new TableGenerator(args).run();
        } catch (Exception e) {
            e.printStackTrace();

            System.exit(1);
        }

        System.exit(0);
    }

    @Override
    protected void run() throws Exception {
        System.out.println(new SQLTable(sqlite.statement(SQLStatements.SUBJECT_INFO_TABLE)) {
            @Override
            public LatexTable formatTable(final List<String> columns, final List<String> rows, final LatexTable table) {
                return table.setRowNames(ListUtil.ensureSize(new ArrayList<>(), rows, ""));
            }
        }.generateTable());
    }
}
