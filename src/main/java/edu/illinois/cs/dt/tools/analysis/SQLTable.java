package edu.illinois.cs.dt.tools.analysis;

import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.eunomia.functional.TriFunction;
import com.reedoei.eunomia.latex.CellType;
import com.reedoei.eunomia.latex.LatexTable;

import java.sql.SQLException;
import java.sql.Types;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class SQLTable {

    public static LatexTable generateAndFormatTable(final TriFunction<List<String>, List<String>, LatexTable, LatexTable> f
            , final Procedure procedure) throws SQLException {
        final QueryResult queryResult = procedure.tableQuery();

        final ListEx<String> columnNames = queryResult.columns().map(QueryColumn::label);
        final List<String> rowNames = queryResult.table().map(l -> l.get(0));

        final LatexTable table = new LatexTable(columnNames, rowNames);

        int rowIndex = 0;
        for (final LinkedHashMap<String, String> rows : queryResult.rows()) {
            final Map<String, Integer> values = new HashMap<>();
            final Map<String, Integer> totals = new HashMap<>();

            // Set up values
            rows.forEach((colLabel, val) -> {
                if (queryResult.column(colLabel).isIntegral()) {
                    values.put(colLabel, Integer.valueOf(val));
                    totals.put(colLabel, Integer.valueOf(val));
                } else if (queryResult.column(colLabel).isDecimal()) {
                    final double v = Double.valueOf(val);
                    // One decimal place of precision
                    values.put(colLabel, (int) (v * 1000.0));
                    totals.put(colLabel, 1000);
                } else {
                    // We'll override this one later
                    values.put(colLabel, 0);
                    totals.put(colLabel, 0);
                }
            });

            table.addRow(values, totals, CellType.DEFAULT);

            // Set up display settings
            for (final String colLabel : rows.keySet()) {
                if (queryResult.column(colLabel).isIntegral()) {
                    table.setupCell(colLabel, rowNames.get(rowIndex), CellType.VALUE_SINGLE_COL);
                } else if (queryResult.column(colLabel).isDecimal()) {
                    table.setupCell(colLabel, rowNames.get(rowIndex), CellType.JUST_PERCENT);
                } else {
                    // Override the default display with the value here
                    table.setupCell(colLabel, rowNames.get(rowIndex), rows.get(colLabel));
                }
            }

            rowIndex++;
        }

        return f.apply(columnNames, rowNames, table);
    }

    public static LatexTable generateTable(final Procedure procedure) throws SQLException {
        return generateAndFormatTable((a, b, table) -> table, procedure);
    }
}
