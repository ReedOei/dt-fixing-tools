#!/usr/bin/env bash

# Usage: bash generate-commands.sh DATABASE
database="$1"

echo "==============================================="
echo "TABLE: MODULE COUNTS (SUBJECT INFO)"
echo "==============================================="
cat module-od-counts-table.sql | sqlite3 "$database" | sed -E "s/\|/ \& /g" | sed -E '$s/([A-Za-z0-9.]+)/\\textbf{\1}/g' | sed -e 's/$/ \\\\/'

echo "==============================================="
echo "TABLE: MINIMIZER TIME"
echo "==============================================="
cat timing-table.sql | sqlite3 "$database" | sed -E "s/\|/ \& /g" | sed -E '$s/([A-Za-z0-9.]+)/\\textbf{\1}/g' | sed -e 's/$/ \\\\/'

