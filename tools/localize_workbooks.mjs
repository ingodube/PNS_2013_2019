// Tables are exported from the canonical R estimates in schema v2.
// Translating old spreadsheets would reintroduce retired indicators.
console.error("Run Rscript tools/run_analysis.R, then tools/promote_analysis.py; workbook text substitution is retired.");
process.exitCode = 1;
