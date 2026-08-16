import fs from "node:fs/promises";
import path from "node:path";
import process from "node:process";
import { FileBlob, SpreadsheetFile, Workbook } from "@oai/artifact-tool";

const args = process.argv.slice(2);
const valueAfter = (flag, fallback) => {
  const index = args.indexOf(flag);
  return index >= 0 && index + 1 < args.length ? args[index + 1] : fallback;
};

const repoRoot = process.cwd();
const sourceDir = path.join(repoRoot, "Spreadsheets (PT-BR)");
const outputDir = path.resolve(valueAfter("--output-dir", path.join(repoRoot, "Spreadsheets (EN)")));
const previewDir = path.resolve(valueAfter("--preview-dir", path.join(outputDir, "_previews")));

const fileNames = {
  "df_absenteismo.xlsx": "health_absenteeism.xlsx",
  "df_absenteismo_respiratorio_2013_2019.xlsx": "harmonized_respiratory_absenteeism_2013_2019.xlsx",
  "df_desembolso.xlsx": "asthma_medication_out_of_pocket.xlsx",
  "df_dias_perdidos.xlsx": "lost_days.xlsx",
  "df_dias_perdidos_respiratorio_2013_2019.xlsx": "harmonized_respiratory_lost_days_2013_2019.xlsx",
  "df_medic_asma_2013_2019_wide.xlsx": "asthma_medication_use_2013_2019_wide.xlsx",
  "df_medic_crise_asma_2013_2019_wide.xlsx": "recent_asthma_attack_medication_use_2013_2019_wide.xlsx",
  "df_população_2013_2019_wide.xlsx": "population_2013_2019_wide.xlsx",
  "df_prev_asma_2013_2019_wide.xlsx": "asthma_diagnosis_prevalence_2013_2019_wide.xlsx",
  "df_prev_crise_2013_2019_wide.xlsx": "asthma_attack_prevalence_2013_2019_wide.xlsx",
  "df_renda_2019.xlsx": "income_2019.xlsx",
  "df_taxa_fumantes_2013_2019_wide.xlsx": "active_smoker_rate_2013_2019_wide.xlsx",
  "df_taxa_medic_asma_2013_2019_wide.xlsx": "asthma_medication_population_rate_2013_2019_wide.xlsx",
};

const csvFileNames = {
  "df_medic_asma_2013_2019_long.csv": "asthma_medication_use_2013_2019_long.csv",
  "df_medic_crise_asma_2013_2019_long.csv": "recent_asthma_attack_medication_use_2013_2019_long.csv",
  "df_prev_asma_2013_2019_long.csv": "asthma_diagnosis_prevalence_2013_2019_long.csv",
  "df_prev_crise_2013_2019_long.csv": "asthma_attack_prevalence_2013_2019_long.csv",
  "df_taxa_fumantes_2013_2019_long.csv": "active_smoker_rate_2013_2019_long.csv",
  "df_taxa_medic_asma_2013_2019_long.csv": "asthma_medication_population_rate_2013_2019_long.csv",
};

const exactTranslations = new Map(Object.entries({
  Brasil: "Brazil",
  indisponivel: "unavailable",
  Geral: "Overall",
  "Morador selecionado": "Selected resident",
  "Moradores que deixaram de realizar atividades habituais por motivo de saúde": "Residents who stopped performing usual activities for health reasons",
  "Moradores cujo principal motivo de perda de atividades foi respiratório harmonizado": "Residents whose main reason for activity loss was a harmonized respiratory condition",
  "Respiratório harmonizado": "Harmonized respiratory condition",
  "Medicamento oral": "Oral medication",
  Bombinha: "Inhaler",
  "Dor nos ossos e articulações": "Bone and joint pain",
  Enxaqueca: "Migraine",
  "Gineco-obstétrico": "Gynecologic and obstetric",
  "Dor de dente": "Toothache",
  "Respiratório (asma/bronquite/pneumonia)": "Respiratory (asthma/bronchitis/pneumonia)",
  Gastrointestinal: "Gastrointestinal",
  "Dengue/Chikungunya/Zika/Febre amarela": "Dengue/Chikungunya/Zika/Yellow fever",
  Cardiovascular: "Cardiovascular",
  Diabetes: "Diabetes",
  "Câncer": "Cancer",
  "Neurológico": "Neurological",
  "Saúde mental": "Mental health",
  "Fratura/violência": "Fracture/violence",
  "Outro problema de saúde": "Other health problem",
  "Não declarado": "Not reported",
  "2013: Asma/bronquite/pneumonia + Resfriado/gripe; 2019: Problemas respiratórios, incluindo resfriado/gripe/sinusite/asma/bronquite/pneumonia.":
    "2013: Asthma/bronchitis/pneumonia + Cold/flu; 2019: Respiratory problems, including cold/flu/sinusitis/asthma/bronchitis/pneumonia.",
}));

const headerTranslations = new Map(Object.entries({
  uf: "federative_unit",
  UF: "federative_unit",
  ano: "year",
  valor: "value",
  ci_l: "ci_lower",
  ci_u: "ci_upper",
  li: "lower",
  ls: "upper",
  metodo_ic: "ci_method",
  estimativa_ic: "estimate_ci",
  prevalencia: "prevalence",
  tipo: "category",
  denominador: "denominator",
  harmonizacao: "harmonization",
  pct: "percentage",
  media_dias: "mean_days",
  renda_def: "inflation_adjusted_income",
  tipo_med: "medication_type",
  asma_pg: "asthma_paid_out_of_pocket",
  pct_asma_med_pg: "asthma_medication_out_of_pocket_percentage",
}));

const technicalReplacements = [
  [/estimativa_ic/g, "estimate_ci"], [/metodo_ic/g, "ci_method"],
  [/prevalencia/g, "prevalence"], [/pct_medic_crise_asma/g, "asthma_attack_medication_percentage"],
  [/pct_medic_asma/g, "asthma_medication_percentage"], [/taxa_100k_fumantes/g, "active_smokers_per_100k"],
  [/total_fumantes/g, "total_active_smokers"], [/taxa_100k_medic_asma/g, "asthma_medication_users_per_100k"],
  [/total_medic_asma/g, "total_asthma_medication_users"], [/total_pop/g, "total_population"],
  [/lim_inf/g, "lower_limit"], [/lim_sup/g, "upper_limit"], [/media_dias/g, "mean_days"],
  [/renda_def/g, "inflation_adjusted_income"], [/asma_medic/g, "asthma_medication"],
  [/asma_pg/g, "asthma_paid_out_of_pocket"], [/pct_asma_med_pg/g, "asthma_medication_out_of_pocket_percentage"],
  [/ci_l/g, "ci_lower"], [/ci_u/g, "ci_upper"], [/^li$/, "lower"], [/^ls$/, "upper"],
];

const ufNames = new Set([
  "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal",
  "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul",
  "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí",
  "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia",
  "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins",
]);

function localizeDisplayNumber(value) {
  if (!/^-?[0-9][0-9.,]*%? \[-?[0-9][0-9.,]*%?; -?[0-9][0-9.,]*%?\]$/.test(value)) return value;
  if (value.includes("%") || value.includes(",")) return value.replaceAll(",", ".");
  return value.replaceAll(".", ",");
}

function localizeString(value, rowIndex) {
  if (ufNames.has(value)) return value;
  if (exactTranslations.has(value)) return exactTranslations.get(value);
  if (rowIndex === 0 && headerTranslations.has(value)) return headerTranslations.get(value);
  if (rowIndex === 0) {
    let translated = value;
    for (const [pattern, replacement] of technicalReplacements) translated = translated.replace(pattern, replacement);
    return translated;
  }
  return localizeDisplayNumber(value);
}

const countNumbers = (matrix) => matrix.flat().filter((value) => typeof value === "number").length;
const csvCell = (value) => {
  if (value === null || value === undefined) return "";
  if (typeof value === "number" || typeof value === "boolean") return String(value);
  return `"${String(value).replaceAll('"', '""')}"`;
};
const serializeCsv = (matrix) => `${matrix.map((row) => row.map(csvCell).join(",")).join("\n")}\n`;

await fs.mkdir(outputDir, { recursive: true });
await fs.mkdir(previewDir, { recursive: true });
const results = [];

for (const [sourceName, outputName] of Object.entries(fileNames)) {
  const input = await FileBlob.load(path.join(sourceDir, sourceName));
  const workbook = await SpreadsheetFile.importXlsx(input);
  const sheet = workbook.worksheets.getItemAt(0);
  const used = sheet.getUsedRange(true);
  const sourceValues = used.values;
  const sourceFormulas = used.formulas;
  const translatedValues = sourceValues.map((row, rowIndex) =>
    row.map((value) => typeof value === "string" ? localizeString(value, rowIndex) : value),
  );
  used.values = translatedValues;

  for (let columnIndex = 0; columnIndex < translatedValues[0].length; columnIndex += 1) {
    const columnValues = translatedValues.map((row) => row[columnIndex]);
    const maxLength = Math.max(...columnValues.map((value) => String(value ?? "").length));
    const column = sheet.getRangeByIndexes(0, columnIndex, translatedValues.length, 1);
    column.format.columnWidth = Math.min(48, Math.max(12, maxLength + 2));
    if (maxLength > 48) column.format.wrapText = true;
  }
  used.format.autofitRows();
  sheet.freezePanes.freezeRows(1);

  const outputPath = path.join(outputDir, outputName);
  const output = await SpreadsheetFile.exportXlsx(workbook);
  await output.save(outputPath);

  const preview = await workbook.render({ sheetName: sheet.name, autoCrop: "all", scale: 1, format: "png" });
  await fs.writeFile(path.join(previewDir, `${path.parse(outputName).name}.png`), new Uint8Array(await preview.arrayBuffer()));

  const errorScan = await workbook.inspect({
    kind: "match",
    searchTerm: "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A",
    options: { useRegex: true, maxResults: 50 },
    summary: `Formula error scan for ${outputName}`,
  });
  const sourceBrazil = sourceValues.flat().filter((value) => value === "Brasil").length;
  const outputBrazil = translatedValues.flat().filter((value) => value === "Brazil").length;
  const formulasPreserved = JSON.stringify(sourceFormulas) === JSON.stringify(used.formulas);
  if (countNumbers(sourceValues) !== countNumbers(translatedValues)) throw new Error(`Numeric cell count changed in ${sourceName}`);
  if (sourceBrazil !== outputBrazil) throw new Error(`Brazil row count mismatch in ${sourceName}`);
  if (!formulasPreserved) throw new Error(`Formula mismatch in ${sourceName}`);

  results.push({
    sourceName, outputName, rows: sourceValues.length, columns: sourceValues[0]?.length ?? 0,
    numericCells: countNumbers(sourceValues), ufCellsPreserved: translatedValues.flat().filter((value) => ufNames.has(value)).length,
    brazilCellsTranslated: outputBrazil, formulasPreserved, formulaErrorScan: errorScan.ndjson,
  });
}

for (const [sourceName, outputName] of Object.entries(csvFileNames)) {
  const csvText = await fs.readFile(path.join(sourceDir, sourceName), "utf8");
  const workbook = await Workbook.fromCSV(csvText, { sheetName: "Data" });
  const sheet = workbook.worksheets.getItemAt(0);
  const used = sheet.getUsedRange(true);
  const sourceValues = used.values;
  const translatedValues = sourceValues.map((row, rowIndex) =>
    row.map((value) => typeof value === "string" ? localizeString(value, rowIndex) : value),
  );
  used.values = translatedValues;
  await fs.writeFile(path.join(outputDir, outputName), serializeCsv(translatedValues), "utf8");

  const preview = await workbook.render({ sheetName: sheet.name, autoCrop: "all", scale: 1, format: "png" });
  await fs.writeFile(path.join(previewDir, `${path.parse(outputName).name}.png`), new Uint8Array(await preview.arrayBuffer()));

  const sourceBrazil = sourceValues.flat().filter((value) => value === "Brasil").length;
  const outputBrazil = translatedValues.flat().filter((value) => value === "Brazil").length;
  if (countNumbers(sourceValues) !== countNumbers(translatedValues)) throw new Error(`Numeric cell count changed in ${sourceName}`);
  if (sourceBrazil !== outputBrazil) throw new Error(`Brazil row count mismatch in ${sourceName}`);
  results.push({
    sourceName, outputName, rows: sourceValues.length, columns: sourceValues[0]?.length ?? 0,
    numericCells: countNumbers(sourceValues), ufCellsPreserved: translatedValues.flat().filter((value) => ufNames.has(value)).length,
    brazilCellsTranslated: outputBrazil, formulasPreserved: true, formulaErrorScan: "CSV: not applicable",
  });
}

await fs.writeFile(path.join(outputDir, "workbook-localization-results.json"), `${JSON.stringify(results, null, 2)}\n`, "utf8");
process.stdout.write(`${JSON.stringify({ outputDir, previewDir, files: results.length })}\n`);
