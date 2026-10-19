# Validation record — methodological correction

Completed locally on 19 October 2026 (America/Bahia). Baseline: `90c90494493a56aae7a8670d8b59b35e191658cd`.

## Scientific revision

The user authorised changes to definitions and estimates. Baseline source hashes,
HTML and the repository snapshot were saved before editing. Visible text, tables,
links, code, formulas, reference entries and figure hashes were extracted from
the immutable baseline. Historical Word, reference and poster files were not edited.

Full official microdata were processed. `data/analysis_manifest.json` records
input hashes, exact versions, calibration checks, IPCA sources and output hashes.
Raw data are cached outside Git; no microdata are committed.

- 2013: 60,202 selected adults; all retained in the age domain.
- 2019: 90,846 selected residents; 88,531 adults; 2,315 aged 15–17 excluded by domain analysis.
- Maximum relative weight discrepancies against V00291: 9.35e-10 (2013) and 5.79e-10 (2019).
- Relative calibration-total errors below 2.21e-16 in both years.
- 588 estimate rows and 84 contrasts, with unique analytical keys.
- Income retains the eight components; nine adults have incomplete totals. Known zero totals remain included. Weighted incomplete frequency is reported separately.
- No standardisation, causal attribution or productivity monetisation was added.

## Tests and publication gates

| Check | Result |
|---|---|
| Synthetic age, routing, missingness, sentinel, income and design cases: 23 assertions | PASS |
| Six national estimates compared with direct final-weight calculations, tolerance 1e-8 | PASS |
| Six standard errors compared with an independent final-weight/post-stratified construction, tolerance 1e-8 | PASS |
| All seven PT-BR and seven EN entry points executed against the final canonical release | PASS |
| All 14 CSV and 14 XLSX files compared by keys, values, denominators, missingness and UF names | PASS |
| Reviewed prose translates deterministically; R chunks are preserved; stale prose is rejected | PASS |
| Both HTML files: 14 tables, 8 figures, 17 references, one MathML display, two authors, zero visible execution blocks | PASS |
| Semantic IDs, numeric table cells, external links, canonical URLs, hreflang and language menu | PASS |
| Responsive validation at 320, 360, 390, 768, 1024 and 1440 px, in both languages | PASS |
| Visual review of English figure/mobile header and Portuguese income/mobile table | PASS |

Five retired figure/table anchors remain as explanatory notes. Validators use
`docs/report-structure.json`, not the previous fixed counts. Wide formulas scroll
inside their container. Canonical/alternate metadata is not embedded as webpage
content. Source line endings and generated CSV/JSON bytes are preserved so hash
checks survive Windows checkouts.

The survey warnings were investigated: they concern strata with one PSU in a
subset. The release records `lonely_strata_n` for every estimate. Unexpected
warnings remain errors. Additional lonely-domain adjustment is not guaranteed
for post-stratified designs, so calculable intervals do not remove this precision
limitation. Boundary proportions retain their points and have no artificial CI.

Rows with at least one lonely stratum: 299. Boundary proportions: 20.

## Reproduction

Use the versions in `data/analysis_manifest.json` and `data/sessionInfo.txt`.
Set `PNS_CACHE_DIR` to the persistent external cache. The recorded staging
folder was its `release` subdirectory.

```text
Rscript tests/test_analysis.R
Rscript tools/run_analysis.R
Rscript tests/validate_real_data.R
Rscript tests/run_entrypoints.R
python tools/promote_analysis.py <cache>/release --check-only
python tools/promote_analysis.py <cache>/release
python tools/compare_revision.py
Rscript .codex/skills/methodology-rmarkdown/scripts/render_bilingual_reports.R .
python tools/validate_publication.py
node .codex/skills/methodology-rmarkdown/scripts/validate_responsive.cjs docs/metodologia.html
node .codex/skills/methodology-rmarkdown/scripts/validate_responsive.cjs docs/methodology-pt-br.html
```

`data/revision_comparison.csv` explains changes from the previous release.
`data/income_previous_for_audit.csv` is a historical comparison, not a current
estimate. Retired spreadsheet names are listed in `data/retired_outputs.json`;
the originals remain in Git history. See `docs/analytical-definitions.md` for
the complete indicator and income-routing rules.

The humanizer skill v3.1.0, commit
`8c307f367c91bd974002f10f2be2965d642c13df`, guided prose editing. Definitions,
numbers, references and legitimate uncertainty were preserved.

## Artifact hashes

| Artifact | SHA-256 |
|---|---|
| `R/pns_core.R` | `48f3b133ad97e8fea8430e475f326cb92754894072072a7807bf403029e52689` |
| `R/report_helpers.R` | `6febfffaffb356f95ad92b2dc43c17addfdcd8c59c4ab57616d7baf102bc8000` |
| `data/estimates.csv` | `91c4ef02378b7b8c18a98ab8ee554f052418c5c1b79bb3c42c059d767e0ec627` |
| `data/contrasts.csv` | `aadfa1240d0ad8f7124be9e3958198e8a4c6d277534c2ba29ade5925563c82da` |
| `data/analysis_manifest.json` | `76c67966eaf29bccabfa8ca9a7708c17ced8a40d48e23ce82d2eb5e9e6ca1d45` |
| `docs/methodology_report_pt_br.Rmd` | `39dbf544ba0e447977532fc2abfcf6afc48a34aec4fd9cb9873e24eb0109cc1b` |
| `docs/methodology_report_en.Rmd` | `94cb0cd37f951cfe8625e730e89fa9bc7abe06b1b83d26874ddc63c5866bf38a` |
| `docs/metodologia.html` | `81e30b907f0b5171d87a03efe8c6a4f6b3b83f98457ef1b74b1794c8dd783055` |
| `docs/methodology-pt-br.html` | `6ce36017ab0fe96c6389d63a4b6633235c8ad88013006b961db116b84857fae9` |

## Publication

Local validation is complete. Public verification will be recorded after the validated commit is pushed.
