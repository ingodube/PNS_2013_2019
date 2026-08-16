# Bilingual methodological report validation

Local validation date: August 16, 2026 (`America/Bahia`)

Local result: **PASS**

## Publication routes

- Default English report: `docs/metodologia.html`.
- PT-BR report: `docs/methodology-pt-br.html`.
- Root redirect: `docs/index.html` → `metodologia.html`.
- Stable public URL: `https://ingodube.github.io/PNS_2013_2019/metodologia.html`.
- The native `Language` dropdown links the two self-contained documents.
- Every output includes matching `lang`, canonical, and `hreflang` metadata.

## Rendering and integrity

- R 4.6.1 and Pandoc 3.8.3.
- Direct self-contained HTML with MathML, `theme: null`, the repo-local Pandoc template, and the repo-local CSS.
- Two consecutive bilingual builds produced identical hashes: PASS.
- PT-BR output compared with the previously published HTML: table cells, figure bytes, scientific code blocks, links, and document order preserved — PASS.
- EN/PT-BR semantic IDs, table numeric values, scientific links, and CSV numeric values agree — PASS.
- The names of all 27 federative units remain unchanged; only `Brasil` is localized as `Brazil` in English — PASS.

| Artifact | SHA-256 |
|---|---|
| `docs/metodologia.html` | `b191bc881b25db0c81767d08734b2c83b8ed72eb8441d42c244ba2b96472e47a` |
| `docs/methodology-pt-br.html` | `717a0399606009e0c6de69df643502a301c00b519ebc79af57021c1c847cd4cb` |
| `docs/methodology_report_en.Rmd` | `ab41af75c5a0e894ce271529ba79f747a6dc90136f30262f17d5c9448fd60886` |
| `docs/methodology_report_pt_br.Rmd` | `a78084837631a6fdf638a46b4ce691c7c40adca97d57729b17549f78ad8a99a7` |
| Pandoc template | `822e98b57513717e2ac9bc7d375f2c12d4176810f2e4c7fe3c223c4f5289726c` |
| Publication CSS | `a016e529f14d5f882b1ab1f7f9db2b3f74d6fe65e0b8f13f7c25f6e15dfebb11` |

## Document checks

| Check | EN | PT-BR | Result |
|---|---:|---:|---|
| Structured author blocks | 2 | 2 | PASS |
| Tables | 13 | 13 | PASS |
| Figures | 10 | 10 | PASS |
| Code blocks | 4 | 4 | PASS |
| MathML expressions | 9 | 9 | PASS |
| Alphabetized author-date references | 13 | 13 | PASS |
| Semantic figure/table/reference IDs | 36 | 36 | PASS |
| Missing internal-link destinations | 0 | 0 | PASS |
| Residual isolated numeric citations | 0 | 0 | PASS |
| AI disclosure in body and absent from TOC | yes | yes | PASS |
| AI-assisted translation disclosed | yes | yes | PASS |

The EN and PT-BR reports contain the exact requested author credentials. Maria Eduarda de Lima e Silva has no invented email or additional credential. Titles remain above tables and figures, sources and notes remain below them, table headers match the alignment of their columns, and prose and references remain justified.

## Localized repository files

| File group | PT-BR | EN | Validation |
|---|---:|---:|---|
| R programs | 7 | 7 | all 14 parse successfully |
| Treated CSV files | 6 | 6 | dimensions, numeric values, UF names, and Brazil label pass |
| Treated XLSX workbooks | 13 | 13 | formulas/numeric-cell counts preserved; all EN workbooks open in Microsoft Excel |

The generation scripts, glossary, and repo-local `methodology-rmarkdown` skill document the protected terms and the deterministic bilingual workflow.

## Responsive browser validation

Microsoft Edge was exercised through Playwright. The language dropdown was opened and closed at every width.

| Width | EN `clientWidth/scrollWidth` | PT-BR `clientWidth/scrollWidth` | TOC columns | Result |
|---:|---:|---:|---:|---|
| 320 px | 320 / 320 | 320 / 320 | 1 | PASS |
| 360 px | 360 / 360 | 360 / 360 | 1 | PASS |
| 390 px | 390 / 390 | 390 / 390 | 1 | PASS |
| 768 px | 768 / 768 | 768 / 768 | 1 | PASS |
| 1024 px | 1024 / 1024 | 1024 / 1024 | 2 | PASS |
| 1440 px | 1440 / 1440 | 1440 / 1440 | 2 | PASS |

At every width and in both languages: zero document-level horizontal bleeding, zero non-scrollable overflow offenders, TOC and language menu contained by the page, internal scrolling limited to necessary containers, target highlighting functional, table and figure titles intact, and zero browser-console errors. Visual inspection of EN and PT-BR at 390 and 1440 px also passed.

## Skill and pipeline validation

- `quick_validate.py`: PASS.
- Skill fixture and package test: PASS.
- Python compilation, Node.js syntax checks, and R parsing: PASS.
- Deterministic EN report-source and R-program localization: PASS.
- Static HTML validation for both languages: PASS.
- Cross-language scientific parity validator: PASS.

## GitHub Pages

Publication verification is pending the commit and push. This section will be updated with the deployed commit, HTTP status, served hashes, redirect behavior, and public responsive results.
