# Bilingual methodological report validation

Local validation date: August 16, 2026 (`America/Bahia`)

Local result: **PASS**

## Publication routes

- English report: `docs/metodologia.html`.
- PT-BR report: `docs/methodology-pt-br.html`.
- Root redirect: `docs/index.html` → `metodologia.html`.
- Stable public URL: `https://ingodube.github.io/PNS_2013_2019/metodologia.html`.
- The native `Language` dropdown links the two self-contained documents.
- Every output includes matching `lang`, canonical, and `hreflang` metadata.

## Rendering and integrity

- R 4.6.1 and Pandoc 3.8.3.
- Direct self-contained HTML with MathML, `theme: null`, the repo-local Pandoc template, and the repo-local CSS.
- Two consecutive bilingual builds produced identical hashes: PASS.
- EN/PT-BR semantic IDs, table numeric values, scientific links, figure bytes, and CSV numeric values agree: PASS.
- All 26 XLSX files, 12 CSV files, and the scientific-poster PDF retained their pre-rename SHA-256 hashes: PASS.

| Artifact | SHA-256 |
|---|---|
| `docs/metodologia.html` | `5cc4021876ea449236d76714482bf0719807f58bfac1fc426e7b035057674295` |
| `docs/methodology-pt-br.html` | `717a0399606009e0c6de69df643502a301c00b519ebc79af57021c1c847cd4cb` |
| `docs/methodology_report_en.Rmd` | `334b24a6a11f7b5f1cb57a0881cfd0db6ee2e5679d34a699e15d34f0dd73d0af` |
| `docs/methodology_report_pt_br.Rmd` | `4292a1a7fbb1d8e3772a162409d2f4b62ce6bd41b9b267289104b6a5e0050e24` |
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
| Semantic figure/table/reference IDs | 37 | 37 | PASS |
| Missing internal-link destinations | 0 | 0 | PASS |
| Residual isolated numeric citations | 0 | 0 | PASS |
| AI disclosure in body and absent from TOC | yes | yes | PASS |
| AI-assisted translation disclosed | yes | yes | PASS |

The English report contains the requested affiliations exactly: `MSc Program in Statistics and Data Science (PGECD/UFBA)` for Ingo Dube Souza and `Department of Industrial Economics (SENAI/CIMATEC)` for both authors. Maria Eduarda de Lima e Silva retains `PhD in Applied Economics (UFRGS)` and has no invented email or additional credential. The PT-BR credentials are unchanged.

Titles remain above tables and figures, sources and notes remain below them, table headers match the alignment of their columns, and prose and references remain justified.

## Localized repository files

| File group | PT-BR | EN | Validation |
|---|---:|---:|---|
| R programs | 7 | 7 | all 14 parse successfully and use the renamed spreadsheet directories |
| CSV spreadsheets | 6 | 6 | dimensions, numeric values, UF names, and Brazil label pass |
| XLSX spreadsheets | 13 | 13 | hashes, formulas, and numeric-cell counts preserved |

The generation scripts, glossary, and repo-local `methodology-rmarkdown` skill document the protected terms and deterministic bilingual workflow.

## Responsive browser validation

Microsoft Edge was exercised through Playwright. The language dropdown and TOC were tested at every width.

| Width | EN `clientWidth/scrollWidth` | PT-BR `clientWidth/scrollWidth` | TOC columns | Result |
|---:|---:|---:|---:|---|
| 320 px | 320 / 320 | 320 / 320 | 1 | PASS |
| 360 px | 360 / 360 | 360 / 360 | 1 | PASS |
| 390 px | 390 / 390 | 390 / 390 | 1 | PASS |
| 768 px | 768 / 768 | 768 / 768 | 1 | PASS |
| 1024 px | 1024 / 1024 | 1024 / 1024 | 2 | PASS |
| 1440 px | 1440 / 1440 | 1440 / 1440 | 2 | PASS |

At every width and in both languages: zero document-level horizontal bleeding, zero non-scrollable overflow offenders, TOC and language menu contained by the page, internal scrolling limited to necessary containers, target highlighting functional, table and figure titles intact, and zero browser-console errors.

## Skill and pipeline validation

- `quick_validate.py`: PASS.
- Skill fixture and package test: PASS.
- Python compilation, Node.js syntax checks, and R parsing: PASS.
- Deterministic EN report-source and R-program localization: PASS.
- Static HTML validation for both languages: PASS.
- Cross-language scientific parity validator: PASS.

## History and GitHub Pages

- Pre-rewrite functional commit: `f9db27e62b7e66733fbf36b4801ca0137032d3c1`.
- Recoverable external bundle: `C:\Users\ingon\Github\PNS_2013_2019-before-message-rewrite-20260816.bundle`; SHA-256 `da8e84e2a234f7675f076dbb862ebc085adc4daa9d615d19bc102ff3f61aa08e`.
- Rewritten publication commit: `54dbc536ea7827289a1e51a62fe0aa8ca6a26c63`.
- All 52 commits preserve their tree sequence, authors, author dates, committers, committer dates, and order: PASS.
- Every reachable commit message uses the approved neutral English vocabulary; no message body remains: PASS.
- `force-with-lease` from the protected remote SHA `d7cfe8ae23349e18b55ee1b0010a8258ca1971ae` to `54dbc536ea7827289a1e51a62fe0aa8ca6a26c63`: PASS.
- `https://ingodube.github.io/PNS_2013_2019/`: HTTP 200 and English entry point: PASS.
- `https://ingodube.github.io/PNS_2013_2019/metodologia.html`: HTTP 200; served SHA-256 `5cc4021876ea449236d76714482bf0719807f58bfac1fc426e7b035057674295`, identical to the local EN file: PASS.
- `https://ingodube.github.io/PNS_2013_2019/methodology-pt-br.html`: HTTP 200; served SHA-256 `717a0399606009e0c6de69df643502a301c00b519ebc79af57021c1c847cd4cb`, identical to the local PT-BR file: PASS.
- Public EN, PT-BR, and root routes at 390 and 1440 px: zero bleed, zero overflow offenders, functional language menu and TOC, correct object counts, correct title order, aligned tables, justified notes/references, and internal target highlighting: PASS.
- The README scientific-poster destination under `poster (PT-BR)` returns HTTP 200 from the GitHub repository: PASS.
- The GitHub repository description is present and written in English: PASS.
- The stable public report path and the poster QR code remain unchanged.
