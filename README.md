# National Health Survey 2013–2019: Asthma, Sampling Design, and Indicators

<p align="justify">
The Brazilian National Health Survey (Pesquisa Nacional de Saúde, PNS), conducted by the Brazilian Institute of Geography and Statistics in partnership with the Ministry of Health, is a nationwide household survey with a complex sampling design. This repository documents and implements R procedures for analyzing asthma-related indicators in Brazil in 2013 and 2019 while accounting for weights, strata, and primary sampling units.
</p>

<p align="justify">
The analytical programs estimate lifetime self-reported medical asthma diagnosis, recent attacks, questionnaire-conditional medication use, current smoking, payment for medication, restriction of usual activities, and composite individual monthly income. All indicators use adults aged 18 years or older. The October 2026 revision corrects eligibility, missingness, age comparability and monetary treatment; medication use is not interpreted as a comparable access trend.
</p>

## Main products

- [Methodological report — English](https://ingodube.github.io/PNS_2013_2019/metodologia.html)
- [Methodological report — Portuguese](https://ingodube.github.io/PNS_2013_2019/methodology-pt-br.html)
- [Scientific poster — historical Portuguese version](poster%20(PT-BR)/poster_pns_asma_morrison.pdf) — predates this methodological revision; consult the reports for current estimates.
- [Analytical definitions and output migration](docs/analytical-definitions.md)
- [Validation record](docs/index.validation.md)

## Localized analytical materials

- **Codes:** [`English`](Codes%20(EN)/) and [`Portuguese (PT-BR)`](Codes%20(PT-BR)/).
- **Spreadsheets:** [`English`](Spreadsheets%20(EN)/) and [`Portuguese (PT-BR)`](Spreadsheets%20(PT-BR)/).

## Sampling-design implementation

<p align="justify">
The R programs construct the survey design with <code>PNSIBGE</code> and use <code>survey</code> to incorporate weights, strata, and clusters. For binary proportions, the workflow uses <code>survey::svyciprop(method = "beta")</code> to produce 95% confidence intervals that respect the natural bounds of proportions and support cautious interpretation in small domains.
</p>

## Reproducibility

<p align="justify">
Run a localized program from the repository root. For example:
</p>

```r
source(file.path("Codes (EN)", "PNS Extraction - Asthma.R"))
```

<p align="justify">
Required package versions are recorded in <code>data/analysis_manifest.json</code> and <code>data/sessionInfo.txt</code>. Raw microdata are cached outside this repository. Set <code>PNS_CACHE_DIR</code> to a persistent directory to share downloads and validated intermediate results across runs. Numerical CSV columns use schema 2, documented with eligibility and missingness in the analytical definitions. English and Portuguese entry points use the same analytical core; neither translates questionnaire labels to define events.
</p>

From the repository root, with R, Python (pandas, numpy, openpyxl), Pandoc and
Node/Playwright available:

```text
Rscript tests/test_analysis.R
Rscript tools/run_analysis.R /absolute/path/to/staging
Rscript tests/validate_real_data.R /absolute/path/to/staging
python tools/promote_analysis.py /absolute/path/to/staging --check-only
python tools/promote_analysis.py /absolute/path/to/staging
python tools/compare_revision.py
Rscript .codex/skills/methodology-rmarkdown/scripts/render_bilingual_reports.R .
python tools/validate_bilingual_outputs.py
```

`tests/run_entrypoints.R` executes all fourteen entry points against
`PNS_CACHE_DIR/release` and compares their outputs. Use that default staging
directory when running this integration check. Run the skill's static and
responsive validators for both HTML files before publication. Staging does not
publish anything. Never publish a mixture of old and corrected outputs.

Income retains the eight-component sum, including cash and in-kind earnings.
Known non-receipt is zero; an unknown component makes the total unknown.
Results are expressed at August 2025 prices after the official July 2019
deflation step. The obsolete population-wide asthma medication rate is retired.

<p align="justify">
Artificial intelligence supported code structuring, consistency review, report formatting, rendering automation, and translation. Scientific decisions, interpretation, validation, and final responsibility remain with the authors.
</p>
