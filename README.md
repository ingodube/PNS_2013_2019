# National Health Survey 2013–2019: Asthma, Sampling Design, and Indicators

<p align="justify">
The Brazilian National Health Survey (Pesquisa Nacional de Saúde, PNS), conducted by the Brazilian Institute of Geography and Statistics in partnership with the Ministry of Health, is a nationwide household survey with a complex sampling design. This repository documents and implements R procedures for analyzing asthma-related indicators in Brazil in 2013 and 2019 while accounting for weights, strata, and primary sampling units.
</p>

<p align="justify">
The analytical programs estimate the prevalence of a medical asthma diagnosis, attacks in the last 12 months, oral-medication or inhaler use, population rates, smoking, out-of-pocket payment, absenteeism, lost days, and income. The workflow is designed to remain transparent, auditable, and reproducible, with confidence intervals that respect the PNS sampling design.
</p>

## Main products

- [Methodological report — English](https://ingodube.github.io/PNS_2013_2019/metodologia.html)
- [Scientific poster — Portuguese (PT-BR)](poster%20(PT-BR)/poster_pns_asma_morrison.pdf)

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
Depending on the indicator, the workflow requires <code>PNSIBGE</code>, <code>survey</code>, <code>dplyr</code>, <code>tidyr</code>, <code>ggplot2</code>, <code>writexl</code>, and <code>deflateBR</code>. Raw microdata are not stored in this repository and are retrieved through the mechanisms documented in the R programs.
</p>

<p align="justify">
Artificial intelligence supported code structuring, consistency review, report formatting, rendering automation, and translation. Scientific decisions, interpretation, validation, and final responsibility remain with the authors.
</p>
