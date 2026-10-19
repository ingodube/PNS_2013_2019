# Analytical definitions — revision of 18 October 2026

All indicators use selected residents aged 18–130, the valid adult age range
in the dictionaries. Selection of the resident precedes construction of the
survey design. Age and outcome restrictions follow construction of the design.
No age/sex standardisation is performed. Proportions in machine-readable files
are fractions; reports multiply them by 100. Means retain days or BRL.

| Identifier | Event/value | Eligible domain; valid information |
|---|---|---|
| asthma_diagnosis | Q074 = 1 | Adults; Q074 in 1,2 |
| asthma_attack | Q076 = 1 | Q074 = 1; Q076 in 1,2 |
| asthma_medication_conditional, 2013 | Q07701 = 1 | Q074 = Q076 = 1; Q07701 in 1,2 |
| asthma_medication_conditional, 2019 | Q07704 in 1,2 OR Q07708 = 1 | Q074 = Q076 = Q07601 = 1; a positive modality establishes use; non-use requires Q07704 = 3 AND Q07708 = 2 |
| current_smoking | P050 in 1,2 | Adults; P050 in 1,2,3 |
| payment_oral | Q07707 = 1 | 2019 medication eligibility AND Q07704 in 1,2; Q07707 in 1,2 |
| payment_inhaler | Q07711 = 1 | 2019 medication eligibility AND Q07708 = 1; Q07711 in 1,2 |
| activity_restriction | J002 = 1 | Adults; J002 in 1,2 |
| restricted_days | J003 | J002 = 1; days in 1–14 |
| respiratory_share | 2013: J004 in 10,11; 2019: J00402 = 5 | J002 = 1; cause valid (1–24 in 2013, 1–14 in 2019) |
| respiratory_days | J003 | J002 = 1 AND respiratory cause; days in 1–14 |
| cause_k_share | J00402 = k | 2019, J002 = 1; valid cause |
| cause_k_days | J003 | 2019, J002 = 1 AND J00402 = k; days in 1–14 |
| income_composed | Eight-component sum, BRL August 2025 | 2019 adults with all components established |
| income_incomplete | At least one unknown component | All 2019 adults |

## Income routing

Components are E01602/E01604 (main-job cash/in-kind), E01802/E01804
(other-job cash/in-kind), F001021 (retirement/pension), F007021
(alimony/donations/allowances), F008021 (rent) and VDF00102 (other income).
They are not replaced by household per capita income.

Work receipt checkboxes are E01601/E01801 = 1 for cash and E01603/E01803 = 2
for in-kind earnings. A blank checkbox establishes zero only if another
modality was selected, including a valid benefits-only category in E01605/E01805.
Ignored checkboxes do not establish zero. VDE001 = 2 or VDE002 = 2 establishes
no current job; E01401 = 7 establishes an unpaid main job. E011 = 1 establishes
no other job. These routes establish zero only for an absent or zero amount;
contradictory positive amounts without recorded receipt remain unknown.

F001011/F007011/F008011/VDF001 = 2 establishes non-receipt; code 1 requires a
valid amount. Missing receipt information remains unknown. Code 99999999 is
ignored for the first seven amounts. The official dictionary gives VDF00102
a range of 00000001–99999999, with blank for no receipt/inapplicability; it
does not designate the upper endpoint as ignored. No global sentinel rule is used.

The sum uses `na.rm = FALSE`. Unknown components invalidate the total, while
complete zero totals remain in the denominator. Known nominal amounts are
multiplied once by the official PNS deflator (July 2019 prices) and by
7323.91 / 5224.18 (SIDRA table 1737, variable 2266, August 2025 / July 2019).
No intermediate rounding occurs. The eight nominal components are assessed
before applying this common respondent-specific multiplier.

## Variance, missingness and comparisons

The selected-resident design uses V0029 and post-stratification on V00293,
pairing it with V00292. Resulting weights must match V00291 within relative
tolerance 1e-6, and calibration totals within 1e-10. No multistage FPC is inferred.
`survey.lonely.psu = 'adjust'` does not imply that post-stratified lonely domains
receive the additional domain adjustment.

Proportion CIs use the beta method; mean CIs use t critical values with domain
degrees of freedom. Boundary proportions retain their point estimates but no
CI. Empty or unestimable domains have explicit status. Calibrated subsets can
retain zero-weight rows outside the domain; `na.rm = TRUE` prevents their
structural missing values from contaminating estimates.

Missingness columns concern eligible respondents. Unknown answers to ancestor
questions can prevent eligibility from being established; downstream missing
percentages alone are therefore not a measure of all questionnaire nonresponse.
Excluding unknown outcomes or income can introduce selection bias.

Contrasts are limited to diagnosis, attacks and current smoking. Variances are
added under an explicit between-edition independence approximation, with
Welch–Satterthwaite degrees of freedom. Cross-edition covariance is unavailable.
State contrasts are exploratory. No equivalence, causal or medication-access
trend is inferred. Respiratory cause groups differ (2019 includes sinusitis).

## Output contract and migration

Schema 2 uses `(indicator, year, geo)` as the unique result key. `geo` is an IBGE
UF code or BR. Columns retain stable machine-readable names in both languages;
UF names are protected and only Brasil/Brazil changes. `n_*` columns are sample
counts and `weighted_*` columns are population totals. `n_event` applies to
proportions. `cv` is unavailable when the point estimate is zero.

The previous spreadsheets used implicit denominators and some invalid indicators.
They are retired as a complete schema-1 release, listed in
`data/retired_outputs.json`, and recoverable at Git commit 90c9049. Current
families are asthma, attacks, medication, smoking, payment, activity and income,
each exported to CSV and XLSX. The retired medication-population rate is not
recreated. Historical Word and poster materials are not numerical sources for
the corrected report.

## Sources

- [IBGE 2013 documentation](https://ftp.ibge.gov.br/PNS/2013/Microdados/Documentacao/)
- [IBGE 2019 documentation](https://ftp.ibge.gov.br/PNS/2019/Microdados/Documentacao/)
- [IBGE deflator instructions](https://ftp.ibge.gov.br/PNS/Documentacao_Geral/PNSIBGE_Deflator.pdf)
- [SIDRA monetary indices](https://apisidra.ibge.gov.br/values/t/1737/n1/all/v/2266/p/201907,202508)
