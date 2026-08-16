---
name: methodology-rmarkdown
description: Produce, revise, render, validate, and publish scholarly or technical reports written in R Markdown or compatible Pandoc Markdown. Use for methodological, statistical, epidemiological, survey, scientific, or institutional reports that require structured authorship, restrained publication typography, author-date citations, internal cross-references, justified prose, accessible tables and figures, MathML, self-contained HTML, responsive validation, and strict preservation of scientific content.
---

# Methodology R Markdown

## Required workflow

1. Inspect the source `.Rmd`, current rendered output, repository instructions, publication configuration, data dependencies, and existing validation report.
2. Capture a baseline before editing: visible prose, metadata, table cells, links, formulas, code blocks, figure bytes, headings, and document order.
3. Edit the `.Rmd` source and reusable assets; never edit only the generated HTML.
4. Preserve analysis, scientific interpretation, data, formulas, links, code, and figure pixels unless the user explicitly authorizes a change.
5. Render with `scripts/render_report.R` and validate with `scripts/validate_report.py` plus `scripts/validate_responsive.cjs`.
6. Inspect the rendered report at 320, 360, 390, 768, 1024, and 1440 px.
7. Commit and publish only after source, output, skill, and validation report pass.
8. Verify the public URL after publication when the report is hosted.

## Bilingual reports

- Keep one reviewed source as the scientific baseline and generate localized sources deterministically.
- Publish each language as a separate self-contained HTML document; do not duplicate languages in one DOM.
- Use a native `Language` dropdown above the report title and expose explicit EN and PT-BR links.
- Set `lang`, canonical, and `hreflang` metadata for every output.
- Preserve stable semantic IDs so the table of contents, citations, figures, tables, and references map one-to-one across languages.
- Store and enforce a translation glossary with protected terms, geographic names, identifiers, formulas, DOI values, and URLs.
- When localized analytical workbooks are versioned, keep them in clearly language-qualified `Spreadsheets (EN)` and `Spreadsheets (PT-BR)` directories and update every consuming path together.
- For Brazil-focused reports, never translate federative-unit names; translate only the national label when required by the target language.
- Validate scientific parity using underlying numeric values rather than locale-formatted display strings.
- Record artificial-intelligence assistance with translation in the disclosure whenever applicable.

## Read the bundled guidance

- Read `references/visual-standard.md` before changing layout or typography.
- Read `references/preservation-validation.md` before changing content-bearing markup, citations, cross-references, tables, figures, formulas, or publication files.
- Reuse `assets/technical-report.html` and `assets/technical-report.css`; do not recreate parallel templates without a documented need.

## Source rules

- Use structured author metadata with a name, zero or more credential lines, and an optional email.
- Keep report type, subtitle, date, repository link, and keywords as plain metadata lines.
- Use `theme: null`, `math_method: mathml`, a native table of contents, section numbering, and self-contained HTML.
- Give headings, references, tables, and figures stable semantic IDs.
- Keep figure and table titles above their objects and sources or notes below.
- Use author-date citations linked to semantic reference IDs. Keep the bibliography unnumbered and alphabetized.
- Exclude declarations or appendices from the table of contents only when explicitly required, while keeping them in the body.
- Keep heavy analyses disabled during publication rendering; read validated intermediate data when the report workflow already uses them.

## Presentation rules

- Use the bundled restrained publication standard without publisher branding.
- Justify body prose and references; keep the last reference line aligned left.
- Keep table headers aligned like the values in their columns.
- Integrate methodological notes as ordinary prose with the initial label in bold unless the user requests a distinct component.
- Keep visible URLs breakable, wide objects internally scrollable, and document-level horizontal overflow at zero.
- Preserve the same public path unless the user explicitly requests a URL change.

## Commands

Render from the repository root:

```powershell
Rscript .codex/skills/methodology-rmarkdown/scripts/render_report.R --input docs/report.Rmd --output docs/report.html
```

Render this repository's EN and PT-BR reports together:

```powershell
Rscript .codex/skills/methodology-rmarkdown/scripts/render_bilingual_reports.R .
```

Run static validation:

```powershell
python .codex/skills/methodology-rmarkdown/scripts/validate_report.py docs/report.html
```

Run responsive validation with Playwright available through `NODE_PATH`:

```powershell
node .codex/skills/methodology-rmarkdown/scripts/validate_responsive.cjs docs/report.html
```

Record exact commands, versions, hashes, counts, browser widths, and PASS/FAIL results in the repository validation report.

For a bilingual report, run the static and responsive validators against every localized HTML file and verify every public language URL after publication.
