# Preservation and validation

## Required baseline

Before editing, record the current HTML SHA-256 and extract:

- visible text by block;
- headings and document order;
- table cells and order;
- link targets;
- TeX or MathML formulas;
- code-block text;
- decoded figure bytes;
- bibliographic references and their fields.

## Authorized differences

Define accepted differences explicitly. Styling and localization may change classes, presentational elements, metadata order, generated numbering, citation formatting, bibliography order, localized labels, decimal display conventions, and translated prose. They must not change scientific claims, numeric values, formulas, data order, or bibliographic fields without explicit authorization.

## Bilingual preservation

- Maintain a one-to-one mapping of sections, tables, figures, formulas, citations, and references across languages.
- Use the same semantic IDs in every localized report.
- Compare underlying numeric values rather than locale-formatted display strings.
- Never translate the names of Brazil's 27 federative units; localize only `Brasil` as `Brazil` in English.
- Keep PNS variable codes, package/API names, formulas, DOI values, and URLs unchanged.
- Preserve source-language publication titles unless an official translated title is used.
- Record any artificial-intelligence support used for translation in the report disclosure.

## Citations and cross-references

- Use narrative `Author (year)` or parenthetical `(Author, year)` citations.
- Separate multiple sources with semicolons.
- Use semantic IDs such as `ref-author-year`, `fig-description`, and `tab-description`.
- Keep the bibliography unnumbered and alphabetized by personal or institutional author.
- Require unique IDs and an existing target for every internal `href`.
- Confirm that table-of-contents and body numbering match.
- Confirm that targets receive the intended `:target` highlight.

## Acceptance criteria

- Valid, self-contained HTML with viewport metadata.
- Expected counts of tables, figures, code blocks, formulas, links, and references in each language.
- Equality of numeric cells, formulas, scientific code, external links, and document order across languages.
- No isolated numeric citations when author-date style is selected.
- Figure and table titles immediately before their objects.
- Table headers aligned like their column values.
- Zero document-level overflow at 320, 360, 390, 768, 1024, and 1440px.
- Open, contained, and navigable contents; no broken internal targets.
- Functional language dropdown and valid cross-language destinations.
- Local verification followed by verification at the public URLs.
