# AGENTS.md

## Local workflow

- Use `C:\Users\ingon\Github\PNS_2013_2019` as the primary repository directory in Codex and VS Code.
- The expected remote is `origin`, pointing to `https://github.com/ingodube/PNS_2013_2019.git`.
- The default working branch is `main`.

Before editing, confirm repository state with `git status --short --branch`, `git remote -v`, and `git log --oneline --max-count=5`.

## Localized analytical artifacts

- Keep English R programs in `Codes (EN)/` and Portuguese programs in `Codes (PT-BR)/`.
- Keep English spreadsheets in `Spreadsheets (EN)/` and Portuguese outputs in `Spreadsheets (PT-BR)/`.
- Treat the PT-BR tables as the validated scientific baseline. English tables may differ only in filenames, headers, textual categories, display punctuation, and `Brasil` → `Brazil`.
- Never translate the names of Brazil's 27 federative units, PNS variable codes, formulas, package/API names, DOI values, or URLs.
- Keep accidental root-level `df_*.csv`, `df_*.xlsx`, `Rplots.pdf`, `~$*.xlsx`, and duplicate local repository folders out of Git.
- The current Portuguese poster belongs at `poster (PT-BR)/poster_pns_asma_morrison.pdf`; do not recreate `outputs/posters/`.

## Bilingual report

- English is the default public language at `docs/metodologia.html`.
- Portuguese is published at `docs/methodology-pt-br.html`.
- `docs/index.html` must redirect to the English default.
- Edit `docs/methodology_report_pt_br.Rmd` as the PT-BR scientific baseline and regenerate `docs/methodology_report_en.Rmd` with `tools/localize_report.py`.
- Keep semantic section, table, figure, and reference IDs aligned across languages.
- Use the repo-local `.codex/skills/methodology-rmarkdown/` workflow for rendering and validation.
- The language menu, `lang`, canonical links, and `hreflang` links must remain valid and responsive.

## Publication

- Push reviewed commits with `git push origin main` only after both local reports and the validation report pass.
- After pushing, confirm that `main...origin/main` is not ahead and verify both GitHub Pages URLs.
- Preserve the stable `metodologia.html` path so existing links and the poster QR code continue to work.
