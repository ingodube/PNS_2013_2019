# Publication visual standard

## Tokens

- Outer background: `#050505`
- Paper: `#ffffff`
- Text: `#202124`
- Secondary text: `#62666b`
- Rule: `#c9cccf`
- Accent: `#e65b2c`
- Dark accent: `#b8421d`
- Code background: `#f5f5f3`
- Body: Georgia, `17px`, line height `1.64`
- Interface: Arial/Helvetica
- Page: `78rem`
- Header and contents: `68rem`
- Reading column: `46rem`

## Structure

- Present a large, left-aligned title and plain metadata lines.
- Use a 3px orange rule below the metadata.
- Keep the table of contents in an open `<details>` element, with two columns above 960px and one below.
- Apply a thin top rule only to major sections after the first.
- Use serif body text and sans-serif headings, metadata, navigation, tables, and captions.
- Keep figure and table titles above the objects, centered, bold, at `0.78rem`, and in the secondary color.
- Keep sources and notes below the objects, justified with the last line left-aligned and restrained typography.
- Use horizontal table rules only; avoid colored fills and zebra striping.
- Justify references at `0.93rem` and align their last line left.
- Highlight internal link targets with `#fff3eb` without changing document flow.

## Language selector

- Place a compact `Language` dropdown above the title.
- Use a native `<details>` control with keyboard, touch, and pointer support.
- Mark the active language with `aria-current="page"`.
- Keep language names explicit: `English (EN)` and `Português (PT-BR)`.
- Set correct document `lang`, canonical, and `hreflang` metadata for every localized output.

## Responsiveness

- Reduce page padding to `3rem 2.1rem 5rem` at 960px and to `2rem 0.9rem 4rem` at 640px.
- Use a 16px body size at 640px and below.
- Allow scrolling only inside tables, code, or formulas that genuinely require it.
- Maintain `scrollWidth <= clientWidth` for the whole document.
- Break visible URLs and long words; never widen the viewport.

## Prohibitions

- Gradients, large shadows, chips, badges, and decorative cards.
- Additional institutional colors.
- Captions below figures or tables.
- External presentation dependencies in self-contained HTML.
- Changes to data, formulas, scientific interpretation, or baseline figure pixels solely to improve fit.
