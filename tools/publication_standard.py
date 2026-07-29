#!/usr/bin/env python3
"""Generate the content-preserving publication wrapper for a canonical HTML report."""

from __future__ import annotations

import argparse
import hashlib
import html
from html.parser import HTMLParser
import json
from pathlib import Path
import sys


DEFAULT_SOURCE = "https://raw.githubusercontent.com/ingodube/PNS_2013_2019/main/docs/metodologia_canonico.html"
DEFAULT_FALLBACK = "https://ingodube.github.io/PNS_2013_2019/metodologia_canonico.html"
DEFAULT_REPOSITORY = "https://github.com/ingodube/PNS_2013_2019"
DEFAULT_TITLE = (
    "Implementação do Plano Amostral da Pesquisa Nacional de Saúde no R: "
    "um estudo de caso sobre a asma no Brasil"
)


TEMPLATE = r'''<!doctype html>
<html lang="pt-BR">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="icon" href="data:,">
  <title>__SHELL_TITLE__</title>
  <style>
    :root {
      color-scheme: light;
      --outer: #050505;
      --paper: #ffffff;
      --ink: #202124;
      --muted: #62666b;
      --rule: #c9cccf;
      --accent: #e65b2c;
      --accent-dark: #b8421d;
      --code-bg: #f5f5f3;
      --measure: 46rem;
      --page-max: 78rem;
      --body-font: Georgia, "Times New Roman", serif;
      --ui-font: Arial, Helvetica, sans-serif;
    }

    * { box-sizing: border-box; }

    html {
      max-width: 100%;
      overflow-x: clip;
      scroll-behavior: smooth;
    }

    body {
      margin: 0;
      max-width: 100%;
      min-width: 0;
      overflow-x: clip;
      background: var(--outer);
      color: var(--ink);
      font-family: var(--body-font);
      font-size: 17px;
      line-height: 1.64;
    }

    a {
      color: var(--accent-dark);
      text-decoration-thickness: 1px;
      text-underline-offset: 0.15em;
    }

    a:hover { color: var(--accent); }

    .skip-link {
      position: fixed;
      top: 0;
      left: 0;
      z-index: 10;
      padding: 0.55rem 0.75rem;
      background: var(--paper);
      color: var(--ink);
      font-family: var(--ui-font);
      transform: translateY(-120%);
    }

    .skip-link:focus { transform: translateY(0); }

    .page {
      width: min(100%, var(--page-max));
      min-width: 0;
      margin: 0 auto;
      padding: clamp(2rem, 6vw, 5rem) clamp(1.25rem, 5vw, 4rem);
      background: var(--paper);
    }

    .article-header,
    .publication-shell,
    .loading,
    .load-error {
      width: min(100%, var(--measure));
      min-width: 0;
      margin-right: auto;
      margin-left: auto;
    }

    .article-header { margin-bottom: 2.3rem; }

    .hero-kicker {
      margin: 0 0 0.75rem;
      color: var(--muted);
      font-family: var(--ui-font);
      font-size: 0.75rem;
      font-weight: 700;
      letter-spacing: 0.1em;
      text-transform: uppercase;
    }

    .article-header h1 {
      margin: 0;
      color: var(--ink);
      font-family: var(--ui-font);
      font-size: clamp(2rem, 6vw, 3.55rem);
      font-weight: 750;
      letter-spacing: -0.035em;
      line-height: 1.03;
      overflow-wrap: anywhere;
    }

    .hero-subtitle {
      margin: 1.2rem 0;
      color: var(--muted);
      font-size: 1.08rem;
      line-height: 1.55;
      text-align: left;
    }

    .hero-meta {
      margin: 0;
      padding-bottom: 1rem;
      border-bottom: 3px solid var(--accent);
      color: var(--muted);
      font-family: var(--ui-font);
      font-size: 0.86rem;
      line-height: 1.5;
      overflow-wrap: anywhere;
    }

    .chip-row {
      margin-top: 0.8rem;
      color: var(--muted);
      font-family: var(--ui-font);
      font-size: 0.78rem;
    }

    .chip-row p { margin: 0; text-align: left; }
    .chip { display: inline; }
    .chip + .chip::before { content: " · "; color: var(--rule); }

    .toc-panel {
      width: 100%;
      min-width: 0;
      margin: 0 0 2.5rem;
      padding: 1rem 0 1.2rem;
      border-top: 1px solid var(--rule);
      border-bottom: 1px solid var(--rule);
      font-family: var(--ui-font);
    }

    .toc-summary {
      display: none;
      color: var(--ink);
      cursor: pointer;
      font-size: 0.84rem;
      font-weight: 700;
      letter-spacing: 0.08em;
      list-style: none;
      text-transform: uppercase;
    }

    .toc-summary::-webkit-details-marker { display: none; }

    .toc-title {
      margin: 0 0 0.8rem;
      color: var(--ink);
      font-size: 0.78rem;
      font-weight: 700;
      letter-spacing: 0.1em;
      text-transform: uppercase;
    }

    .toc-list {
      margin: 0;
      padding: 0;
      columns: 2;
      column-gap: 2rem;
      list-style: none;
    }

    .toc-list li {
      break-inside: avoid;
      margin: 0 0 0.3rem;
      text-align: left;
    }

    .toc-list a {
      display: block;
      padding: 0.16rem 0;
      color: var(--muted);
      font-size: 0.79rem;
      line-height: 1.35;
      overflow-wrap: anywhere;
      text-decoration: none;
    }

    .toc-list a:hover,
    .toc-list a.active { color: var(--accent-dark); }
    .toc-level-2 a { padding-left: 0.75rem; }
    .toc-level-3 a { padding-left: 1.5rem; font-size: 0.74rem; }

    .header-section-number::after { content: "."; }

    .article-content,
    .article-content > *,
    .publication-shell { min-width: 0; max-width: 100%; }

    .article-content h1,
    .article-content h2,
    .article-content h3,
    .article-content h4 {
      color: var(--ink);
      font-family: var(--ui-font);
      font-weight: 700;
      letter-spacing: -0.012em;
      line-height: 1.2;
      overflow-wrap: anywhere;
      scroll-margin-top: 1rem;
    }

    .article-content h1 { margin: 2.8rem 0 1rem; font-size: 1.8rem; }
    .article-content h2 { margin: 2.2rem 0 0.85rem; font-size: 1.35rem; }
    .article-content h3 { margin: 1.8rem 0 0.7rem; font-size: 1.08rem; }
    .article-content h4 { margin: 1.5rem 0 0.6rem; font-size: 0.98rem; }

    .article-content p { margin: 0 0 1.05rem; text-align: justify; }
    .article-content li { text-align: left; }

    .article-content a,
    .breakable-url,
    .references a,
    .reference-entry {
      overflow-wrap: anywhere;
      word-break: break-word;
    }

    .callout {
      margin: 1.15rem 0;
      padding: 0;
      border: 0;
      background: transparent;
    }

    .callout strong {
      display: inline;
      color: var(--ink);
      font-family: var(--ui-font);
      font-size: 0.88rem;
      font-weight: 700;
      letter-spacing: 0;
      text-transform: none;
    }

    .callout p { display: inline; margin: 0; }

    .table-scroll,
    .math.display,
    .MathJax_Display {
      width: 100%;
      max-width: 100%;
      overflow-x: auto;
      overflow-y: hidden;
      -webkit-overflow-scrolling: touch;
    }

    table,
    .report-table {
      width: 100%;
      margin: 1.3rem 0 0.65rem;
      border-collapse: collapse;
      background: var(--paper);
      color: var(--ink);
      font-family: var(--ui-font);
      font-size: 0.78rem;
      line-height: 1.4;
    }

    table caption {
      caption-side: top;
      margin: 0 0 0.45rem;
      color: var(--ink);
      font-weight: 700;
      text-align: left;
    }

    table th,
    table td {
      padding: 0.45rem 0.55rem;
      border: 0;
      border-bottom: 1px solid var(--rule);
      background: var(--paper);
      text-align: left;
      vertical-align: top;
    }

    table thead th { border-top: 1px solid var(--ink); border-bottom-color: var(--ink); }
    table tbody tr:last-child td { border-bottom-color: var(--ink); }
    table .numeric-cell { text-align: right; font-variant-numeric: tabular-nums; }

    figure.publication-figure {
      width: 100%;
      max-width: 100%;
      margin: 2rem 0 1.5rem;
    }

    figure.publication-figure > img,
    figure.publication-figure > div,
    .article-content img,
    .article-content svg,
    .article-content canvas {
      display: block;
      max-width: 100%;
      height: auto;
    }

    figure.publication-figure figcaption {
      margin: 0.65rem 0 0;
      color: var(--muted);
      font-family: var(--ui-font);
      font-size: 0.78rem;
      line-height: 1.45;
      text-align: left;
      overflow-wrap: anywhere;
    }

    .figure-title { display: none !important; }

    .source-note,
    .source-after-note,
    .table-source,
    .table-note,
    .figure-note {
      color: var(--muted);
      font-family: var(--ui-font);
      font-size: 0.76rem;
      line-height: 1.45;
      text-align: left !important;
    }

    pre,
    code {
      font-family: Consolas, "Liberation Mono", monospace;
      overflow-wrap: anywhere;
      word-break: break-word;
    }

    pre {
      width: 100%;
      max-width: 100%;
      margin: 1.2rem 0;
      padding: 0.8rem;
      overflow-x: auto;
      border-top: 1px solid var(--rule);
      border-bottom: 1px solid var(--rule);
      background: var(--code-bg);
      font-size: 0.76rem;
      line-height: 1.5;
    }

    code { white-space: pre-wrap; }
    .references p, .references li { text-align: left; }

    .loading,
    .load-error {
      color: var(--ink);
      font-family: var(--ui-font);
    }

    @media (max-width: 960px) {
      .toc-summary {
        display: flex;
        align-items: center;
        justify-content: space-between;
      }

      .toc-summary::after { content: "+"; color: var(--accent); font-size: 1rem; }
      .toc-disclosure[open] .toc-summary::after { content: "−"; }
      .toc-title { display: none; }
      .toc-nav { padding-top: 0.9rem; }
      .toc-list { columns: 1; }
    }

    @media (max-width: 640px) {
      body { font-size: 16px; }
      .page { padding: 1.35rem 0.9rem 2.5rem; }
      .article-header h1 { font-size: clamp(1.85rem, 10vw, 2.45rem); }
      .article-content p { text-align: left; hyphens: none; }
      .references p, .references li { padding-left: 0; text-indent: 0; }
      pre { white-space: pre-wrap; overflow-x: hidden; }
      pre code { white-space: pre-wrap; }
      table { width: max-content; min-width: 100%; }
    }

    @media print {
      body { background: var(--paper); }
      .page { width: 100%; max-width: none; padding: 0; }
      .toc-panel { break-after: page; }
    }
  </style>
  <script>
    window.MathJax = {
      tex: { inlineMath: [['\\(', '\\)']], displayMath: [['\\[', '\\]']] },
      options: { skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code'] }
    };
  </script>
  <script defer src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
</head>
<body>
  <a class="skip-link" href="#article">Pular para o artigo</a>
  <main class="page" id="page">
    <p class="loading" id="loading" role="status">Carregando o relatório...</p>
  </main>

  <script>
    (() => {
      'use strict';

      const DEFAULT_SOURCES = [__SOURCE_URL__, __FALLBACK_URL__];
      const REPOSITORY_URL = __REPOSITORY_URL__;
      const params = new URLSearchParams(window.location.search);
      const sources = params.get('source') ? [params.get('source')] : DEFAULT_SOURCES;
      const page = document.getElementById('page');
      const cleanText = value => (value || '').replace(/\s+/g, ' ').trim();

      async function fetchFirstAvailable(urls) {
        let lastError;
        for (const url of urls) {
          try {
            const response = await fetch(url, { mode: 'cors', credentials: 'omit' });
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            return { markup: await response.text(), url };
          } catch (error) {
            lastError = error;
          }
        }
        throw lastError || new Error('Não foi possível obter o documento canônico.');
      }

      function sanitizeDocument(doc) {
        doc.querySelectorAll('script, style, link[rel="stylesheet"], iframe, object, embed, form')
          .forEach(node => node.remove());
        doc.querySelectorAll('*').forEach(node => {
          [...node.attributes].forEach(attribute => {
            if (/^on/i.test(attribute.name)) node.removeAttribute(attribute.name);
            if (attribute.name === 'style') node.removeAttribute('style');
          });
        });
      }

      function markBreakableLinks(root) {
        root.querySelectorAll('a').forEach(link => {
          const visibleText = cleanText(link.textContent);
          if (/^(?:https?:\/\/|www\.)/i.test(visibleText) || visibleText.length > 48) {
            link.classList.add('breakable-url');
          }
        });
      }

      function preserveMathSource(root) {
        root.querySelectorAll('.math').forEach(node => {
          node.dataset.texSource = node.textContent;
        });
      }

      function markNumericCells(root) {
        root.querySelectorAll('table').forEach(table => {
          [...table.rows].forEach(row => {
            [...row.cells].forEach(cell => {
              const value = cleanText(cell.textContent);
              if (/^[−+-]?(?:\d{1,3}(?:[. ]\d{3})*|\d+)(?:[,.]\d+)?(?:%|\s*–\s*[\d,.]+)?$/.test(value)) {
                cell.classList.add('numeric-cell');
              }
            });
          });
        });
      }

      function wrapTables(root) {
        root.querySelectorAll('table').forEach(table => {
          if (table.parentElement?.classList.contains('table-scroll')) return;
          const wrapper = document.createElement('div');
          wrapper.className = 'table-scroll';
          wrapper.setAttribute('role', 'region');
          wrapper.setAttribute(
            'aria-label',
            cleanText(table.querySelector('caption')?.textContent) || 'Tabela com rolagem horizontal'
          );
          wrapper.tabIndex = 0;
          table.before(wrapper);
          wrapper.appendChild(table);
        });
      }

      function rebuildFigures(root) {
        [...root.querySelectorAll('.figure-title')].forEach((caption, index) => {
          let candidate = caption.nextElementSibling;
          let image = null;
          let imageContainer = null;
          while (candidate && !/^H[1-4]$/.test(candidate.tagName)) {
            image = candidate.matches('img') ? candidate : candidate.querySelector('img');
            if (image) {
              imageContainer = candidate;
              break;
            }
            candidate = candidate.nextElementSibling;
          }
          if (!image || !imageContainer) return;

          const captionText = cleanText(caption.textContent);
          const figure = document.createElement('figure');
          const figcaption = document.createElement('figcaption');
          const captionId = `figure-caption-${index + 1}`;
          figure.className = 'publication-figure';
          figure.setAttribute('aria-labelledby', captionId);
          figcaption.id = captionId;
          while (caption.firstChild) figcaption.appendChild(caption.firstChild);
          caption.before(figure);
          figure.append(imageContainer, figcaption);
          caption.remove();
          if (!cleanText(image.alt)) image.alt = captionText;
          image.loading = 'lazy';
          image.decoding = 'async';
        });
      }

      function ensureHeadingIds(root) {
        const headings = [...root.querySelectorAll('h1, h2, h3')];
        const used = new Set();
        headings.forEach((heading, index) => {
          let base = heading.id || heading.closest('.section[id]')?.id || '';
          if (!base) {
            base = cleanText(heading.textContent)
              .normalize('NFD')
              .replace(/[\u0300-\u036f]/g, '')
              .toLowerCase()
              .replace(/[^a-z0-9]+/g, '-')
              .replace(/^-|-$/g, '') || `secao-${index + 1}`;
          }
          let id = base;
          let suffix = 2;
          while (used.has(id)) id = `${base}-${suffix++}`;
          used.add(id);
          heading.id = id;
        });
        return headings;
      }

      function buildToc(headings) {
        const list = document.createElement('ol');
        list.className = 'toc-list';
        headings.forEach(heading => {
          const item = document.createElement('li');
          const link = document.createElement('a');
          item.className = `toc-level-${heading.tagName.slice(1)}`;
          link.href = `#${encodeURIComponent(heading.id)}`;
          [...heading.childNodes].forEach(node => link.appendChild(node.cloneNode(true)));
          item.appendChild(link);
          list.appendChild(item);
        });
        return list;
      }

      function observeSections(headings, tocRoot) {
        if (!('IntersectionObserver' in window)) return;
        const links = new Map(
          [...tocRoot.querySelectorAll('a[href^="#"]')]
            .map(link => [decodeURIComponent(link.hash.slice(1)), link])
        );
        const observer = new IntersectionObserver(entries => {
          const visible = entries
            .filter(entry => entry.isIntersecting)
            .sort((a, b) => a.boundingClientRect.top - b.boundingClientRect.top)[0];
          if (!visible) return;
          links.forEach(link => link.classList.remove('active'));
          links.get(visible.target.id)?.classList.add('active');
        }, { rootMargin: '-8% 0px -78% 0px', threshold: [0, 1] });
        headings.forEach(heading => observer.observe(heading));
      }

      function buildHeader(sourceDoc) {
        const sourceHero = sourceDoc.querySelector('.report-hero');
        const header = document.createElement('header');
        header.className = 'article-header';

        if (sourceHero) {
          const kicker = sourceHero.querySelector('.hero-kicker');
          const sourceTitle = sourceHero.querySelector('.hero-title');
          const subtitle = sourceHero.querySelector('.hero-subtitle');
          const meta = sourceHero.querySelector('.hero-meta');
          const keywords = sourceHero.querySelector('.chip-row');
          if (kicker) header.appendChild(kicker);
          if (sourceTitle) {
            const title = document.createElement('h1');
            while (sourceTitle.firstChild) title.appendChild(sourceTitle.firstChild);
            header.appendChild(title);
          }
          if (subtitle) header.appendChild(subtitle);
          if (meta) header.appendChild(meta);
          if (keywords) header.appendChild(keywords);
          sourceHero.remove();
          return header;
        }

        const title = document.createElement('h1');
        title.textContent = cleanText(sourceDoc.querySelector('#header .title')?.textContent)
          || cleanText(sourceDoc.querySelector('title')?.textContent);
        const meta = document.createElement('p');
        meta.className = 'hero-meta';
        const repository = document.createElement('a');
        repository.href = REPOSITORY_URL;
        repository.textContent = 'Repositório do projeto';
        meta.appendChild(repository);
        header.append(title, meta);
        return header;
      }

      function buildPage(sourceDoc) {
        sanitizeDocument(sourceDoc);
        const header = buildHeader(sourceDoc);
        const title = cleanText(header.querySelector('h1')?.textContent);
        const sourceContent = sourceDoc.querySelector('.toc-content')
          || sourceDoc.querySelector('.main-container')
          || sourceDoc.body;

        sourceContent.querySelectorAll('#header, .report-hero, #TOC, .tocify, .tocify-extend-page')
          .forEach(node => node.remove());
        markBreakableLinks(sourceContent);
        preserveMathSource(sourceContent);
        markNumericCells(sourceContent);
        rebuildFigures(sourceContent);
        wrapTables(sourceContent);

        const article = document.createElement('article');
        article.id = 'article';
        article.className = 'article-content';
        while (sourceContent.firstChild) article.appendChild(sourceContent.firstChild);

        const headings = ensureHeadingIds(article);
        const aside = document.createElement('aside');
        aside.className = 'toc-panel';
        aside.setAttribute('aria-label', 'Sumário do relatório');
        aside.innerHTML = `
          <details class="toc-disclosure">
            <summary class="toc-summary">Sumário</summary>
            <p class="toc-title">Sumário</p>
            <nav class="toc-nav" aria-label="Seções"></nav>
          </details>`;
        aside.querySelector('.toc-nav').appendChild(buildToc(headings));

        const disclosure = aside.querySelector('.toc-disclosure');
        const tocMedia = window.matchMedia('(max-width: 960px)');
        const syncToc = event => { disclosure.open = !event.matches; };
        syncToc(tocMedia);
        tocMedia.addEventListener?.('change', syncToc);
        aside.addEventListener('click', event => {
          if (event.target.closest('a') && tocMedia.matches) disclosure.open = false;
        });

        const shell = document.createElement('div');
        shell.className = 'publication-shell';
        shell.append(aside, article);
        if (title) document.title = title;
        page.replaceChildren(header, shell);
        observeSections(headings, aside);
        window.MathJax?.typesetPromise?.([article]).catch(console.warn);
      }

      fetchFirstAvailable(sources)
        .then(({ markup }) => buildPage(new DOMParser().parseFromString(markup, 'text/html')))
        .catch(error => {
          console.error(error);
          page.innerHTML = `
            <section class="load-error" role="alert">
              <h1>Não foi possível carregar o relatório</h1>
              <p>O documento canônico não pôde ser obtido. Verifique a conexão e tente novamente.</p>
              <p><a href="${DEFAULT_SOURCES[1]}">Abrir o documento canônico</a></p>
            </section>`;
        });
    })();
  </script>
</body>
</html>
'''


class HTMLProbe(HTMLParser):
    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.html = 0
        self.body = 0

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        if tag.lower() == "html":
            self.html += 1
        elif tag.lower() == "body":
            self.body += 1


def git_blob_sha(data: bytes) -> str:
    """Match Git's text normalization for the repository's CRLF working tree."""
    normalized = data.replace(b"\r\n", b"\n")
    return hashlib.sha1(
        f"blob {len(normalized)}\0".encode("ascii") + normalized
    ).hexdigest()


def validate_input(path: Path, expected_blob: str | None) -> str:
    data = path.read_bytes()
    try:
        text = data.decode("utf-8")
    except UnicodeDecodeError as exc:
        raise ValueError(f"Input must be UTF-8 HTML: {path}") from exc
    probe = HTMLProbe()
    probe.feed(text)
    if (probe.html, probe.body) != (1, 1):
        raise ValueError(f"Input must contain one html and one body element: {path}")
    blob = git_blob_sha(data)
    if expected_blob and blob.lower() != expected_blob.lower():
        raise ValueError(f"Input Git blob mismatch: expected {expected_blob}, found {blob}")
    return blob


def render(source_url: str, fallback_url: str, repository_url: str, shell_title: str) -> bytes:
    output = TEMPLATE
    output = output.replace("__SOURCE_URL__", json.dumps(source_url, ensure_ascii=False))
    output = output.replace("__FALLBACK_URL__", json.dumps(fallback_url, ensure_ascii=False))
    output = output.replace("__REPOSITORY_URL__", json.dumps(repository_url, ensure_ascii=False))
    output = output.replace("__SHELL_TITLE__", html.escape(shell_title, quote=False))
    return output.encode("utf-8")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input_html", type=Path)
    parser.add_argument("output_html", type=Path)
    parser.add_argument("--source-url", default=DEFAULT_SOURCE)
    parser.add_argument("--fallback-url", default=DEFAULT_FALLBACK)
    parser.add_argument("--repository-url", default=DEFAULT_REPOSITORY)
    parser.add_argument("--shell-title", default=DEFAULT_TITLE)
    parser.add_argument("--expected-input-blob")
    parser.add_argument("--validate-against", type=Path)
    parser.add_argument("--expected-output-sha256")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    input_path = args.input_html.resolve()
    output_path = args.output_html.resolve()
    if input_path == output_path:
        raise ValueError("Input and output must differ to prevent a self-fetch loop.")

    input_blob = validate_input(input_path, args.expected_input_blob)
    output = render(args.source_url, args.fallback_url, args.repository_url, args.shell_title)
    output_sha256 = hashlib.sha256(output).hexdigest()

    if args.validate_against and output != args.validate_against.read_bytes():
        reference_sha = hashlib.sha256(args.validate_against.read_bytes()).hexdigest()
        raise ValueError(
            f"Generated output differs from reference: generated={output_sha256}, reference={reference_sha}"
        )
    if args.expected_output_sha256 and output_sha256.lower() != args.expected_output_sha256.lower():
        raise ValueError(
            f"Generated SHA-256 mismatch: expected {args.expected_output_sha256}, found {output_sha256}"
        )

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_bytes(output)
    print(f"input_git_blob={input_blob}")
    print(f"output_sha256={output_sha256}")
    print(f"output_bytes={len(output)}")
    print(f"output={output_path}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, ValueError) as exc:
        print(f"error: {exc}", file=sys.stderr)
        raise SystemExit(2)
