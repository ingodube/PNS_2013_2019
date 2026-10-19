#!/usr/bin/env python3
from __future__ import annotations

import argparse
import base64
from collections import Counter
import hashlib
from html import unescape
from html.parser import HTMLParser
from pathlib import Path
import re
import sys


class ReportProbe(HTMLParser):
    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.tags = Counter()
        self.ids: list[str] = []
        self.internal_links: list[str] = []
        self.external_links: list[str] = []
        self.classes = Counter()
        self.visible: list[str] = []
        self.reference_texts: list[str] = []
        self.figure_hashes: list[str] = []
        self.table_cells: list[str] = []
        self.code_blocks: list[str] = []
        self._hidden_depth = 0
        self._reference_depth = 0
        self._cell_depth = 0
        self._pre_depth = 0

    def handle_starttag(self, tag: str, attrs) -> None:
        attrs_map = dict(attrs)
        tag = tag.lower()
        self.tags[tag] += 1
        classes = set((attrs_map.get("class") or "").split())
        self.classes.update(classes)
        if attrs_map.get("id"):
            self.ids.append(attrs_map["id"])
        href = attrs_map.get("href")
        if href and href.startswith("#"):
            self.internal_links.append(href[1:])
        elif href:
            self.external_links.append(href)
        if tag in {"script", "style"}:
            self._hidden_depth += 1
        if tag in {"th", "td"}:
            self._cell_depth += 1
            self.table_cells.append("")
        if tag == "pre":
            self._pre_depth += 1
            self.code_blocks.append("")
        if "reference-entry" in classes:
            self._reference_depth += 1
            self.reference_texts.append("")
        if tag == "img" and (attrs_map.get("src") or "").startswith("data:image"):
            payload = attrs_map["src"].split(",", 1)[1]
            self.figure_hashes.append(hashlib.sha256(base64.b64decode(payload)).hexdigest())

    def handle_endtag(self, tag: str) -> None:
        if tag.lower() in {"script", "style"} and self._hidden_depth:
            self._hidden_depth -= 1
        if tag.lower() in {"th", "td"} and self._cell_depth:
            self._cell_depth -= 1
        if tag.lower() == "pre" and self._pre_depth:
            self._pre_depth -= 1
        if tag.lower() == "p" and self._reference_depth:
            self._reference_depth -= 1

    def handle_data(self, data: str) -> None:
        if self._hidden_depth:
            return
        if self._cell_depth:
            self.table_cells[-1] += data
        if self._pre_depth:
            self.code_blocks[-1] += data
        normalized = " ".join(data.split())
        if not normalized:
            return
        self.visible.append(normalized)
        if self._reference_depth:
            self.reference_texts[-1] += (" " if self.reference_texts[-1] else "") + normalized


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Validate a self-contained technical report HTML.")
    parser.add_argument("html", type=Path)
    parser.add_argument("--expected-tables", type=int)
    parser.add_argument("--expected-figures", type=int)
    parser.add_argument("--expected-code-blocks", type=int)
    parser.add_argument("--expected-references", type=int)
    parser.add_argument("--expected-authors", type=int)
    parser.add_argument("--expected-math", type=int)
    parser.add_argument("--required-text", action="append", default=[])
    parser.add_argument("--expected-figure-sha256", action="append", default=[])
    parser.add_argument("--baseline-html", type=Path)
    parser.add_argument("--allow-code-block-change", action="append", type=int, default=[])
    parser.add_argument("--author-date", action="store_true")
    parser.add_argument("--expected-lang")
    parser.add_argument("--retired-id", action="append", default=[], help="Explicit legacy anchor retained for a retirement note")
    return parser.parse_args()


def require(condition: bool, message: str, failures: list[str]) -> None:
    if not condition:
        failures.append(message)


def normalized_artifacts(values: list[str]) -> list[str]:
    return [re.sub(r"\s+", " ", value).strip() for value in values]


def main() -> int:
    args = parse_args()
    data = args.html.read_bytes()
    text = data.decode("utf-8")
    probe = ReportProbe()
    probe.feed(text)
    visible = " ".join(probe.visible)
    failures: list[str] = []

    require(probe.tags["html"] == 1, "expected exactly one html element", failures)
    require(probe.tags["head"] == 1, "expected exactly one head element", failures)
    require(probe.tags["body"] == 1, "expected exactly one body element", failures)
    require('name="viewport"' in text, "viewport metadata missing", failures)
    require("report-page" in probe.classes, "report-page missing", failures)
    require("report-header" in probe.classes, "report-header missing", failures)
    require("report-toc" in probe.classes, "report-toc missing", failures)
    require("report-content" in probe.classes, "report-content missing", failures)
    require("language-switcher" in probe.classes, "language-switcher missing", failures)
    require("data:image" in text, "HTML is not self-contained with embedded images", failures)
    require(len(re.findall(r'hreflang="(?:en|pt-BR)"', text)) >= 4, "language alternate links missing", failures)
    require(
        len(re.findall(r'<a\b[^>]*\baria-current="page"', text)) == 1,
        "active language marker missing or duplicated",
        failures,
    )
    if args.expected_lang:
        require(
            re.search(rf'<html\b[^>]*\blang="{re.escape(args.expected_lang)}"', text) is not None,
            f'expected html lang="{args.expected_lang}"',
            failures,
        )

    duplicates = sorted(key for key, count in Counter(probe.ids).items() if count > 1)
    require(not duplicates, "duplicate IDs: " + ", ".join(duplicates), failures)
    missing = sorted(set(probe.internal_links) - set(probe.ids))
    require(not missing, "missing internal targets: " + ", ".join(missing), failures)

    checks = [
        (args.expected_tables, probe.tags["table"], "tables"),
        (args.expected_figures, len(probe.figure_hashes), "figures"),
        (args.expected_code_blocks, probe.tags["pre"], "code blocks"),
        (args.expected_references, len(probe.reference_texts), "references"),
        (args.expected_authors, probe.classes["author-block"], "author blocks"),
        (args.expected_math, probe.tags["math"], "mathematical expressions"),
    ]
    for expected, actual, label in checks:
        if expected is not None:
            require(actual == expected, f"expected {expected} {label}, found {actual}", failures)

    for required_text in args.required_text:
        require(required_text in visible, f"required text missing: {required_text}", failures)

    if args.author_date:
        prose_html = re.sub(
            r"<(?:pre|code|script|style)\b.*?</(?:pre|code|script|style)>",
            " ",
            text,
            flags=re.IGNORECASE | re.DOTALL,
        )
        prose_text = unescape(re.sub(r"<[^>]+>", " ", prose_html))
        prose_text = re.sub(r"\s+", " ", prose_text)
        require(not re.search(r"(?<!\d)\[\d{1,3}\](?!\d)", prose_text), "numeric citation remains", failures)
        normalized_refs = [re.sub(r"\s+", " ", item).strip() for item in probe.reference_texts]
        require(
            normalized_refs == sorted(normalized_refs, key=lambda value: value.casefold()),
            "references are not alphabetized",
            failures,
        )
        semantic_references = [item for item in probe.ids if item.startswith("ref-")]
        require(
            all(not re.fullmatch(r"ref-\d+", item) for item in semantic_references),
            "numeric reference ID remains",
            failures,
        )

    if args.expected_figures is not None:
        require(
            len([item for item in probe.ids if item.startswith("fig-") and item not in args.retired_id]) == args.expected_figures,
            "semantic figure IDs do not match figure count",
            failures,
        )
    if args.expected_tables is not None:
        require(
            len([item for item in probe.ids if item.startswith("tab-") and item not in args.retired_id]) == args.expected_tables,
            "semantic table IDs do not match table count",
            failures,
        )

    if args.expected_figure_sha256:
        require(
            probe.figure_hashes == args.expected_figure_sha256,
            "embedded figure hashes differ from baseline",
            failures,
        )

    if args.baseline_html:
        baseline = ReportProbe()
        baseline.feed(args.baseline_html.read_text(encoding="utf-8"))
        require(
            normalized_artifacts(probe.table_cells) == normalized_artifacts(baseline.table_cells),
            "table cells differ from baseline",
            failures,
        )
        require(
            probe.figure_hashes == baseline.figure_hashes,
            "figure bytes differ from baseline",
            failures,
        )
        exempt = set(args.allow_code_block_change)
        current_code = normalized_artifacts(probe.code_blocks)
        baseline_code = normalized_artifacts(baseline.code_blocks)
        require(len(current_code) == len(baseline_code), "code block count differs from baseline", failures)
        for index, (current, previous) in enumerate(zip(current_code, baseline_code), start=1):
            if index not in exempt:
                require(current == previous, f"code block {index} differs from baseline", failures)
        baseline_external = Counter(baseline.external_links)
        current_external = Counter(probe.external_links)
        require(
            not (baseline_external - current_external),
            "external links from baseline are missing",
            failures,
        )

    print(f"html_sha256={hashlib.sha256(data).hexdigest()}")
    print(f"tables={probe.tags['table']}")
    print(f"figures={len(probe.figure_hashes)}")
    print(f"code_blocks={probe.tags['pre']}")
    print(f"references={len(probe.reference_texts)}")
    print(f"internal_links={len(probe.internal_links)}")
    print(f"external_links={len(probe.external_links)}")
    if args.baseline_html:
        print("baseline_preservation=PASS" if not failures else "baseline_preservation=FAIL")
    if failures:
        for failure in failures:
            print(f"FAIL: {failure}", file=sys.stderr)
        return 1
    print("static_validation=PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
