#!/usr/bin/env python3
"""Validate scientific parity between the EN and PT-BR publication outputs."""

from __future__ import annotations

import argparse
import csv
from decimal import Decimal, InvalidOperation
import hashlib
from html.parser import HTMLParser
from pathlib import Path
import re
import sys


UF_NAMES = {
    "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal",
    "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul",
    "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí",
    "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima",
    "Santa Catarina", "São Paulo", "Sergipe", "Tocantins",
}

CSV_PAIRS = {
    "df_medic_asma_2013_2019_long.csv": "asthma_medication_use_2013_2019_long.csv",
    "df_medic_crise_asma_2013_2019_long.csv": "recent_asthma_attack_medication_use_2013_2019_long.csv",
    "df_prev_asma_2013_2019_long.csv": "asthma_diagnosis_prevalence_2013_2019_long.csv",
    "df_prev_crise_2013_2019_long.csv": "asthma_attack_prevalence_2013_2019_long.csv",
    "df_taxa_fumantes_2013_2019_long.csv": "active_smoker_rate_2013_2019_long.csv",
    "df_taxa_medic_asma_2013_2019_long.csv": "asthma_medication_population_rate_2013_2019_long.csv",
}


class Probe(HTMLParser):
    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.ids: list[str] = []
        self.links: list[str] = []
        self.figure_hashes: list[str] = []
        self.table_text: list[str] = []
        self.visible: list[str] = []
        self.tags: dict[str, int] = {}
        self._hidden = 0
        self._cell = 0

    def handle_starttag(self, tag: str, attrs) -> None:
        tag = tag.lower()
        attributes = dict(attrs)
        self.tags[tag] = self.tags.get(tag, 0) + 1
        if attributes.get("id"):
            self.ids.append(attributes["id"])
        if tag == "a" and attributes.get("href"):
            self.links.append(attributes["href"])
        if tag in {"script", "style"}:
            self._hidden += 1
        if tag in {"th", "td"}:
            self._cell += 1
            self.table_text.append("")
        source = attributes.get("src", "")
        if tag == "img" and source.startswith("data:image"):
            import base64
            self.figure_hashes.append(hashlib.sha256(base64.b64decode(source.split(",", 1)[1])).hexdigest())

    def handle_endtag(self, tag: str) -> None:
        tag = tag.lower()
        if tag in {"script", "style"} and self._hidden:
            self._hidden -= 1
        if tag in {"th", "td"} and self._cell:
            self._cell -= 1

    def handle_data(self, data: str) -> None:
        if self._hidden:
            return
        if self._cell:
            self.table_text[-1] += data
        text = " ".join(data.split())
        if text:
            self.visible.append(text)


def number_tokens(values: list[str], locale: str) -> list[Decimal]:
    tokens: list[Decimal] = []
    for value in values:
        for token in re.findall(r"(?<![\w])[-−]?\d[\d.,]*(?![\w])", value):
            raw = token.replace("−", "-")
            if locale == "pt-BR":
                raw = raw.replace(".", "").replace(",", ".")
            else:
                raw = raw.replace(",", "")
            try:
                tokens.append(Decimal(raw))
            except InvalidOperation:
                raise AssertionError(f"Could not parse numeric token {token!r} for {locale}")
    return tokens


def read_csv(path: Path) -> list[list[str]]:
    with path.open("r", encoding="utf-8-sig", newline="") as stream:
        return list(csv.reader(stream))


def csv_numbers(rows: list[list[str]]) -> list[Decimal]:
    values: list[Decimal] = []
    for row in rows[1:]:
        for cell in row:
            try:
                values.append(Decimal(cell))
            except InvalidOperation:
                continue
    return values


def parse(path: Path) -> Probe:
    probe = Probe()
    probe.feed(path.read_text(encoding="utf-8"))
    return probe


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo", type=Path, default=Path(__file__).resolve().parents[1])
    args = parser.parse_args()
    root = args.repo.resolve()
    en = parse(root / "docs" / "metodologia.html")
    pt = parse(root / "docs" / "methodology-pt-br.html")
    failures: list[str] = []

    def require(condition: bool, message: str) -> None:
        if not condition:
            failures.append(message)

    required_ids = lambda probe: {item for item in probe.ids if item == "TOC" or item.startswith(("fig-", "tab-", "ref-"))}
    require(required_ids(en) == required_ids(pt), "semantic IDs differ between languages")
    require(len(set(en.figure_hashes)) == len(set(pt.figure_hashes)) == 10, "figure sets are incomplete")
    require(number_tokens(en.table_text, "en") == number_tokens(pt.table_text, "pt-BR"), "table numeric values differ")
    require(en.tags.get("table", 0) == pt.tags.get("table", 0) == 13, "expected 13 tables in each report")
    require(len(en.figure_hashes) == len(pt.figure_hashes) == 10, "expected 10 figures in each report")
    require(en.tags.get("pre", 0) == pt.tags.get("pre", 0) == 4, "expected four code blocks in each report")
    require(en.tags.get("math", 0) == pt.tags.get("math", 0) == 9, "expected nine mathematical expressions in each report")
    require("translation" in " ".join(en.visible).casefold(), "English AI disclosure does not mention translation")
    require("tradução" in " ".join(pt.visible).casefold(), "PT-BR AI disclosure does not mention translation")
    require(
        not any("Brasil" in cell for cell in en.table_text) and any("Brazil" in cell for cell in en.table_text),
        "English tables must use Brazil, not Brasil",
    )
    for name in UF_NAMES:
        require(any(name in cell for cell in en.table_text), f"federative-unit name missing from English tables: {name}")

    ignored_links = {"metodologia.html", "methodology-pt-br.html"}
    en_links = {link for link in en.links if not link.startswith("#") and link not in ignored_links}
    pt_links = {link for link in pt.links if not link.startswith("#") and link not in ignored_links}
    require(en_links == pt_links, "external links differ between languages")

    pt_dir = root / "Processed Tables (PT-BR)"
    en_dir = root / "Processed Tables (EN)"
    for pt_name, en_name in CSV_PAIRS.items():
        pt_rows = read_csv(pt_dir / pt_name)
        en_rows = read_csv(en_dir / en_name)
        require(len(pt_rows) == len(en_rows), f"CSV row count differs: {pt_name}")
        require(all(len(a) == len(b) for a, b in zip(pt_rows, en_rows)), f"CSV column count differs: {pt_name}")
        require(csv_numbers(pt_rows) == csv_numbers(en_rows), f"CSV numeric values differ: {pt_name}")
        pt_ufs = [cell for row in pt_rows for cell in row if cell in UF_NAMES]
        en_ufs = [cell for row in en_rows for cell in row if cell in UF_NAMES]
        require(pt_ufs == en_ufs, f"federative-unit values differ: {pt_name}")
        require(sum(cell == "Brasil" for row in pt_rows for cell in row) ==
                sum(cell == "Brazil" for row in en_rows for cell in row), f"Brazil localization differs: {pt_name}")

    print(f"semantic_ids={len(required_ids(en))}")
    print(f"table_numeric_tokens={len(number_tokens(en.table_text, 'en'))}")
    print(f"figure_hashes={len(en.figure_hashes)}")
    print(f"csv_pairs={len(CSV_PAIRS)}")
    if failures:
        for failure in failures:
            print(f"FAIL: {failure}", file=sys.stderr)
        return 1
    print("bilingual_parity=PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
