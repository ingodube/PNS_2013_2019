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
    import json
    import pandas as pd
    root=Path(__file__).resolve().parents[1]
    config=json.loads((root/'docs/report-structure.json').read_text())
    en=parse(root/'docs/metodologia.html'); pt=parse(root/'docs/methodology-pt-br.html')
    required=lambda p:{x for x in p.ids if x=='TOC' or x.startswith(('fig-','tab-','ref-'))}
    assert required(en)==required(pt), 'Semantic IDs differ'
    for label,probe in [('en',en),('pt',pt)]:
        assert probe.tags.get('table',0)==config['tables'], label+' table count'
        assert len(probe.figure_hashes)==config['figures'], label+' figure count'
        assert probe.tags.get('math',0)==config['math'], label+' math count'
        assert probe.tags.get('pre',0)==config['code_blocks'], label+' code count'
        assert len([i for i in probe.ids if i.startswith('ref-')])==config['references']
        assert set(config['retired_ids']).issubset(probe.ids)
    assert number_tokens(en.table_text,'en')==number_tokens(pt.table_text,'pt-BR'), 'HTML numeric parity'
    for uf in UF_NAMES: assert any(uf in c for c in en.table_text),uf
    assert 'translation' in ' '.join(en.visible).lower()
    assert 'tradução' in ' '.join(pt.visible).lower()
    external=lambda p:{a for a in p.links if not a.startswith('#') and a not in {'metodologia.html','methodology-pt-br.html'}}
    assert external(en)==external(pt),'External link parity'
    canonical=pd.read_csv(root/'data/estimates.csv')
    for group in ['asthma','attacks','activity','payment','medication','income','smoking']:
        tables=[]
        for locale in ['PT-BR','EN']:
            directory=root/('Spreadsheets ('+locale+')')
            a=pd.read_csv(directory/(group+'.csv')); b=pd.read_excel(directory/(group+'.xlsx'))
            pd.testing.assert_frame_equal(a,b,check_dtype=False,check_exact=False,rtol=1e-12,atol=1e-12)
            assert not a.duplicated(['indicator','year','geo']).any()
            expected=canonical[canonical.indicator.isin(a.indicator.unique())].reset_index(drop=True)
            pd.testing.assert_frame_equal(a.drop(columns='UF').reset_index(drop=True),expected,check_dtype=False,check_exact=False,rtol=1e-12,atol=1e-12)
            a['UF']=a.UF.replace({'Brazil':'Brasil'}); tables.append(a)
        pd.testing.assert_frame_equal(*tables,check_dtype=False)
    print('PASS: 14 CSV + 14 XLSX; canonical keys, denominators, missingness and values; HTML tables, figures, IDs and references')
    return 0
if __name__=='__main__': raise SystemExit(main())
