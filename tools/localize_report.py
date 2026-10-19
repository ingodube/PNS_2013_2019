#!/usr/bin/env python3
"""Translate reviewed prose blocks; never translate analytical code or data paths."""
from pathlib import Path
import json,re
ROOT=Path(__file__).resolve().parents[1]
def main():
    source=(ROOT/'docs/methodology_report_pt_br.Rmd').read_text(encoding='utf-8')
    translations=json.loads((ROOT/'docs/report-translations.json').read_text(encoding='utf-8'))
    metadata=source.split('---',2)[1]
    if metadata != translations['metadata']['pt']: raise ValueError('Metadata changed: review its English translation')
    seen=set()
    def replace(match):
        key,content=match.groups()
        if key in seen: raise ValueError('Duplicate prose key: '+key)
        seen.add(key)
        if key not in translations or content != translations[key]['pt']: raise ValueError('Unreviewed prose: '+key)
        return translations[key]['en']
    body=re.sub(r'<!-- i18n:([a-z-]+) -->\n(.*?)\n<!-- /i18n -->',replace,source.split('---',2)[2],flags=re.S)
    if seen != set(translations)-{'metadata'}: raise ValueError('Missing or obsolete translations')
    target='---'+translations['metadata']['en']+'---'+body
    chunks=lambda s: re.findall(r'```\{r.*?```',s,re.S)
    if chunks(source)!=chunks(target): raise ValueError('Scientific code changed during translation')
    (ROOT/'docs/methodology_report_en.Rmd').write_text(target,encoding='utf-8')
    print(f'English source: {len(seen)} reviewed blocks; R chunks unchanged')
if __name__=='__main__': main()
