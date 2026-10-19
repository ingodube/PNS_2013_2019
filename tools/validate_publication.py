"""Static publication gate, using the reviewed structure rather than legacy counts."""
from pathlib import Path
from html.parser import HTMLParser
import json,subprocess,sys
ROOT=Path(__file__).resolve().parents[1]
class Head(HTMLParser):
    def __init__(self): super().__init__(); self.links=[]
    def handle_starttag(self,tag,attrs):
        if tag=='link': self.links.append(dict(attrs))
def main():
    c=json.loads((ROOT/'docs/report-structure.json').read_text())
    for file,language in [('metodologia.html','en'),('methodology-pt-br.html','pt-BR')]:
        args=[sys.executable,str(ROOT/'.codex/skills/methodology-rmarkdown/scripts/validate_report.py'),str(ROOT/'docs'/file),'--expected-lang',language,'--author-date','--expected-authors','2']
        for flag,key in [('tables','tables'),('figures','figures'),('references','references'),('math','math'),('code-blocks','code_blocks')]: args+=['--expected-'+flag,str(c[key])]
        for retired in c['retired_ids']: args+=['--retired-id',retired]
        subprocess.run(args,cwd=ROOT,check=True)
        head=Head(); head.feed((ROOT/'docs'/file).read_text(encoding='utf-8'))
        canonical=[x['href'] for x in head.links if x.get('rel')=='canonical']
        assert canonical==['https://ingodube.github.io/PNS_2013_2019/'+file]
        alternate={x.get('hreflang'):x['href'] for x in head.links if x.get('rel')=='alternate'}
        assert alternate['en'].endswith('/metodologia.html') and alternate['pt-BR'].endswith('/methodology-pt-br.html')
        assert all('<' not in x.get('href','') for x in head.links)
    subprocess.run([sys.executable,str(ROOT/'tools/validate_bilingual_outputs.py')],cwd=ROOT,check=True)
    print('Static publication gate PASS')
if __name__=='__main__': main()
