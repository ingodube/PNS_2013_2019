"""Validate and promote a complete staged numerical release (not the HTML pages)."""
from pathlib import Path
import argparse,hashlib,json,shutil,subprocess
import numpy as np
import pandas as pd
ROOT=Path(__file__).resolve().parents[1]
GROUPS=['asthma','attacks','activity','payment','medication','income','smoking']
def validate(stage):
    manifest=json.loads((stage/'analysis_manifest.json').read_text())
    assert manifest['schema_version']==2
    sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
    assert sha(ROOT/'R/pns_core.R')==manifest['analysis_sha256']
    for name,expected in manifest['output_sha256'].items(): assert sha(stage/name)==expected,name
    x=pd.read_csv(stage/'estimates.csv')
    assert not x.duplicated(['indicator','year','geo']).any()
    assert (x.n_eligible==x.n_valid+x.n_missing).all()
    assert (x.n_valid>=0).all() and (x.n_valid<=x.n_eligible).all()
    assert np.allclose(x.weighted_valid+x.weighted_missing,x.weighted_eligible)
    p=x[x.kind=='proportion'].dropna(subset=['estimate'])
    assert p.estimate.between(0,1).all()
    ok=x[x.status=='ok']; assert ok[['estimate','se','lower','upper']].notna().all().all()
    assert (ok.lower<=ok.estimate).all() and (ok.upper>=ok.estimate).all()
    for group in GROUPS:
        frames=[]
        for lang in ['pt','en']:
            z=pd.read_csv(stage/lang/(group+'.csv')); w=pd.read_excel(stage/lang/(group+'.xlsx'))
            pd.testing.assert_frame_equal(z,w,check_dtype=False,check_exact=False,rtol=1e-12,atol=1e-12)
            z['UF']=z.UF.replace({'Brazil':'Brasil'}); frames.append(z)
            target=x[x.indicator.isin(z.indicator.unique())].reset_index(drop=True)
            pd.testing.assert_frame_equal(z.drop(columns='UF').reset_index(drop=True),target,check_dtype=False,check_exact=False,rtol=1e-12,atol=1e-12)
        pd.testing.assert_frame_equal(*frames,check_dtype=False)
    return manifest
def main():
    p=argparse.ArgumentParser(); p.add_argument('stage',type=Path); p.add_argument('--check-only',action='store_true'); args=p.parse_args()
    stage=args.stage.resolve(); validate(stage)
    if args.check_only: print('Staged numerical release PASS'); return
    # Remove only tracked legacy analytical exports, never untracked user files.
    tracked=subprocess.check_output(['git','ls-files','-z','Spreadsheets (EN)','Spreadsheets (PT-BR)'],cwd=ROOT).decode().split('\0')
    retired=[]
    for name in filter(None,tracked):
        path=ROOT/name
        if path.parent.name not in ['Spreadsheets (EN)','Spreadsheets (PT-BR)'] or path.suffix not in ['.csv','.xlsx']: continue
        if path.stem in GROUPS: continue
        retired.append(name); path.unlink()
    for lang,directory in [('pt','Spreadsheets (PT-BR)'),('en','Spreadsheets (EN)')]:
        for group in GROUPS:
            for ext in ['csv','xlsx']: shutil.copy2(stage/lang/(group+'.'+ext),ROOT/directory/(group+'.'+ext))
    for name in ['estimates.csv','contrasts.csv','analysis_manifest.json','sessionInfo.txt']: shutil.copy2(stage/name,ROOT/'data'/name)
    if retired: (ROOT/'data/retired_outputs.json').write_text(json.dumps({'schema_version':1,'replacement_schema':2,'files':retired,'reason':'Corrected definitions and explicit denominators; original outputs remain in Git history at 90c9049.'},ensure_ascii=False,indent=2),encoding='utf-8')
    print('Complete numerical release promoted; HTML must be rendered and validated before publication')
if __name__=='__main__': main()
