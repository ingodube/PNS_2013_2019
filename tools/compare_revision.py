"""Compare schema-1 results with the corrected release without treating them as targets."""
from pathlib import Path
import io,subprocess
import pandas as pd
ROOT=Path(__file__).resolve().parents[1]
BASE='90c9049'
SPECS=[
 ('asthma_diagnosis','df_prev_asma_2013_2019_long.csv','prevalencia',1,'Adult domain; explicit valid answers'),
 ('asthma_attack','df_prev_crise_2013_2019_long.csv','prevalencia',1,'Adult domain; diagnosis and valid attack answer'),
 ('asthma_medication_conditional','df_medic_asma_2013_2019_long.csv','pct_medic_asma',1,'Changed estimand: edition-specific eligibility and reference period; not an access trend'),
 ('current_smoking','df_taxa_fumantes_2013_2019_long.csv','taxa_100k_fumantes',.001,'Adult domain; old rounded per-100000 rate converted to percent')]
def main():
    new=pd.read_csv(ROOT/'data/estimates.csv'); rows=[]
    for indicator,name,col,scale,reason in SPECS:
        raw=subprocess.check_output(['git','show',BASE+':Spreadsheets (PT-BR)/'+name],cwd=ROOT)
        old=pd.read_csv(io.BytesIO(raw))
        for _,r in old.iterrows():
            candidates=new[(new.indicator==indicator)&(new.year==r.ano)]
            localized=pd.read_csv(ROOT/'Spreadsheets (PT-BR)'/({'asthma_diagnosis':'asthma','asthma_attack':'attacks','asthma_medication_conditional':'medication','current_smoking':'smoking'}[indicator]+'.csv'))
            match=localized[(localized.year==r.ano)&(localized.UF==r.uf)]
            assert len(match)==1
            corrected=float(match.iloc[0].estimate)*100
            rows.append(dict(indicator=indicator,year=r.ano,UF=r.uf,previous_percent=r[col]*scale,corrected_percent=corrected,difference_pp=corrected-r[col]*scale,interpretation=reason))
    pd.DataFrame(rows).to_csv(ROOT/'data/revision_comparison.csv',index=False)
    # Income also changes the rules for missing components and the price base.
    raw=subprocess.check_output(['git','show',BASE+':Spreadsheets (PT-BR)/df_renda_2019.xlsx'],cwd=ROOT)
    old=pd.read_excel(io.BytesIO(raw)); old.to_csv(ROOT/'data/income_previous_for_audit.csv',index=False)
    print('Revision comparisons saved; they are audit records, not validation targets')
if __name__=='__main__': main()
