#!/usr/bin/env python3
"""Generate language-specific entry points; scientific code is never translated."""
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
ENTRIES = [('Extração PNS - Asma.R', 'PNS Extraction - Asthma.R', 'asthma'), ('Extração PNS - Crise Asma 12 Meses.R', 'PNS Extraction - Asthma Attacks in the Last 12 Months.R', 'attacks'), ('Extração PNS - Dias Perdidos.R', 'PNS Extraction - Lost Days.R', 'activity'), ('Extração PNS - Farmácia Popular.R', 'PNS Extraction - Farmacia Popular.R', 'payment'), ('Extração PNS - Medicamentos Asma.R', 'PNS Extraction - Asthma Medication.R', 'medication'), ('Extração PNS - Renda.R', 'PNS Extraction - Income.R', 'income'), ('Extração PNS - Taxa de fumantes ativos.R', 'PNS Extraction - Active Smoker Rate.R', 'smoking')]
TEMPLATE = '''# Generated entry point. Definitions and estimators are shared across languages.
roots <- c(Sys.getenv("PNS_REPO_ROOT"), getwd(), dirname(getwd()))
root <- roots[file.exists(file.path(roots, "R", "pns_core.R"))][1]
if (is.na(root)) stop("Run from repository root or set PNS_REPO_ROOT")
source(file.path(root, "R", "pns_core.R"), encoding="UTF-8")
run_group("{group}", "{locale}")
'''
if __name__ == "__main__":
    for pt, en, group in ENTRIES:
        for directory, name, locale in [("Codes (PT-BR)",pt,"pt"),("Codes (EN)",en,"en")]:
            (ROOT/directory/name).write_text(TEMPLATE.format(group=group,locale=locale),encoding="utf-8")
    print("14 entry points generated; no analytical text substitution")
