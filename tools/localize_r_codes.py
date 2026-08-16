#!/usr/bin/env python3
"""Create the reviewed English copies of the Portuguese PNS R programs."""

from __future__ import annotations

from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "Codes (PT-BR)"
TARGET = ROOT / "Codes (EN)"

FILE_NAMES = {
    "Extração PNS - Asma.R": "PNS Extraction - Asthma.R",
    "Extração PNS - Crise Asma 12 Meses.R": "PNS Extraction - Asthma Attacks in the Last 12 Months.R",
    "Extração PNS - Dias Perdidos.R": "PNS Extraction - Lost Days.R",
    "Extração PNS - Farmácia Popular.R": "PNS Extraction - Farmacia Popular.R",
    "Extração PNS - Medicamentos Asma.R": "PNS Extraction - Asthma Medication.R",
    "Extração PNS - Renda.R": "PNS Extraction - Income.R",
    "Extração PNS - Taxa de fumantes ativos.R": "PNS Extraction - Active Smoker Rate.R",
}

OUTPUT_FILE_NAMES = {
    "df_absenteismo.xlsx": "health_absenteeism.xlsx",
    "df_absenteismo_respiratorio_2013_2019.xlsx": "harmonized_respiratory_absenteeism_2013_2019.xlsx",
    "df_desembolso.xlsx": "asthma_medication_out_of_pocket.xlsx",
    "df_dias_perdidos.xlsx": "lost_days.xlsx",
    "df_dias_perdidos_respiratorio_2013_2019.xlsx": "harmonized_respiratory_lost_days_2013_2019.xlsx",
    "df_medic_asma_2013_2019_long.csv": "asthma_medication_use_2013_2019_long.csv",
    "df_medic_asma_2013_2019_wide.xlsx": "asthma_medication_use_2013_2019_wide.xlsx",
    "df_medic_crise_asma_2013_2019_long.csv": "recent_asthma_attack_medication_use_2013_2019_long.csv",
    "df_medic_crise_asma_2013_2019_wide.xlsx": "recent_asthma_attack_medication_use_2013_2019_wide.xlsx",
    "df_população_2013_2019_wide.xlsx": "population_2013_2019_wide.xlsx",
    "df_prev_asma_2013_2019_long.csv": "asthma_diagnosis_prevalence_2013_2019_long.csv",
    "df_prev_asma_2013_2019_wide.xlsx": "asthma_diagnosis_prevalence_2013_2019_wide.xlsx",
    "df_prev_crise_2013_2019_long.csv": "asthma_attack_prevalence_2013_2019_long.csv",
    "df_prev_crise_2013_2019_wide.xlsx": "asthma_attack_prevalence_2013_2019_wide.xlsx",
    "df_renda_2019.xlsx": "income_2019.xlsx",
    "df_taxa_fumantes_2013_2019_long.csv": "active_smoker_rate_2013_2019_long.csv",
    "df_taxa_fumantes_2013_2019_wide.xlsx": "active_smoker_rate_2013_2019_wide.xlsx",
    "df_taxa_medic_asma_2013_2019_long.csv": "asthma_medication_population_rate_2013_2019_long.csv",
    "df_taxa_medic_asma_2013_2019_wide.xlsx": "asthma_medication_population_rate_2013_2019_wide.xlsx",
}

REPLACEMENTS = {
    'dir.exists(file.path(candidate_roots, "Codes (PT-BR)"))': 'dir.exists(file.path(candidate_roots, "Codes (EN)"))',
    'file.path(repo_root, "Processed Tables (PT-BR)")': 'file.path(repo_root, "Processed Tables (EN)")',
    'stop("Não foi possível localizar a raiz do repositório.")': 'stop("The repository root could not be located.")',
    "# Carregando pacotes necessários": "# Load required packages",
    "# Tratando os dados": "# Prepare the data",
    "# Limpando as bases de dados": "# Clean the data sets",
    "# Definindo o desenho amostral para MORADOR SELECIONADO": "# Define the survey design for the SELECTED RESIDENT",
    "# Corrigindo a base de dados": "# Prepare the plotting data",
    "# Criando gráfico": "# Create the chart",
    "# Gerando o gráfico": "# Create the chart",
    "# Juntando as tabelas": "# Combine the tables",
    "# Juntando crises": "# Combine asthma-attack estimates",
    "# Juntando tudo": "# Combine the results",
    "# Salvando a base no formato long": "# Save the data in long format",
    "# Salvando a base no formato wide": "# Save the data in wide format",
    "# Salvando a base de dados": "# Save the data set",
    "# Transformando no formato wide": "# Reshape to wide format",
    "# Transformando a população projetada em formato wide": "# Reshape the projected population to wide format",
    "# Prevalência (%) por UF": "# Prevalence (%) by federative unit",
    "# Prevalência de crises de asma nos últimos 12 meses": "# Prevalence of asthma attacks in the last 12 months",
    "# Porcentagem de pessoas com asma que usam medicamentos atualmente": "# Percentage of people with asthma who currently use medication",
    "# Porcentagem de asmáticos que tiveram crises recentes e que consomem medicamentos": "# Percentage of people with recent asthma attacks who use medication",
    "# Total de usuários de medicamentos orais ou bombinhas": "# Total users of oral medication or inhalers",
    "# Calculando taxa de fumantes por UF": "# Calculate active-smoker rates by federative unit",
    "# Criando a variável renda declarada": "# Create the reported-income variable",
    "# Deflacionando os salários": "# Deflate income values",
    "# Desenho amostral PNS 2019": "# PNS 2019 survey design",
    "# Calculando a estimativa de renda média por UF e Brasil": "# Estimate mean income by federative unit and Brazil",
    "# Estimação do percentual de usuários de medicamentos orais que fizeram o desembolso": "# Estimate the percentage of oral-medication users who paid out of pocket",
    "# Estimando o total e a taxa por 100 mil habitantes de pessoas que fizeram o desembolso de medicamentos orais ou bombinhas": "# Estimate the total and rate per 100,000 residents who paid out of pocket for oral medication or inhalers",
    "# Brasil por último": "# Brazil last",
    '"Brasil"': '"Brazil"',
    '"indisponivel"': '"unavailable"',
    '"Geral"': '"Overall"',
    '"Morador selecionado"': '"Selected resident"',
    '"Moradores que deixaram de realizar atividades habituais por motivo de saúde"': '"Residents who stopped performing usual activities for health reasons"',
    '"Moradores cujo principal motivo de perda de atividades foi respiratório harmonizado"': '"Residents whose main reason for activity loss was a harmonized respiratory condition"',
    '"Respiratório harmonizado"': '"Harmonized respiratory condition"',
    '"Medicamento oral"': '"Oral medication"',
    '"Bombinha"': '"Inhaler"',
    '"Problemas nos ossos e articulações"': '"Bone and joint problems"',
    '"Dor nos ossos e articulações"': '"Bone and joint pain"',
    '"Dor de cabeça ou enxaqueca"': '"Headache or migraine"',
    '"Enxaqueca"': '"Migraine"',
    '"Problemas gineco-obstétricos"': '"Gynecologic and obstetric problems"',
    '"Gineco-obstétrico"': '"Gynecologic and obstetric"',
    '"Problema odontológico"': '"Dental problem"',
    '"Dor de dente"': '"Toothache"',
    '"Problemas respiratórios"': '"Respiratory problems"',
    '"Respiratório (asma/bronquite/pneumonia)"': '"Respiratory (asthma/bronchitis/pneumonia)"',
    '"Problemas gastrointestinais"': '"Gastrointestinal problems"',
    '"Gastrointestinal"': '"Gastrointestinal"',
    '"Problemas cardiovasculares"': '"Cardiovascular problems"',
    '"Cardiovascular"': '"Cardiovascular"',
    '"Câncer"': '"Cancer"',
    '"Problemas neurológicos"': '"Neurological problems"',
    '"Neurológico"': '"Neurological"',
    '"Saúde mental"': '"Mental health"',
    '"Lesões ou fraturas"': '"Injuries or fractures"',
    '"Fratura/violência"': '"Fracture/violence"',
    '"Outro problema de saúde"': '"Other health problem"',
    '"Não declarado"': '"Not reported"',
    '"Dengue/Chikungunya/Zika/Febre amarela"': '"Dengue/Chikungunya/Zika/Yellow fever"',
    '"Taxa de fumantes por 100 mil habitantes"': '"Active smokers per 100,000 residents"',
    '"Prevalência (%) de diagnóstico médico de asma"': '"Prevalence (%) of a medical asthma diagnosis"',
    '"Prevalência (%) de crises asmáticas nos últimos 12 meses."': '"Prevalence (%) of asthma attacks in the last 12 months"',
    '"Renda média deflacionada (R$)"': '"Mean inflation-adjusted income (BRL)"',
    '"Média de dias perdidos"': '"Mean lost days"',
    '"(%)"': '"Percentage (%)"',
    '"(%) de pessoas com diagnóstico de asma e que \\n usam medicamentos orais ou bombinhas para tratamento de asma."': '"Percentage of people diagnosed with asthma who use oral medication or inhalers for asthma treatment"',
    '"(%) de pessoas que tiveram crise de asma nos últimos 12 meses e que \\n usam medicamentos orais ou bombinhas para tratamento de asma."': '"Percentage of people with an asthma attack in the last 12 months who use oral medication or inhalers for asthma treatment"',
    '"Taxa de usuários de medicamentos orais ou bombinhas para tratamento de asma \\n por 100 mil habitantes."': '"Users of oral medication or inhalers for asthma treatment per 100,000 residents"',
}


def translate(text: str) -> str:
    translated = text
    for source, target in REPLACEMENTS.items():
        translated = translated.replace(source, target)
    for source, target in OUTPUT_FILE_NAMES.items():
        translated = translated.replace(source, target)
    header = (
        "# English localization. PNS response labels such as Sim/Não remain in Portuguese "
        "because they are literal values in the source microdata.\n"
    )
    normalized = "\n".join(line.rstrip() for line in translated.splitlines()) + "\n"
    return header + normalized


def main() -> None:
    TARGET.mkdir(parents=True, exist_ok=True)
    for source_name, target_name in FILE_NAMES.items():
        text = (SOURCE / source_name).read_text(encoding="utf-8")
        (TARGET / target_name).write_text(translate(text), encoding="utf-8", newline="\n")
    print(f"Created {len(FILE_NAMES)} English R programs in {TARGET}")


if __name__ == "__main__":
    main()
