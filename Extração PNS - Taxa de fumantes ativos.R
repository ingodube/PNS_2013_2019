# Carregando pacotes necessários
library(dplyr)
library(tidyr)
library(PNSIBGE)
library(ggplot2)
library(survey)
library(writexl)

options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

cria_design_pns = function(data_pns){
  data_design = data_pns %>%
    select(-any_of(c("V0028", "V00281", "V00282", "V00283",
                     "V0030", "V00301", "V00302", "V00303")))
  
  pns_design(data_pns = data_design)
}

estima_taxa_100k_uf = function(var, denom_var, design){
  formula_var = as.formula(paste0("~", var))
  formula_denom = as.formula(paste0("~", denom_var))
  
  estima_uma = function(design_atual, uf_atual){
    total = svytotal(formula_var, design = design_atual, na.rm = TRUE)
    total_pop = svytotal(formula_denom, design = design_atual, na.rm = TRUE)
    ci_total = as.numeric(confint(total))
    taxa = tryCatch(
      svyciprop(formula_var, design = design_atual,
                method = "beta", na.rm = TRUE),
      error = function(e) NULL
    )
    
    if (is.null(taxa)) {
      valor_taxa = NA_real_
      ci_taxa = c(NA_real_, NA_real_)
      metodo_ic = "indisponivel"
    } else {
      ci_taxa = tryCatch(as.numeric(confint(taxa)), error = function(e) c(NA_real_, NA_real_))
      valor_taxa = as.numeric(coef(taxa))[1]
      metodo_ic = ifelse(!is.finite(valor_taxa) || length(ci_taxa) < 2 || any(!is.finite(ci_taxa)),
                         "indisponivel", "beta")
      if (metodo_ic == "indisponivel") {
        valor_taxa = NA_real_
        ci_taxa = c(NA_real_, NA_real_)
      }
    }
    
    data.frame(
      uf = uf_atual,
      total_fumantes = as.numeric(coef(total))[1],
      lim_inf_total_fumantes = ci_total[1],
      lim_sup_total_fumantes = ci_total[2],
      total_pop = as.numeric(coef(total_pop))[1],
      taxa_100k_fumantes = round(valor_taxa * 100000),
      lim_inf_taxa_100k_fumantes = round(ci_taxa[1] * 100000),
      lim_sup_taxa_100k_fumantes = round(ci_taxa[2] * 100000),
      metodo_ic = metodo_ic
    )
  }
  
  ufs = sort(unique(design$variables$V0001))
  
  do.call(
    rbind,
    lapply(ufs, function(uf_atual){
      estima_uma(subset(design, V0001 == uf_atual), uf_atual)
    })
  )
}

variaveis_2019 = c("V0001", "V0024", "UPA_PNS", "ID_DOMICILIO", "V0006_PNS",
                   "V0025A", "V0025B", "P050",
                   "V0028", "V0029", "V0030", "V00281", "V00282", "V00291",
                   "V00292", "V00283", "V00293", "V00301", "V00302", "V00303")

pns2019 = get_pns(year = 2019, vars = variaveis_2019,
                   design = FALSE, labels = TRUE, selected = TRUE,
                   anthropometry = FALSE)


variaveis_2013 = c("V0001", "V0024", "UPA_PNS", "ID_DOMICILIO", "V0006_PNS",
                   "V0025", "P050", "V0028", "V0029", "V00281", "V00282", "V00291",
                   "V00292", "V00283", "V00293")

pns2013 = get_pns(year = 2013, vars = variaveis_2013,
                  design = FALSE, labels = TRUE, selected = TRUE,
                  anthropometry = FALSE)

# Tratando os dados
pns2013 = pns2013 %>%
  mutate(
    fumante = case_when(
      P050 %in% c("Sim, diariamente", "Sim, menos que diariamente") ~ 1,
      P050 == "Não fumo atualmente" ~ 0,
      TRUE ~ NA_real_ # preserva NA
    ),
    fumante_valid = ifelse(is.na(fumante), NA_real_, 1)
  )

pns2019 = pns2019 %>%
  mutate(
    fumante = case_when(
      P050 %in% c("Sim, diariamente", "Sim, menos que diariamente") ~ 1,
      P050 == "Não fumo atualmente" ~ 0,
      TRUE ~ NA_real_ # preserva NA
    ),
    fumante_valid = ifelse(is.na(fumante), NA_real_, 1)
  )

# Definindo o desenho amostral para MORADOR SELECIONADO
design_pns2013 = cria_design_pns(pns2013)
design_pns2019 = cria_design_pns(pns2019)

# Calculando taxa de fumantes por UF
df_taxa_fumantes_2013 = estima_taxa_100k_uf("fumante", "fumante_valid", design_pns2013)
df_taxa_fumantes_2013$ano = rep(2013, nrow(df_taxa_fumantes_2013))

df_taxa_fumantes_2019 = estima_taxa_100k_uf("fumante", "fumante_valid", design_pns2019)
df_taxa_fumantes_2019$ano = rep(2019, nrow(df_taxa_fumantes_2019))

# Juntando as tabelas
df_taxa_fumantes_2013_2019 = rbind(df_taxa_fumantes_2013, df_taxa_fumantes_2019)

# Salvando a base no formato long
write.csv(df_taxa_fumantes_2013_2019, file = "df_taxa_fumantes_2013_2019_long.csv", row.names = FALSE)


# Transformando a população projetada em formato wide
df_população_2013_2019_wide = df_taxa_fumantes_2013_2019 %>%
  select(uf, ano, total_pop) %>%
  pivot_wider(
    names_from = ano,
    values_from = total_pop,
    names_prefix = "total_pop_"
  )

# Salvando a base no formato wide
write_xlsx(df_população_2013_2019_wide, path = "df_população_2013_2019_wide.xlsx")

# Transformando no formato wide
df_taxa_fumantes_2013_2019_wide = df_taxa_fumantes_2013_2019 %>%
  select(uf, ano, taxa_100k_fumantes) %>%
  pivot_wider(
    names_from = ano,
    values_from = taxa_100k_fumantes,
    names_prefix = "taxa_100k_fumantes_"
  )

# Salvando a base no formato wide
write_xlsx(df_taxa_fumantes_2013_2019_wide, path = "df_taxa_fumantes_2013_2019_wide.xlsx")

# Corrigindo a base de dados
df_plot = df_taxa_fumantes_2013_2019 %>%
  mutate(ano = factor(ano, levels = c(2019, 2013))) # garante que 2019 fica embaixo

# Criando gráfico
ggplot(df_plot, aes(x = reorder(uf, taxa_100k_fumantes), 
                    y = taxa_100k_fumantes, 
                    fill = ano)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.8) + # barras mais finas e separadas
  geom_errorbar(aes(ymin = lim_inf_taxa_100k_fumantes,
                    ymax = lim_sup_taxa_100k_fumantes),
                position = position_dodge(width = 0.9), 
                width = 0.3) +
  coord_flip() +
  labs(x = NULL,
       y = "Taxa de fumantes por 100 mil habitantes",
       fill = NULL) +
  scale_fill_manual(values = c("2013" = "#a7a6f2", "2019" = "#ead4a4"),
                    breaks = c("2013", "2019")) + 
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size = 11))
