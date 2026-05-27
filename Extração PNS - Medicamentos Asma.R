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

estima_prop_beta = function(var, design, pct_col, value_col = var){
  formula_var = as.formula(paste0("~", var))
  
  estima_uma = function(design_atual, uf_atual){
    est = tryCatch(
      svyciprop(formula_var, design = design_atual,
                method = "beta", na.rm = TRUE),
      error = function(e) NULL
    )
    
    if (is.null(est)) {
      return(data.frame(
        uf = uf_atual,
        valor = NA_real_,
        ci_l = NA_real_,
        ci_u = NA_real_,
        pct = NA_real_,
        li = NA_real_,
        ls = NA_real_,
        metodo_ic = "indisponivel"
      ))
    }
    
    ci = tryCatch(as.numeric(confint(est)), error = function(e) c(NA_real_, NA_real_))
    valor = as.numeric(coef(est))[1]
    
    if (!is.finite(valor) || length(ci) < 2 || any(!is.finite(ci))) {
      return(data.frame(
        uf = uf_atual,
        valor = NA_real_,
        ci_l = NA_real_,
        ci_u = NA_real_,
        pct = NA_real_,
        li = NA_real_,
        ls = NA_real_,
        metodo_ic = "indisponivel"
      ))
    }
    
    data.frame(
      uf = uf_atual,
      valor = valor,
      ci_l = ci[1],
      ci_u = ci[2],
      pct = valor * 100,
      li = ci[1] * 100,
      ls = ci[2] * 100,
      metodo_ic = "beta"
    )
  }
  
  ufs = sort(unique(design$variables$V0001))
  
  est_uf = do.call(
    rbind,
    lapply(ufs, function(uf_atual){
      estima_uma(subset(design, V0001 == uf_atual), uf_atual)
    })
  )
  
  est_br = estima_uma(design, "Brasil")
  
  rbind(est_uf, est_br) %>%
    rename(!!value_col := valor,
           !!pct_col := pct)
}

estima_taxa_medic_100k = function(var, denom_var, design){
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
      total_medic_asma = as.numeric(coef(total))[1],
      lim_inf_total_medic_asma = ci_total[1],
      lim_sup_total_medic_asma = ci_total[2],
      total_pop = as.numeric(coef(total_pop))[1],
      taxa_100k_medic_asma = round(valor_taxa * 100000),
      lim_inf_taxa_100k_medic_asma = round(ci_taxa[1] * 100000),
      lim_sup_taxa_100k_medic_asma = round(ci_taxa[2] * 100000),
      metodo_ic = metodo_ic
    )
  }
  
  ufs = sort(unique(design$variables$V0001))
  
  est_uf = do.call(
    rbind,
    lapply(ufs, function(uf_atual){
      estima_uma(subset(design, V0001 == uf_atual), uf_atual)
    })
  )
  
  est_br = estima_uma(design, "Brasil")
  
  rbind(est_uf, est_br)
}

variaveis_2019 = c("V0001", "V0024", "UPA_PNS", "ID_DOMICILIO", "V0006_PNS",
                   "V0025A", "V0025B", "J00402", "J01101", "Q074", "Q075", "Q076", "Q07601",
                   "Q07704", "Q07705", "Q07706", "Q07707", "Q07708",
                   "Q07709", "Q07710", "Q07711",  "Q078", "H022", "H024", "H025",
                   "H026",  "P050",
                   "P051", "P052", "P053", "P05401", "P05402",
                   "P05403", "P05404", "P05405", "P05406",
                   "P05407", "P05408", "P05409", "P05410",
                   "P05411", "P05412", "P05413", "P05414",
                   "P05415", "P05416", "P05417", "P05418",
                   "P05419", "P05421", "P05422", "P056",
                   "P05601", "P05602", "P05603", "P05604", "P05605",
                   "P057", "P058", "P05801", "P05802",
                   "P05901", "P05902", "P05903", "P05904", "P05905", "P05906",
                   "P060", "P06101", "P06102", "P06103", "P06104",
                   "P06105", "P06106", "P06302", "P064",
                   "P067", "P06701", "P068", "Q010",
                   "V0028", "V0029", "V0030", "V00281", "V00282", "V00291",
                   "V00292", "V00283", "V00293", "V00301", "V00302", "V00303")

pns2019 = get_pns(year = 2019, vars = variaveis_2019,
                  design = FALSE, labels = TRUE, selected = TRUE,
                  anthropometry = FALSE)


variaveis_2013 = c("V0001", "V0024", "UPA_PNS", "ID_DOMICILIO", "V0006_PNS",
                   "V0025", "J004", "Q074", "Q075", "Q076",
                   "Q07701", "Q07702", "Q078", "P050",
                   "P051", "P052", "P053", "P05401", "P05402",
                   "P05403", "P05404", "P05405", "P05406",
                   "P05407", "P05408", "P05409", "P05410",
                   "P05411", "P05412", "P05413", "P05414",
                   "P05415", "P05416", "P05417", "P05418",
                   "P05419", "P05421", "P05422", "P056",
                   "P05601", "P05602", "P05603", "P05604",
                   "P057", "P058", "P05801", "P05802",
                   "P05901", "P05902", "P05903", "P05904",
                   "P060", "P061", "P062", "P063", "P064",
                   "P065", "P066", "P067", "P068", "P07001",
                   "P07002", "P07003", "P071", "P072", "Q06205",
                   "V0028", "V0029", "V00281", "V00282", "V00291",
                   "V00292", "V00283", "V00293")

pns2013 = get_pns(year = 2013, vars = variaveis_2013,
                  design = FALSE, labels = TRUE, selected = TRUE,
                  anthropometry = FALSE)

# Limpando as bases de dados
pns2013 = pns2013 %>%
  mutate(
    crise_asma_12m = case_when(
      Q076 == "Sim" ~ 1,
      Q076 == "Não" ~ 0,
      TRUE ~ NA_real_
    ),
    asma_medic = case_when(
      Q07701 == "Sim" ~ 1,
      Q07701 == "Não" ~ 0,
      TRUE ~ NA_real_
    ),
    asma_medic_zeroNA = ifelse(is.na(asma_medic), 0, asma_medic),
    pop_taxa_medic_asma = 1
  )

pns2019 = pns2019 %>%
  mutate(
    crise_asma_12m = case_when(
      Q076 == "Sim" ~ 1,
      Q076 == "Não" ~ 0,
      TRUE ~ NA_real_
    ),
    asma_medic = case_when(
      Q07704 %in% c("Sim, todos", "Sim, alguns") | Q07708 == "Sim" ~ 1,
      Q07704 == "Não, nenhum" & Q07708 == "Não" ~ 0,
      TRUE ~ NA_real_
    ),
    asma_medic_zeroNA = ifelse(is.na(asma_medic), 0, asma_medic),
    pop_taxa_medic_asma = 1
  )

# Definindo o desenho amostral para MORADOR SELECIONADO
design_pns2013 = cria_design_pns(pns2013)
design_pns2019 = cria_design_pns(pns2019)

# Porcentagem de pessoas com asma que usam medicamentos atualmente
medic_asma_2013 = estima_prop_beta("asma_medic", design_pns2013,
                                    "pct_medic_asma", "asma_medic")
medic_asma_2013$ano = 2013

medic_asma_2019 = estima_prop_beta("asma_medic", design_pns2019,
                                    "pct_medic_asma", "asma_medic")
medic_asma_2019$ano = 2019

# Juntando tudo
medic_asma_2013_2019 = rbind(medic_asma_2013, medic_asma_2019)

# Salvando a base no formato long
write.csv(medic_asma_2013_2019, file = "df_medic_asma_2013_2019_long.csv", row.names = FALSE)

# Transformando no formato wide
medic_asma_2013_2019_wide = medic_asma_2013_2019 %>%
  select(uf, ano, pct_medic_asma) %>%
  pivot_wider(
    names_from = ano,
    values_from = pct_medic_asma,
    names_prefix = "pct_medic_asma_"
  )

# Salvando a base no formato wide
write_xlsx(medic_asma_2013_2019_wide, path = "df_medic_asma_2013_2019_wide.xlsx")

# Corrigindo a base de dados
df_plot = medic_asma_2013_2019 %>%
  mutate(
    ano = factor(ano, levels = c(2019, 2013)),  # garante ordem das cores
    uf = factor(uf, 
                levels = c(setdiff(unique(uf), "Brasil"), "Brasil")) # Brasil por último
  )

# Criando gráfico
ggplot(df_plot, aes(x = uf, 
                    y = pct_medic_asma, 
                    fill = ano)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.8) +
  geom_errorbar(aes(ymin = li, ymax = ls),
                position = position_dodge(width = 0.9), width = 0.3) +
  coord_flip() +
  labs(x = NULL,
       y = "(%) de pessoas com diagnóstico de asma e que \n usam medicamentos orais ou bombinhas para tratamento de asma.",
       fill = NULL) +
  scale_fill_manual(values = c("2013" = "#a7a6f2", "2019" = "#ead4a4"),
                    breaks = c("2013", "2019")) + 
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size = 11))

# Porcentagem de asmáticos que tiveram crises recentes e que consomem medicamentos
medic_crise_asma_2013 = estima_prop_beta(
  "asma_medic",
  subset(design_pns2013, crise_asma_12m == 1),
  "pct_medic_crise_asma",
  "asma_medic"
)
medic_crise_asma_2013$ano = 2013

medic_crise_asma_2019 = estima_prop_beta(
  "asma_medic",
  subset(design_pns2019, crise_asma_12m == 1),
  "pct_medic_crise_asma",
  "asma_medic"
)
medic_crise_asma_2019$ano = 2019

# Juntando tudo
medic_crise_asma_2013_2019 = rbind(medic_crise_asma_2013, medic_crise_asma_2019)

# Salvando a base no formato long
write.csv(medic_crise_asma_2013_2019, file = "df_medic_crise_asma_2013_2019_long.csv", row.names = FALSE)

# Transformando no formato wide
medic_crise_asma_2013_2019_wide = medic_crise_asma_2013_2019 %>%
  select(uf, ano, pct_medic_crise_asma) %>%
  pivot_wider(
    names_from = ano,
    values_from = pct_medic_crise_asma,
    names_prefix = "pct_medic_crise_asma_"
  )

# Salvando a base no formato wide
write_xlsx(medic_crise_asma_2013_2019_wide, path = "df_medic_crise_asma_2013_2019_wide.xlsx")

# Corrigindo a base de dados
df_plot = medic_crise_asma_2013_2019 %>%
  mutate(
    ano = factor(ano, levels = c(2019, 2013)),  # garante ordem das cores
    uf = factor(uf, 
                levels = c(setdiff(unique(uf), "Brasil"), "Brasil")) # Brasil por último
  )

# Criando gráfico
ggplot(df_plot, aes(x = uf, 
                    y = pct_medic_crise_asma, 
                    fill = ano)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.8) +
  geom_errorbar(aes(ymin = li, ymax = ls),
                position = position_dodge(width = 0.9), width = 0.3) +
  coord_flip() +
  labs(x = NULL,
       y = "(%) de pessoas que tiveram crise de asma nos últimos 12 meses e que \n usam medicamentos orais ou bombinhas para tratamento de asma.",
       fill = NULL) +
  scale_fill_manual(values = c("2013" = "#a7a6f2", "2019" = "#ead4a4"),
                    breaks = c("2013", "2019")) + 
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size = 11))

# Total de usuários de medicamentos orais ou bombinhas
df_taxa_medic_asma_2013 = estima_taxa_medic_100k(
  "asma_medic_zeroNA",
  "pop_taxa_medic_asma",
  design_pns2013
)
df_taxa_medic_asma_2013$ano = rep(2013, nrow(df_taxa_medic_asma_2013))

df_taxa_medic_asma_2019 = estima_taxa_medic_100k(
  "asma_medic_zeroNA",
  "pop_taxa_medic_asma",
  design_pns2019
)
df_taxa_medic_asma_2019$ano = rep(2019, nrow(df_taxa_medic_asma_2019))

# Juntando as tabelas
df_taxa_medic_asma_2013_2019 = rbind(
  df_taxa_medic_asma_2013,
  df_taxa_medic_asma_2019
)

# Salvando a base no formato long
write.csv(df_taxa_medic_asma_2013_2019, file = "df_taxa_medic_asma_2013_2019_long.csv", row.names = FALSE)

# Transformando no formato wide
taxa_medic_asma_2013_2019_wide1 = df_taxa_medic_asma_2013_2019 %>%
  select(uf, ano, total_medic_asma) %>%
  pivot_wider(
    names_from = ano,
    values_from = total_medic_asma,
    names_prefix = "total_medic_asma_"
  )

# Transformando no formato wide
taxa_medic_asma_2013_2019_wide2 = df_taxa_medic_asma_2013_2019 %>%
  select(uf, ano, taxa_100k_medic_asma) %>%
  pivot_wider(
    names_from = ano,
    values_from = taxa_100k_medic_asma,
    names_prefix = "taxa_100k_medic_asma_"
  )

taxa_medic_asma_2013_2019_wide = merge(taxa_medic_asma_2013_2019_wide1, taxa_medic_asma_2013_2019_wide2,
                                       by.x = "uf", by.y = "uf")

# Salvando a base no formato wide
write_xlsx(taxa_medic_asma_2013_2019_wide, path = "df_taxa_medic_asma_2013_2019_wide.xlsx")

# Corrigindo a base de dados
df_plot = df_taxa_medic_asma_2013_2019 %>%
  mutate(
    ano = factor(ano, levels = c(2019, 2013)),  # garante ordem das cores
    uf = factor(uf, 
                levels = c(setdiff(unique(uf), "Brasil"), "Brasil")) # Brasil por último
  )

# Criando gráfico
ggplot(df_plot, aes(x = uf, 
                    y = taxa_100k_medic_asma, 
                    fill = ano)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.8) +
  geom_errorbar(aes(ymin = lim_inf_taxa_100k_medic_asma, ymax = lim_sup_taxa_100k_medic_asma),
                position = position_dodge(width = 0.9), width = 0.3) +
  coord_flip() +
  labs(x = NULL,
       y = "Taxa de usuários de medicamentos orais ou bombinhas para tratamento de asma \n por 100 mil habitantes.",
       fill = NULL) +
  scale_fill_manual(values = c("2013" = "#a7a6f2", "2019" = "#ead4a4"),
                    breaks = c("2013", "2019")) + 
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size = 11))
