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

estima_prop_logit = function(var, design, pct_col){
  formula_var = as.formula(paste0("~", var))
  
  estima_uma = function(design_atual, uf_atual){
    est = svyciprop(formula_var, design = design_atual,
                    method = "logit", na.rm = TRUE)
    ci = as.numeric(confint(est))
    
    data.frame(
      uf = uf_atual,
      valor = as.numeric(coef(est))[1],
      ci_l = ci[1],
      ci_u = ci[2]
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
    mutate(
      !!pct_col := valor * 100,
      li = ci_l * 100,
      ls = ci_u * 100
    )
}

variaveis_2019 = c("V0001", "V0024", "UPA_PNS", "ID_DOMICILIO", "V0006_PNS",
                   "V0025A", "V0025B", "Q076",
                   "V0028", "V0029", "V0030", "V00281", "V00282", "V00291",
                   "V00292", "V00283", "V00293", "V00301", "V00302", "V00303")

pns2019 = get_pns(year = 2019, vars = variaveis_2019,
                  design = FALSE, labels = TRUE, selected = TRUE,
                  anthropometry = FALSE)


variaveis_2013 = c("V0001", "V0024", "UPA_PNS", "ID_DOMICILIO", "V0006_PNS",
                   "V0025", "Q076", "V0028", "V0029", "V00281", "V00282", "V00291",
                   "V00292", "V00283", "V00293")

pns2013 = get_pns(year = 2013, vars = variaveis_2013,
                  design = FALSE, labels = TRUE, selected = TRUE,
                  anthropometry = FALSE)

# Tratando os dados
pns2013 = pns2013 %>%
  mutate(crise_asma_12m = ifelse(Q076 == "Sim", 1, ifelse(Q076 == "Não", 0, NA)))

pns2019 = pns2019 %>%
  mutate(crise_asma_12m = ifelse(Q076 == "Sim", 1, ifelse(Q076 == "Não", 0, NA)))

# Definindo o desenho amostral para MORADOR SELECIONADO
design_pns2013 = cria_design_pns(pns2013)
design_pns2019 = cria_design_pns(pns2019)

# Prevalência de crises de asma nos últimos 12 meses
prev_crise_2013 = estima_prop_logit("crise_asma_12m", design_pns2013, "prevalencia")
prev_crise_2013$ano = 2013

prev_crise_2019 = estima_prop_logit("crise_asma_12m", design_pns2019, "prevalencia")
prev_crise_2019$ano = 2019

# Juntando crises
prev_crise_2013_2019 = rbind(prev_crise_2013, prev_crise_2019)

# Salvando a base no formato long
write.csv(prev_crise_2013_2019, file = "df_prev_crise_2013_2019_long.csv", row.names = FALSE)

# Transformando no formato wide
prev_crise_2013_2019_wide = prev_crise_2013_2019 %>%
  select(uf, ano, prevalencia) %>%
  pivot_wider(
    names_from = ano,
    values_from = prevalencia,
    names_prefix = "prevalencia_"
  )

# Salvando a base no formato wide
write_xlsx(prev_crise_2013_2019_wide, path = "df_prev_crise_2013_2019_wide.xlsx")

# Corrigindo a base de dados
df_plot = prev_crise_2013_2019 %>%
  mutate(
    ano = factor(ano, levels = c(2019, 2013)),  # garante ordem das cores
    uf = factor(uf, 
                levels = c(setdiff(unique(uf), "Brasil"), "Brasil")) # Brasil por último
  )

# Criando gráfico
ggplot(df_plot, aes(x = uf, 
                    y = prevalencia, 
                    fill = ano)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.8) +
  geom_errorbar(aes(ymin = li, ymax = ls),
                position = position_dodge(width = 0.9), width = 0.3) +
  coord_flip() +
  labs(x = NULL,
       y = "Prevalência (%) de crises asmáticas nos últimos 12 meses.",
       fill = NULL) +
  scale_fill_manual(values = c("2013" = "#a7a6f2", "2019" = "#ead4a4"),
                    breaks = c("2013", "2019")) + 
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size = 11))
