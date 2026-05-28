# Carregando pacotes necessários
library(dplyr)
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

estima_prop_beta = function(var, label, design){
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
        asma_pg = NA_real_,
        ci_l = NA_real_,
        ci_u = NA_real_,
        pct_asma_med_pg = NA_real_,
        li = NA_real_,
        ls = NA_real_,
        tipo_med = label,
        metodo_ic = "indisponivel"
      ))
    }
    
    ci = tryCatch(as.numeric(confint(est)), error = function(e) c(NA_real_, NA_real_))
    valor = as.numeric(coef(est))[1]
    
    if (!is.finite(valor) || length(ci) < 2 || any(!is.finite(ci))) {
      return(data.frame(
        uf = uf_atual,
        asma_pg = NA_real_,
        ci_l = NA_real_,
        ci_u = NA_real_,
        pct_asma_med_pg = NA_real_,
        li = NA_real_,
        ls = NA_real_,
        tipo_med = label,
        metodo_ic = "indisponivel"
      ))
    }
    
    data.frame(
      uf = uf_atual,
      asma_pg = valor,
      ci_l = ci[1],
      ci_u = ci[2],
      pct_asma_med_pg = valor * 100,
      li = ci[1] * 100,
      ls = ci[2] * 100,
      tipo_med = label,
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

# Limpando as bases de dados
pns2019 = pns2019 %>%
  mutate(
    asma_oral_pg = case_when(
      Q07707 == "Sim" ~ 1,
      Q07707 == "Não" ~ 0,
      TRUE ~ NA_real_
    ),
    asma_bombinha_pg = case_when(
      Q07711 == "Sim" ~ 1,
      Q07711 == "Não" ~ 0,
      TRUE ~ NA_real_
    ),
      asma_oral_fp = case_when(
        Q07705 %in% c("Sim, todos", "Sim, alguns") ~ 1,
        Q07705 == "Não, nenhum" ~ 0,
        TRUE ~ NA_real_
      ),
      asma_oral_publico = case_when(
        Q07706 %in% c("Sim, todos", "Sim, alguns") ~ 1,
        Q07706 == "Não, nenhum" ~ 0,
        TRUE ~ NA_real_
      ),
      asma_bombinha_fp = case_when(
        Q07709 %in% c("Sim, todos", "Sim, alguns") ~ 1,
        Q07709 == "Não, nenhum" ~ 0,
        TRUE ~ NA_real_
      ),
      asma_bombinha_publico = case_when(
        Q07710 %in% c("Sim, todos", "Sim, alguns") ~ 1,
        Q07710 == "Não, nenhum" ~ 0,
        TRUE ~ NA_real_
      ),
    asma_bombinha_pg_zeroNA = ifelse(is.na(asma_bombinha_pg), 0, asma_bombinha_pg),
    asma_bombinha_fp_zeroNA = ifelse(is.na(asma_bombinha_fp), 0, asma_bombinha_fp),
    asma_bombinha_publico_zeroNA = ifelse(is.na(asma_bombinha_publico), 0, asma_bombinha_publico),
    asma_oral_pg_zeroNA = ifelse(is.na(asma_oral_pg), 0, asma_oral_pg),
    asma_oral_fp_zeroNA = ifelse(is.na(asma_oral_fp), 0, asma_oral_fp),
    asma_oral_publico_zeroNA = ifelse(is.na(asma_oral_publico), 0, asma_oral_publico)
  )

design_pns2019 = cria_design_pns(pns2019)

# Estimação do percentual de usuários de medicamentos orais que fizeram o desembolso
asma_oral_pg = estima_prop_beta("asma_oral_pg", "Medicamento oral", design_pns2019)
asma_bombinha_pg = estima_prop_beta("asma_bombinha_pg", "Bombinha", design_pns2019)

# Juntando tudo
df_desembolso = rbind(asma_oral_pg, asma_bombinha_pg)

# Salvando a base de dados
write_xlsx(df_desembolso, path = "df_desembolso.xlsx")

# Gerando o gráfico
df_plot = df_desembolso %>%
  mutate(
    tipo_med = factor(tipo_med, levels = c("Bombinha", "Medicamento oral")),
    uf = factor(uf,
                levels = c(setdiff(unique(uf), "Brasil"), "Brasil")) # Brasil por último
  )

ggplot(df_plot, aes(x = uf, 
                    y = pct_asma_med_pg, 
                    fill = tipo_med)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.8) +
  geom_errorbar(aes(ymin = li,
                    ymax = ls),
                position = position_dodge(width = 0.9), 
                width = 0.3) +
  coord_flip() +
  labs(x = NULL,
       y = "(%) de usuários de medicamentos de asma que fizeram desembolso",
       fill = NULL) +
  scale_fill_manual(values = c("Medicamento oral" = "#a7a6f2", "Bombinha" = "#ead4a4"),
                    breaks = c("Medicamento oral", "Bombinha")) + 
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size = 11))

# Estimando o total e a taxa por 100 mil habitantes de pessoas que fizeram o desembolso de medicamentos orais ou bombinhas
