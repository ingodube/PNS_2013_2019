# Carregando pacotes necessários
library(dplyr)
library(PNSIBGE)
library(survey)
library(writexl)
library(deflateBR)

options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

cria_design_pns = function(data_pns){
  data_design = data_pns %>%
    select(-any_of(c("V0028", "V00281", "V00282", "V00283",
                     "V0030", "V00301", "V00302", "V00303")))
  
  pns_design(data_pns = data_design)
}

estima_media = function(var, design, nome_media){
  formula_var = as.formula(paste0("~", var))
  
  estima_uma = function(design_atual, uf_atual){
    est = svymean(formula_var, design = design_atual, na.rm = TRUE)
    ci = as.numeric(confint(est))
    
    data.frame(
      UF = uf_atual,
      media = as.numeric(coef(est))[1],
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
  
  rbind(est_br, est_uf) %>%
    rename(!!nome_media := media)
}

variaveis_2019 = c("V0001", "V0024", "UPA_PNS", "ID_DOMICILIO", "V0006_PNS",
                   "V0025A", "V0025B", "E01602", "E01604", "E01802", "E01804",
                   "F001021", "F007021", "F008021", "VDF00102",
                   "V0028", "V0029", "V0030", "V00281", "V00282", "V00291",
                   "V00292", "V00283", "V00293", "V00301", "V00302", "V00303")

pns2019 = get_pns(year = 2019, vars = variaveis_2019,
                  design = FALSE, labels = TRUE, selected = TRUE,
                  anthropometry = FALSE)

pns2019 = pns2019 %>%
  mutate(F008021 = as.integer(F008021))

vars_renda = c("E01602", "E01604", "E01802", "E01804",
               "F001021", "F007021", "F008021", "VDF00102")

# Criando a variável renda declarada
pns2019 = pns2019 %>%
  mutate(
    renda = if_else(
      if_all(all_of(vars_renda), is.na),
      NA_real_,
      rowSums(select(., all_of(vars_renda)), na.rm = TRUE)
    )
  )

# Deflacionando os salários
pns2019$data_referencia = as.Date(paste0("2019-06-30")) # Baseline
pns2019$renda_def = round(deflate(pns2019$renda, pns2019$data_referencia, "08/2025", "ipca"), 2)

# Desenho amostral PNS 2019
design_pns2019 = cria_design_pns(pns2019)

# Calculando a estimativa de renda média por UF e Brasil
df_renda_2019 = estima_media("renda_def", design_pns2019, "renda_def")

# Salvando a base de dados
write_xlsx(df_renda_2019, path = "df_renda_2019.xlsx")
