# Carregando pacotes necessários
library(dplyr)
library(tidyr)
library(PNSIBGE)
library(survey)
library(writexl)
library(fastDummies)

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
        pct = NA_real_,
        li = NA_real_,
        ls = NA_real_,
        tipo = label,
        metodo_ic = "indisponivel"
      ))
    }
    
    ci = tryCatch(as.numeric(confint(est)), error = function(e) c(NA_real_, NA_real_))
    valor = as.numeric(coef(est))[1]
    
    if (!is.finite(valor) || length(ci) < 2 || any(!is.finite(ci))) {
      return(data.frame(
        uf = uf_atual,
        pct = NA_real_,
        li = NA_real_,
        ls = NA_real_,
        tipo = label,
        metodo_ic = "indisponivel"
      ))
    }
    
    data.frame(
      uf = uf_atual,
      pct = valor * 100,
      li = ci[1] * 100,
      ls = ci[2] * 100,
      tipo = label,
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
  
  rbind(est_br, est_uf)
}

variaveis_2019 = c("V0001", "V0024", "UPA_PNS", "ID_DOMICILIO", "V0006_PNS",
                   "V0025A", "V0025B", "J002", "J003", "J00402",
                   "V0028", "V0029", "V0030", "V00281", "V00282", "V00291",
                   "V00292", "V00283", "V00293", "V00301", "V00302", "V00303")

pns2019 = get_pns(year = 2019, vars = variaveis_2019,
                  design = FALSE, labels = TRUE, selected = TRUE,
                  anthropometry = FALSE)

# Tratando os dados
pns2019 = pns2019 %>%
  mutate(
    absenteismo = case_when(
      J002 == "Sim" ~ 1,
      J002 == "Não" ~ 0,
      TRUE ~ NA_real_
    )
  )

pns2019 = pns2019 %>%
  dummy_cols(select_columns = "J00402", 
             remove_first_dummy = FALSE, # mantém todas as categorias
             remove_selected_columns = TRUE, # remove a original, deixa só as dummies
             ignore_na = TRUE)

nomes_causas = c("dor_ossos", "enxaqueca", "gineco_obstetrico", "dor_dente",
                 "asma_dpoc", "gastrointestinal", "dengue", "cardiovascular",
                 "diabetes", "cancer", "neurologico", "mental", "fratura",
                 "outro", "nao_declarado")

dummies_j00402 = names(pns2019)[startsWith(names(pns2019), "J00402_")]

if (length(dummies_j00402) != length(nomes_causas)) {
  stop("Número inesperado de categorias em J00402; revisar nomes das dummies antes de estimar.")
}

pns2019 = pns2019 %>%
  rename_with(~nomes_causas, all_of(dummies_j00402))

# Desenho amostral da PNS 2019
design_pns2019 = cria_design_pns(pns2019)

# Estimação do percentual de pessoas que deixaram de realizar quaisquer de suas atividades
# habituais por conta de alguma doença
df_absenteismo = estima_prop_beta("absenteismo", "Geral", design_pns2019) %>%
  rename(pct_absenteismo = pct)

# Função para estimar prevalência de absenteísmo por causa específica
estima_absenteismo = function(var, label, design){
  estima_prop_beta(var, label, design)
}

# Lista de variáveis dummy e labels correspondentes
variaveis = c("absenteismo", "dor_ossos", "enxaqueca", "gineco_obstetrico", 
               "dor_dente", "asma_dpoc", "gastrointestinal", "dengue", 
               "cardiovascular", "diabetes", "cancer", "neurologico", 
               "mental", "fratura", "outro", "nao_declarado")

labels = c("Geral", "Dor nos ossos e articulações", "Enxaqueca", 
            "Gineco-obstétrico", "Dor de dente", "Respiratório (asma/bronquite/pneumonia)", 
            "Gastrointestinal", "Dengue/Chikungunya/Zika/Febre amarela", 
            "Cardiovascular", "Diabetes", "Câncer", "Neurológico", 
            "Saúde mental", "Fratura/violência", "Outro problema de saúde", 
            "Não declarado")

# Rodando para todas as variáveis
lista_resultados = mapply(
  estima_absenteismo,
  var = variaveis,
  label = labels,
  MoreArgs = list(design = design_pns2019),
  SIMPLIFY = FALSE
)

# Empilhando em um único data.frame
df_final = do.call(rbind, lista_resultados)

# Salvando a base de dados
write_xlsx(df_final, path = "df_absenteismo.xlsx")

# Função para estimar a média de dias perdidos por motivo
estima_dias_perdidos = function(var, label, design){
  
  # restringe a amostra apenas aos que marcaram o motivo
  subdesign = subset(design, get(var) == 1)
  
  # estima média de dias (J003) nesse subgrupo
  est = svymean(~J003, design = subdesign, na.rm = TRUE)
  ci = as.numeric(confint(est))
  
  data.frame(
    tipo = label,
    media_dias = as.numeric(coef(est)),
    li = ci[1],
    ls = ci[2]
  )
}

# Rodando para todas as variáveis
lista_resultados = mapply(
  estima_dias_perdidos,
  var = variaveis,
  label = labels,
  MoreArgs = list(design = design_pns2019),
  SIMPLIFY = FALSE
)

# Empilhando em um único data.frame
df_dias_perdidos = do.call(rbind, lista_resultados)

# Salvando os resultados
write_xlsx(df_dias_perdidos, path = "df_dias_perdidos.xlsx")
