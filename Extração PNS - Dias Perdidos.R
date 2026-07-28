# Carregando pacotes necessários
library(dplyr)
library(tidyr)
library(PNSIBGE)
library(survey)
library(writexl)

options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

fmt_decimal = function(x, digits = 2, big_mark = FALSE){
  ifelse(
    is.na(x),
    NA_character_,
    format(
      round(x, digits),
      decimal.mark = ",",
      big.mark = ifelse(big_mark, ".", ""),
      nsmall = digits,
      trim = TRUE
    )
  )
}

formatar_ic_pct = function(estimativa, li, ls){
  ifelse(
    is.na(estimativa) | is.na(li) | is.na(ls),
    NA_character_,
    paste0(fmt_decimal(estimativa, 2), "% [",
           fmt_decimal(li, 2), "%; ",
           fmt_decimal(ls, 2), "%]")
  )
}

formatar_ic_media = function(estimativa, li, ls){
  ifelse(
    is.na(estimativa) | is.na(li) | is.na(ls),
    NA_character_,
    paste0(fmt_decimal(estimativa, 1), " [",
           fmt_decimal(li, 1), "; ",
           fmt_decimal(ls, 1), "]")
  )
}

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
                method = "beta", level = 0.95, na.rm = TRUE),
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

  rbind(est_uf, est_br) %>%
    mutate(
      uf = as.character(uf),
      estimativa_ic = formatar_ic_pct(pct, li, ls)
    ) %>%
    arrange(if_else(uf == "Brasil", 1L, 0L), uf)
}

estima_media_dias = function(var, label, design, day_var = "J003"){
  formula_day = as.formula(paste0("~", day_var))

  estima_uma = function(design_atual, uf_atual){
    subdesign = subset(design_atual, get(var) == 1)

    est = tryCatch(
      svymean(formula_day, design = subdesign, na.rm = TRUE),
      error = function(e) NULL
    )

    if (is.null(est)) {
      return(data.frame(
        uf = uf_atual,
        tipo = label,
        media_dias = NA_real_,
        li = NA_real_,
        ls = NA_real_,
        metodo_ic = "normal"
      ))
    }

    ci = tryCatch(as.numeric(confint(est, level = 0.95)), error = function(e) c(NA_real_, NA_real_))
    valor = as.numeric(coef(est))[1]

    data.frame(
      uf = uf_atual,
      tipo = label,
      media_dias = valor,
      li = ci[1],
      ls = ci[2],
      metodo_ic = "normal"
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
      uf = as.character(uf),
      estimativa_ic = formatar_ic_media(media_dias, li, ls)
    ) %>%
    arrange(if_else(uf == "Brasil", 1L, 0L), uf)
}

variaveis_2019 = c("V0001", "V0024", "UPA_PNS", "ID_DOMICILIO", "V0006_PNS",
                   "V0025A", "V0025B", "J002", "J003", "J00402",
                   "V0028", "V0029", "V0030", "V00281", "V00282", "V00291",
                   "V00292", "V00283", "V00293", "V00301", "V00302", "V00303")

variaveis_2013 = c("V0001", "V0024", "UPA_PNS", "ID_DOMICILIO", "V0006_PNS",
                   "V0025", "J002", "J003", "J004",
                   "V0028", "V0029", "V0030", "V00281", "V00282", "V00291",
                   "V00292", "V00283", "V00293")

pns2019 = get_pns(year = 2019, vars = variaveis_2019,
                  design = FALSE, labels = TRUE, selected = TRUE,
                  anthropometry = FALSE)

pns2013 = get_pns(year = 2013, vars = variaveis_2013,
                  design = FALSE, labels = TRUE, selected = TRUE,
                  anthropometry = FALSE)

categorias_2019 = list(
  dor_ossos = "Problemas nos ossos e articulações",
  enxaqueca = "Dor de cabeça ou enxaqueca",
  gineco_obstetrico = "Problemas gineco-obstétricos",
  dor_dente = "Problema odontológico",
  asma_dpoc = "Problemas respiratórios",
  gastrointestinal = "Problemas gastrointestinais",
  dengue = "Dengue, Chikungunya, Zika",
  cardiovascular = "Problemas cardiovasculares",
  diabetes = "Diabetes",
  cancer = "Câncer",
  neurologico = "Problemas neurológicos",
  mental = "Saúde mental",
  fratura = "Lesões ou fraturas",
  outro = "Outro problema de saúde",
  nao_declarado = "Não declarado"
)

cria_indicador_causa = function(motivo, padrao){
  case_when(
    grepl(padrao, motivo, ignore.case = TRUE) ~ 1,
    !is.na(motivo) ~ 0,
    TRUE ~ NA_real_
  )
}

pns2019 = pns2019 %>%
  mutate(
    absenteismo = case_when(
      J002 == "Sim" ~ 1,
      J002 == "Não" ~ 0,
      TRUE ~ NA_real_
    ),
    dor_ossos = cria_indicador_causa(as.character(J00402), categorias_2019$dor_ossos),
    enxaqueca = cria_indicador_causa(as.character(J00402), categorias_2019$enxaqueca),
    gineco_obstetrico = cria_indicador_causa(as.character(J00402), categorias_2019$gineco_obstetrico),
    dor_dente = cria_indicador_causa(as.character(J00402), categorias_2019$dor_dente),
    asma_dpoc = cria_indicador_causa(as.character(J00402), categorias_2019$asma_dpoc),
    gastrointestinal = cria_indicador_causa(as.character(J00402), categorias_2019$gastrointestinal),
    dengue = cria_indicador_causa(as.character(J00402), categorias_2019$dengue),
    cardiovascular = cria_indicador_causa(as.character(J00402), categorias_2019$cardiovascular),
    diabetes = cria_indicador_causa(as.character(J00402), categorias_2019$diabetes),
    cancer = cria_indicador_causa(as.character(J00402), categorias_2019$cancer),
    neurologico = cria_indicador_causa(as.character(J00402), categorias_2019$neurologico),
    mental = cria_indicador_causa(as.character(J00402), categorias_2019$mental),
    fratura = cria_indicador_causa(as.character(J00402), categorias_2019$fratura),
    outro = cria_indicador_causa(as.character(J00402), categorias_2019$outro),
    nao_declarado = cria_indicador_causa(as.character(J00402), categorias_2019$nao_declarado),
    respiratorio_harmonizado = asma_dpoc
  )

pns2013 = pns2013 %>%
  mutate(
    absenteismo = case_when(
      J002 == "Sim" ~ 1,
      J002 == "Não" ~ 0,
      TRUE ~ NA_real_
    ),
    respiratorio_harmonizado = case_when(
      as.character(J004) %in% c("Asma / bronquite / pneumonia", "Resfriado / gripe") ~ 1,
      !is.na(J004) ~ 0,
      TRUE ~ NA_real_
    )
  )

design_pns2019 = cria_design_pns(pns2019)
design_pns2013 = cria_design_pns(pns2013)

df_absenteismo = estima_prop_beta("absenteismo", "Geral", design_pns2019) %>%
  rename(pct_absenteismo = pct)

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

lista_resultados = mapply(
  estima_prop_beta,
  var = variaveis,
  label = labels,
  MoreArgs = list(design = design_pns2019),
  SIMPLIFY = FALSE
)

df_final = do.call(rbind, lista_resultados) %>%
  mutate(denominador = ifelse(tipo == "Geral",
                              "Morador selecionado",
                              "Moradores que deixaram de realizar atividades habituais por motivo de saúde"))

write_xlsx(df_final, path = "df_absenteismo.xlsx")

lista_resultados = mapply(
  estima_media_dias,
  var = variaveis,
  label = labels,
  MoreArgs = list(design = design_pns2019),
  SIMPLIFY = FALSE
)

df_dias_perdidos = do.call(rbind, lista_resultados) %>%
  filter(uf == "Brasil") %>%
  select(tipo, media_dias, li, ls, metodo_ic, estimativa_ic)

write_xlsx(df_dias_perdidos, path = "df_dias_perdidos.xlsx")

df_absenteismo_respiratorio_2013_2019 = bind_rows(
  estima_prop_beta("respiratorio_harmonizado", "Respiratório harmonizado", design_pns2013) %>%
    mutate(ano = 2013),
  estima_prop_beta("respiratorio_harmonizado", "Respiratório harmonizado", design_pns2019) %>%
    mutate(ano = 2019)
) %>%
  mutate(
    denominador = "Moradores que deixaram de realizar atividades habituais por motivo de saúde",
    harmonizacao = paste(
      "2013: Asma/bronquite/pneumonia + Resfriado/gripe;",
      "2019: Problemas respiratórios, incluindo resfriado/gripe/sinusite/asma/bronquite/pneumonia."
    )
  )

write_xlsx(df_absenteismo_respiratorio_2013_2019,
           path = "df_absenteismo_respiratorio_2013_2019.xlsx")

df_dias_perdidos_respiratorio_2013_2019 = bind_rows(
  estima_media_dias("respiratorio_harmonizado", "Respiratório harmonizado", design_pns2013) %>%
    mutate(ano = 2013),
  estima_media_dias("respiratorio_harmonizado", "Respiratório harmonizado", design_pns2019) %>%
    mutate(ano = 2019)
) %>%
  mutate(
    denominador = "Moradores cujo principal motivo de perda de atividades foi respiratório harmonizado",
    harmonizacao = paste(
      "2013: Asma/bronquite/pneumonia + Resfriado/gripe;",
      "2019: Problemas respiratórios, incluindo resfriado/gripe/sinusite/asma/bronquite/pneumonia."
    )
  )

write_xlsx(df_dias_perdidos_respiratorio_2013_2019,
           path = "df_dias_perdidos_respiratorio_2013_2019.xlsx")
