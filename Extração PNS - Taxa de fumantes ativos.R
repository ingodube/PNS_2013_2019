# Carregando pacotes necessários
library(dplyr)
library(tidyr)
library(PNSIBGE)
library(ggplot2)
library(survey)
library(sidrar)
library(writexl)

variaveis_2019 = c("V0001", "V0024", "UPA_PNS", "V0006_PNS",
                   "V0025A", "V0025B", "P050",
                   "V0028", "V0029", "V0030", "V00281", "V00282", "V00291",
                   "V00292", "V00283", "V00293", "V00301", "V00302", "V00303")

pns2019 = get_pns(year = 2019, vars = variaveis_2019,
                   design = FALSE, labels = TRUE, selected = TRUE,
                   anthropometry = FALSE)


variaveis_2013 = c("V0001", "V0024", "UPA_PNS", "V0006_PNS",
                   "V0025", "P050", "V0028", "V0029", "V00281", "V00282", "V00291",
                   "V00292", "V00283", "V00293")

pns2013 = get_pns(year = 2013, vars = variaveis_2013,
                  design = FALSE, labels = TRUE, selected = TRUE,
                  anthropometry = FALSE)

# Tratando os dados
pns2013 = pns2013 %>%
  mutate(fumante = case_when(
    P050 %in% c("Sim, diariamente", "Sim, menos que diariamente") ~ 1,
    P050 == "Não fumo atualmente" ~ 0,
    TRUE ~ NA_real_ # preserva NA
  ))

pns2019 = pns2019 %>%
  mutate(fumante = case_when(
    P050 %in% c("Sim, diariamente", "Sim, menos que diariamente") ~ 1,
    P050 == "Não fumo atualmente" ~ 0,
    TRUE ~ NA_real_ # preserva NA
  ))

# Definindo o desenho amostral para MORADOR SELECIONADO
design_pns2013 = svydesign(
  id = ~UPA_PNS,        # Unidades primárias de amostragem
  strata = ~V0024,      # Estratos
  weights = ~V00291,    # Peso calibrado do morador selecionado
  data = pns2013,
  nest = TRUE
)

design_pns2019 = svydesign(
  id = ~UPA_PNS,        # Unidades primárias de amostragem
  strata = ~V0024,      # Estratos
  weights = ~V00291,    # Peso calibrado do morador selecionado
  data = pns2019,
  nest = TRUE
)

# Calculando taxa de fumantes por UF
pop_fumante_uf_2013 = svyby(
  ~fumante,
  ~V0001,
  design = design_pns2013,
  svytotal,   # total estimado de fumantes por UF
  vartype = c("ci"),
  na.rm = TRUE
)

pop_total_uf_2013 = svyby(
  ~I(!is.na(fumante)),  # todos os moradores selecionados válidos
  ~V0001,
  design = design_pns2013,
  svytotal,
  na.rm = TRUE
)

df_taxa_fumantes_2013 = data.frame(
  uf = pop_fumante_uf_2013$V0001, # Unidade Federativa
  total_fumantes = pop_fumante_uf_2013$fumante, # Total estimado de fumantes ativos
  lim_inf_total_fumantes = pop_fumante_uf_2013$ci_l, # Limite inferior do IC do total estimado
  lim_sup_total_fumantes = pop_fumante_uf_2013$ci_u, # Limite superior do IC do total estimado
  total_pop = pop_total_uf_2013$`I(!is.na(fumante))TRUE` # População total projetada (2013)
) %>%
  mutate(
    taxa_100k_fumantes = round((total_fumantes / total_pop) * 100000),
    lim_inf_taxa_100k_fumantes = round((lim_inf_total_fumantes / total_pop) * 100000),
    lim_sup_taxa_100k_fumantes = round((lim_sup_total_fumantes / total_pop) * 100000)
  )

df_taxa_fumantes_2013$ano = rep(2013, nrow(df_taxa_fumantes_2013))

pop_fumante_uf_2019 = svyby(
  ~fumante,
  ~V0001,
  design = design_pns2019,
  svytotal,   # total estimado de fumantes por UF
  vartype = c("ci"),
  na.rm = TRUE
)

pop_total_uf_2019 = svyby(
  ~I(!is.na(fumante)),  # todos os moradores selecionados válidos
  ~V0001,
  design = design_pns2019,
  svytotal,
  na.rm = TRUE
)

df_taxa_fumantes_2019 = data.frame(
  uf = pop_fumante_uf_2019$V0001, # Unidade Federativa
  total_fumantes = pop_fumante_uf_2019$fumante, # Total estimado de fumantes ativos
  lim_inf_total_fumantes = pop_fumante_uf_2019$ci_l, # Limite inferior do IC do total estimado
  lim_sup_total_fumantes = pop_fumante_uf_2019$ci_u, # Limite superior do IC do total estimado
  total_pop = pop_total_uf_2019$`I(!is.na(fumante))TRUE` # População total projetada (2013)
) %>%
  mutate(
    taxa_100k_fumantes = round((total_fumantes / total_pop) * 100000),
    lim_inf_taxa_100k_fumantes = round((lim_inf_total_fumantes / total_pop) * 100000),
    lim_sup_taxa_100k_fumantes = round((lim_sup_total_fumantes / total_pop) * 100000)
  )

df_taxa_fumantes_2019$ano = rep(2019, nrow(df_taxa_fumantes_2019))

# Juntando as tabelas
df_taxa_fumantes_2013_2019 = rbind(df_taxa_fumantes_2013, df_taxa_fumantes_2019)

# Salvando a base no formato long
write.csv(df_taxa_fumantes_2013_2019, file = "df_taxa_fumantes_2013_2019_long.csv")


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
