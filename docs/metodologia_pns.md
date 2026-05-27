# Metodologia PNS nos scripts de extração

Este repositório usa os microdados da Pesquisa Nacional de Saúde (PNS) por meio do pacote `PNSIBGE`.

## Desenho amostral

Os scripts devem construir o plano amostral com `PNSIBGE::pns_design()` sempre que possível. Para análises do morador selecionado, os dados enviados ao `pns_design()` devem conter o conjunto de variáveis do peso do morador selecionado:

- `UPA_PNS`
- `ID_DOMICILIO`
- `V0024`
- `V0029`
- `V00291`
- `V00292`
- `V00293`

Quando a base também contiver os conjuntos de peso domiciliar (`V0028`, `V00281`, `V00282`, `V00283`) ou de antropometria (`V0030`, `V00301`, `V00302`, `V00303`), os helpers locais removem esses campos antes de chamar `pns_design()`. Isso evita que o pacote escolha outro conjunto de pesos antes do peso do morador selecionado.

Os scripts também devem usar:

```r
options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)
```

Essas opções são compatíveis com o fluxo usado pelo `PNSIBGE::pns_design()` para lidar com PSU solitária e domínios com PSU solitária.

## Intervalos de confiança

Para proporções binárias, o padrão dos scripts é `survey::svyciprop(method = "beta")` com nível de 95%. Esse método evita os limites fora da escala 0-1 que podem aparecer com intervalos Wald calculados por `svymean()`/`confint()` e é estável em domínios pequenos da PNS.

Os scripts não devem truncar manualmente limites superiores em 100%. Se um intervalo de confiança de proporção precisar respeitar a escala percentual, a transformação deve vir do estimador adequado, não de pós-processamento manual.

## Taxas por 100 mil

Taxas por 100 mil habitantes devem ser estimadas como proporções ponderadas multiplicadas por `100000`, com o intervalo de confiança calculado na proporção original e depois reescalado.

Não usar o intervalo de confiança de um total estimado dividido por um denominador pontual como intervalo da taxa. Essa operação não propaga corretamente a variância da razão.

## Referências

- `PNSIBGE::pns_design()` cria um objeto de desenho amostral da PNS para análise com o pacote `survey`: <https://www.rdocumentation.org/packages/PNSIBGE/versions/0.2.1/topics/pns_design>
- O código-fonte do `pns_design()` mostra o uso de `survey.lonely.psu = "adjust"`, `survey.adjust.domain.lonely = TRUE` e pós-estratificação por conjunto de pesos: <https://rdrr.io/cran/PNSIBGE/src/R/pns_design.R>
- `survey::svyciprop()` documenta métodos de IC para proporções em desenhos complexos, incluindo `method = "beta"`: <https://rdrr.io/rforge/survey/man/svyciprop.html>
