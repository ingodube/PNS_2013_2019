# Pesquisa Nacional de Saúde 2013-2019: Asma, Plano Amostral e Indicadores

<p align="justify">
A Pesquisa Nacional de Saúde (PNS), conduzida pelo Instituto Brasileiro de Geografia e Estatística em parceria com o Ministério da Saúde, é um inquérito domiciliar de abrangência nacional com desenho amostral complexo. Este repositório documenta e implementa, em R, procedimentos para analisar indicadores relacionados à asma no Brasil em 2013 e 2019, incorporando pesos, estratos e unidades primárias de amostragem.
</p>

<p align="justify">
As rotinas estimam prevalência de diagnóstico médico de asma, ocorrência de crises nos últimos 12 meses, uso de medicamentos orais ou bombinhas, taxas populacionais, tabagismo, desembolso, absenteísmo, dias perdidos e renda. O objetivo é manter o fluxo analítico transparente, auditável e reproduzível, com intervalos de confiança compatíveis com o plano amostral da PNS.
</p>

## Produtos principais

- [Relatório metodológico publicado](https://ingodube.github.io/PNS_2013_2019/metodologia.html)
- [Pôster científico](poster/poster_pns_asma_morrison.pdf)
- [Fonte editável do relatório](docs/metodologia_pns.Rmd)

## Organização do repositório

```text
PNS_2013_2019/
├── Códigos/              # rotinas R de extração, tratamento e estimação
├── Tabelas tratadas/     # CSV e XLSX validados usados pelo relatório
├── poster/               # versão vigente do pôster científico
├── docs/                 # fonte, artefatos e página publicada do relatório
├── tools/                # gerador do wrapper editorial
├── AGENTS.md             # orientações duráveis para manutenção
└── README.md
```

## Implementação do plano amostral

<p align="justify">
Os códigos constroem o desenho amostral com o pacote <code>PNSIBGE</code> e utilizam o pacote <code>survey</code> para incorporar pesos, estratos e conglomerados. Para proporções binárias, o fluxo utiliza <code>survey::svyciprop(method = "beta")</code>, produzindo intervalos de confiança de 95% que respeitam os limites naturais das proporções e oferecem leitura mais prudente em domínios pequenos.
</p>

## Reprodutibilidade

Execute os códigos a partir da raiz do repositório. Cada arquivo em [`Códigos/`](Códigos/) localiza essa raiz, cria `Tabelas tratadas/` quando necessário e grava os resultados diretamente nessa pasta. Um exemplo de execução é:

```r
source(file.path("Códigos", "Extração PNS - Asma.R"))
```

As rotinas dependem, conforme o indicador, dos pacotes `PNSIBGE`, `survey`, `dplyr`, `tidyr`, `ggplot2`, `writexl` e `deflateBR`. Os microdados brutos não são incluídos no repositório e são obtidos pelos mecanismos documentados nos próprios códigos.

## Política para dados tratados

Os CSV e XLSX de [`Tabelas tratadas/`](Tabelas%20tratadas/) são produtos analíticos validados e permanecem versionados porque alimentam diretamente o relatório metodológico. Arquivos temporários, bloqueios do Excel, `Rplots.pdf`, cópias locais de trabalho e tabelas geradas acidentalmente na raiz permanecem fora do Git conforme o `.gitignore`.

## Relatório metodológico

O relatório descreve a extração, a recodificação das variáveis, a construção dos indicadores, a implementação do desenho amostral, o cálculo dos intervalos de confiança e a interpretação dos resultados. O endereço público permanece estável para preservar referências externas e o QR code do pôster.
