# Pesquisa Nacional de Saúde 2013-2019: Asma, Plano Amostral e Indicadores

<p align="justify">
A Pesquisa Nacional de Saúde (PNS) é um dos principais inquéritos domiciliares realizados no Brasil, conduzido pelo Instituto Brasileiro de Geografia e Estatística (IBGE) em parceria com o Ministério da Saúde. A pesquisa produz informações sobre condições de saúde, doenças crônicas, estilo de vida, acesso e utilização dos serviços de saúde, medicamentos, bem-estar e desigualdades sociais. Por sua abrangência nacional e por seu desenho amostral complexo, a PNS é uma fonte central para a vigilância epidemiológica e para a formulação de políticas públicas orientadas por evidências.
</p>

<p align="justify">
Este repositório documenta e implementa, em linguagem R, procedimentos para analisar a evolução de indicadores relacionados à asma no Brasil entre 2013 e 2019. O trabalho tem como objetivo estimar a prevalência de diagnóstico médico de asma, a ocorrência de crises nos últimos 12 meses, o uso de medicamentos orais ou bombinhas, taxas populacionais por 100 mil habitantes, fatores de risco como tabagismo e indicadores complementares de acesso, absenteísmo, dias perdidos e renda. A proposta é tornar o fluxo analítico transparente, auditável e reprodutível.
</p>

## Implementação do Plano Amostral

<p align="justify">
O plano amostral da PNS é probabilístico, estratificado e por conglomerados em múltiplos estágios. Na prática, isso significa que as estimativas não devem ser calculadas como se os microdados viessem de uma amostra aleatória simples. Os códigos deste repositório constroem o desenho amostral com o pacote <code>PNSIBGE</code> e utilizam o pacote <code>survey</code> para incorporar pesos, estratos e unidades primárias de amostragem. Esse cuidado é essencial para produzir estimativas válidas de prevalência, médias, totais, taxas por 100 mil habitantes e intervalos de confiança.
</p>

<p align="justify">
Para proporções binárias, como diagnóstico médico de asma, crise recente, uso de medicamentos e tabagismo, os códigos utilizam <code>survey::svyciprop(method = "beta")</code>. O uso do intervalo de confiança beta é importante porque respeita os limites naturais das proporções entre 0 e 1, evita truncamentos manuais e oferece leitura mais prudente em domínios pequenos, como Unidades da Federação e subgrupos de pessoas com asma. Os resultados são apresentados com intervalos de confiança de 95%, permitindo avaliar a magnitude das estimativas e a incerteza associada.
</p>

## Relatório Metodológico

<p align="justify">
O relatório metodológico descreve detalhadamente os passos de extração, recodificação das variáveis, construção dos indicadores, implementação do desenho amostral complexo, cálculo dos intervalos de confiança, produção dos gráficos e interpretação dos resultados. O trabalho também discute interoperabilidade de dados observacionais, ETL, OMOP/OHDSI e a utilidade da PNS como camada populacional de referência para análises em saúde.
</p>

**Acesse o relatório:**  
[https://ingodube.github.io/PNS_2013_2019/metodologia.html](https://ingodube.github.io/PNS_2013_2019/metodologia.html)

## Organização Geral

- Códigos R de extração e análise: arquivos `Extração PNS - *.R`.
- Documentação metodológica: `docs/metodologia_pns.Rmd`.
- Relatório publicado: `docs/metodologia.html`.
- Pôsteres científicos: `outputs/posters/`.

## Reprodutibilidade

<p align="justify">
Os arquivos de dados gerados localmente, como <code>df_*.csv</code> e <code>df_*.xlsx</code>, permanecem fora do controle de versão conforme definido no <code>.gitignore</code>. O objetivo é versionar os códigos, a documentação e os produtos finais necessários para leitura e comunicação dos achados, mantendo os outputs intermediários reproduzíveis a partir das rotinas analíticas.
</p>
