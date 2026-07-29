# Validação da organização e do padrão de publicação

Data da validação: 29/07/2026 (America/Bahia)

Resultado geral local: **PASS**

## Organização do repositório

| Verificação | Resultado |
|---|---|
| Códigos R exclusivamente em `Códigos/` | 7 arquivos — PASS |
| Dados tratados exclusivamente em `Tabelas tratadas/` | 6 CSV e 13 XLSX — PASS |
| Arquivos R, CSV ou XLSX soltos na raiz | 0 — PASS |
| Conteúdo de `poster/` | somente `poster_pns_asma_morrison.pdf` — PASS |
| Pasta antiga `outputs/` | ausente — PASS |
| Pasta local duplicada `PNS_2013_2019/` | arquivada fora do repositório — PASS |
| Política entre `.gitignore`, `AGENTS.md` e README | harmonizada — PASS |

Os três códigos locais divergentes e o `Rplots.pdf` foram preservados em arquivo externo antes da limpeza. As quatro apresentações PPTX removidas continuam recuperáveis pelo histórico do Git.

## Códigos e tabelas

- Análise sintática dos sete arquivos R: PASS.
- Localização da raiz a partir da raiz do projeto ou de `Códigos/`: PASS.
- Escritas `write.csv()` e `write_xlsx()` direcionadas para `Tabelas tratadas/`: PASS.
- Leitura do relatório direcionada para `Tabelas tratadas/`: PASS.
- Exemplo de execução atualizado para `source(file.path("Códigos", ...))`: PASS.
- `df_população_2013_2019_wide.xlsx` incorporado ao conjunto versionado: PASS.

## Pôster

- Arquivo: `poster/poster_pns_asma_morrison.pdf`.
- SHA-256: `2e360c8de9d1e094ca81859c32d8502d0e3c3dfc245d8e8d4fc911db018e79e3`.
- Uma página, sem criptografia: PASS.
- Renderização visual sem cortes, sobreposições ou glifos corrompidos: PASS.
- Igualdade binária com o anexo original: PASS.

## Artefatos do relatório

- Fonte editável: `docs/metodologia_pns.Rmd`.
- Fonte canônica: `docs/metodologia_canonico.html`.
- Página pública: `docs/metodologia.html`.
- Git blob canônico: `51949fe0cbffd41be172980879bd4e01f522951e`.
- SHA-256 canônico no diretório de trabalho: `1548a4877956544f2235009d6de48f2eca7f3fd3c5dd43df3d5b4cef1b384357`.
- SHA-256 canônico após normalização de fim de linha pelo Git: `971d97c80e057266bab25928f290be9803032df61dfc2be6e11c1bbd30df1fe8`.
- SHA-256 do wrapper: `6a473ac3235a96895e5d3588b1aa26b6f2dd60ee6e7075553ce6513da2ca0ff4`.
- Renderização com Pandoc 3.8.3: PASS.
- Geração determinística do wrapper: PASS.

## Preservação do relatório

| Item | Versão anterior | Nova versão | Resultado |
|---|---:|---:|---|
| Tabelas analíticas | 13 | 13 | PASS |
| Células das tabelas | iguais | iguais | PASS |
| Figuras | 10 | 10 | PASS |
| Pixels das figuras | iguais | iguais | PASS |
| Links | 27 | 27 | PASS |
| Blocos de código | 4 | 4 | PASS |
| Expressões matemáticas em TeX | 9 | 9 | PASS |
| Referências | 13 | 13 | PASS |
| Cards metodológicos | 4 | 4 | PASS |

A única alteração visível no conteúdo do relatório é o caminho operacional necessário no exemplo de `source()`. Textos científicos, células, links, fórmulas, referências, títulos, figuras e ordem documental permaneceram inalterados.

## Validação responsiva no Microsoft Edge

| Largura | `clientWidth` | `scrollWidth` | Sangramento | Sumário | Resultado |
|---:|---:|---:|---:|---|---|
| 320 px | 320 | 320 | 0 | abre e fecha | PASS |
| 360 px | 360 | 360 | 0 | abre e fecha | PASS |
| 390 px | 390 | 390 | 0 | abre e fecha | PASS |
| 768 px | 768 | 768 | 0 | abre e fecha | PASS |
| 1024 px | 1024 | 1024 | 0 | aberto no desktop | PASS |
| 1440 px | 1440 | 1440 | 0 | aberto no desktop | PASS |

Em todas as larguras, o relatório apresentou 13 tabelas, 10 figuras, quatro blocos de código e quatro cards, sem erro de console, falha de carregamento, elemento fora do papel ou desalinhamento de títulos e colunas.

## Interface pública

- Endereço preservado: `https://ingodube.github.io/PNS_2013_2019/metodologia.html`.
- O QR code existente não precisa ser alterado.
- A cópia canônica embutida foi validada por `file://`.
- A nova versão pública e o link do PDF serão confirmados após o push.
