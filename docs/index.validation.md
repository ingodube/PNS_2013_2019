# Validação do relatório metodológico

Data da validação local: 09/08/2026 (America/Bahia)

Resultado geral local: **PASS**

## Pipeline de publicação

- Fonte editável: `docs/metodologia_pns.Rmd`.
- Saída pública direta e autocontida: `docs/metodologia.html`.
- Redirecionamento preservado: `docs/index.html` → `metodologia.html`.
- Endereço preservado: `https://ingodube.github.io/PNS_2013_2019/metodologia.html`.
- Renderização: R 4.6.1 e Pandoc 3.8.3, com MathML, `theme: null`, template e CSS próprios.
- Pipeline anterior removido: `tools/publication_standard.py` e `docs/metodologia_canonico.html`.
- Dependências externas de apresentação: 0 — PASS.
- Geração determinística em duas renderizações consecutivas: PASS.

## Integridade e hashes

| Artefato | SHA-256 / Git blob |
|---|---|
| HTML anterior usado como baseline | `1548a4877956544f2235009d6de48f2eca7f3fd3c5dd43df3d5b4cef1b384357` |
| `docs/metodologia.html` | `cba2e22014035b421e9c43886dd7dea0d44cef5ddd00eb1c5987ad7b328d1fc0` |
| Git blob de `docs/metodologia.html` | `153a1882aa657e3f635be65972570120e291943a` |
| `docs/metodologia_pns.Rmd` | `2a141d0bb138c6f1677f04dc1d6735395afdfeeaf10f3812204eaeacb2adbca2` |
| Template Pandoc | `4df20e07e97cdbd35036042e8ae6c6084de5ea8199180ac2d3ed1cff56ccc324` |
| CSS editorial | `87df8f2f540aba1373925c7ebb1bc46b086013c43782e63b20a81037e0d2ee95` |
| Script de renderização | `4202f893085e12a74c3d4e57d99cdcde5bc518e836021fe3642e1f15ac0f4672` |

Hashes SHA-256 dos pixels decodificados das dez figuras, na ordem documental:

1. `8328f3fe4f56b4d1a53c35957402abd350b007f8cce6e21226bfe2490b3ffd33`
2. `cf8bf599cd665dadfca7ba4e93a5b6cf6a50d86f7c25d2425e398aabb88c6b97`
3. `3dc9e39b64ef5303839be324dc72dce85dd46d658f75c117cbd04b868e6e3543`
4. `95610c279c698ba24a48def9dbec963aa6dccf725e100895401c75bc00063092`
5. `bb1a0ea748a09e72ab155fd0959abe74e981748b5063a83f0dabce4326d003a7`
6. `b93a9acf6593a117353d440140e54b9ce7b796dfe21e3b9416fbcb848dc69d24`
7. `35430388e0550621f002cbba84afbe3a20864d52a9e6c50a36cbc3342fbfffe9`
8. `b6034d6d062baebc611a2c0f14e4c4d368913ab11c3ecad42ca381ed44c7e017`
9. `1a43dd5abbe2421a153d347cc5c02a84d04c5169086a974760b07332c0acbaab`
10. `1440959a479de9279d8e2556a43f2efb5f08e4ac1c4a6b12c5d79a07b6a4389a`

## Autoria, citações e navegação

| Verificação | Resultado |
|---|---|
| Blocos de autoria estruturada | exatamente 2 — PASS |
| Credenciais e e-mail de Ingo Dube Souza | correspondência literal — PASS |
| Credenciais de Maria Eduarda de Lima e Silva | correspondência literal, sem e-mail inventado — PASS |
| Data visível do relatório | `29/07/2026`, preservada — PASS |
| Referências | 13, sem numeração e em ordem alfabética — PASS |
| Citações numéricas residuais no texto | 0 — PASS |
| IDs de referências | 13 semânticos e únicos — PASS |
| IDs de figuras e tabelas | 10 `fig-*` e 13 `tab-*`, únicos — PASS |
| Destinos internos ausentes | 0 — PASS |
| Links do sumário | 21 destinos únicos — PASS |
| Numeração do sumário e corpo | correspondência integral — PASS |
| Declaração de IA | presente no corpo e ausente do sumário — PASS |
| Realce visual de `:target` | PASS |

## Preservação científica

| Item | Baseline | Nova saída | Resultado |
|---|---:|---:|---|
| Tabelas | 13 | 13 | PASS |
| Células de tabelas | 836 | 836, igualdade textual e de ordem | PASS |
| Figuras | 10 | 10 | PASS |
| Pixels das figuras | 10 hashes | 10 hashes idênticos | PASS |
| Blocos de código | 4 | 4 | PASS |
| Blocos científicos de código | 3 | 3 idênticos | PASS |
| Exemplo operacional de renderização | pipeline antigo | comando da skill | alteração autorizada |
| Expressões matemáticas | 9 | 9 em MathML | PASS |
| Links externos anteriores | 10 | 10 preservados | PASS |
| Novo link de autoria | — | `mailto:ingo.dube@ufba.br` | alteração autorizada |
| Avisos metodológicos | 4 | 4 como prosa integrada | PASS |

As diferenças textuais aceitas foram limitadas à autoria completa, às citações autor-data, à ordenação alfabética e retirada da numeração bibliográfica, às ligações internas e ao exemplo operacional do novo pipeline. Dados, células, fórmulas, campos bibliográficos, afirmações científicas, figuras e sua ordem foram preservados.

## Validação visual e responsiva no Microsoft Edge

| Largura | `clientWidth` | `scrollWidth` | Sangramento | Colunas do sumário | Abre/fecha | Resultado |
|---:|---:|---:|---:|---:|---|---|
| 320 px | 320 | 320 | 0 | 1 | PASS | PASS |
| 360 px | 360 | 360 | 0 | 1 | PASS | PASS |
| 390 px | 390 | 390 | 0 | 1 | PASS | PASS |
| 768 px | 768 | 768 | 0 | 1 | PASS | PASS |
| 1024 px | 1024 | 1024 | 0 | 2 | n/a (desktop) | PASS |
| 1440 px | 1440 | 1440 | 0 | 2 | n/a (desktop) | PASS |

Em todas as larguras: zero elementos fora do papel; sumário contido; títulos de figuras e tabelas acima dos objetos; cabeçalhos alinhados como os valores de cada coluna; fontes, notas, texto e referências justificados; tabelas limitadas aos próprios contêineres; quatro blocos de código; 13 tabelas; dez figuras; 13 referências; e zero erro de console.

## Validação da skill

- `quick_validate.py`: PASS.
- Fixture estático: PASS.
- Análise sintática de Python, Node.js e R: PASS.
- Validação parametrizada contra o HTML baseline: PASS.

## GitHub Pages

Validação pública pendente do primeiro push desta versão. Após a publicação, este registro deve confirmar HTTP 200, igualdade do SHA-256 servido e os testes em 390 e 1440 px.
