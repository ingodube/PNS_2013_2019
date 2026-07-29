# Validação do padrão de publicação

Data da validação: 29/07/2026 (America/Bahia)

Resultado geral: **PASS**

## Artefatos e integridade

- Fonte canônica: `docs/metodologia_canonico.html`
- Página de publicação e endereço público preservado: `docs/metodologia.html`
- Entrada do site: `docs/index.html`, com redirecionamento para `metodologia.html`
- Gerador: `tools/publication_standard.py`
- Git blob canônico: `2532735be200512e58e331f8a39ddda22c6e95d5`
- SHA-256 canônico: `ca2130a01e21c1b558ba0aa31ff82a5141c3a8d90669c259a364dd0262b0759b`
- SHA-256 da publicação: `e6a5617b7d5fda4717045281af122e49ec1b8668c6a63040a888a3b56b1e3089`
- Validação do pacote anexado: PASS
- Geração determinística contra `docs/index.html`: PASS
- URLs primária e fallback configuradas para `metodologia_canonico.html`: PASS

O blob e o SHA-256 da fonte canônica permaneceram inalterados durante toda a geração.

## Verificações estáticas

- Um elemento `html`, um `head` e um `body`: PASS
- Metadado de viewport: PASS
- Fundo `#050505`, papel branco, acento `#e65b2c` e medida de leitura de 46 rem: PASS
- Corpo de 17 px com altura de linha 1,64: PASS
- Pontuação visual da numeração de títulos sem alteração do texto: PASS
- Quebra de URLs e código, proteção de fórmulas e rolagem interna de tabelas: PASS
- Ausência de gradientes, sombras grandes e rotinas de reescrita ou recoloração: PASS
- Entrada e saída distintas: PASS

## Preservação do conteúdo renderizado

As comparações foram feitas entre o documento canônico e o DOM da página de publicação carregada localmente.

| Item | Fonte | Publicação | Resultado |
|---|---:|---:|---|
| Título e seções substantivas | 23 | 23 | PASS |
| Metadados originalmente marcados como títulos | 2 | 2 campos de metadados | PASS |
| Tabelas e respectivas células | 14 | 14 | PASS |
| Figuras e fontes de imagem | 10 | 10 | PASS |
| Legendas de figuras | 10 | 10 | PASS |
| Links no artigo | 26 | 26 | PASS |
| Link nos metadados | 1 | 1 | PASS |
| Blocos de código | 4 | 4 | PASS |
| Expressões matemáticas em TeX | 9 | 9 | PASS |
| Notas metodológicas | 4 | 4 | PASS |

Textos, ordem, células, destinos de links, fontes de imagem, legendas e TeX foram comparados por igualdade. As nove expressões também foram renderizadas visualmente sem alterar sua fonte TeX.

## Validação responsiva em navegador real

| Largura | `clientWidth` | `scrollWidth` | Sangramento | Sumário | Tabelas | Figuras | Resultado |
|---:|---:|---:|---:|---|---:|---:|---|
| 320 px | 320 | 320 | 0 | abre e fecha | 14 | 10 | PASS |
| 360 px | 360 | 360 | 0 | abre e fecha | 14 | 10 | PASS |
| 390 px | 390 | 390 | 0 | abre e fecha | 14 | 10 | PASS |
| 768 px | 768 | 768 | 0 | abre e fecha | 14 | 10 | PASS |
| 1024 px | 1024 | 1024 | 0 | aberto no desktop | 14 | 10 | PASS |
| 1440 px | 1440 | 1440 | 0 | aberto no desktop | 14 | 10 | PASS |

Em todas as larguras, o sumário permaneceu dentro da coluna de leitura, os contêineres de tabelas ficaram dentro do viewport, a pontuação visual dos títulos esteve ativa e não houve erro de console. Nenhuma tabela precisou de rolagem horizontal com o conteúdo atual; os contêineres internos continuam preparados para ativá-la quando necessário.

A inspeção visual incluiu o cabeçalho e o sumário em 320 e 1440 px, além da primeira nota metodológica, da primeira tabela e da transição para a seção seguinte em 320 px.

## Limitação operacional

A página de publicação busca `metodologia_canonico.html` em tempo de execução. A validação local usou o parâmetro `source` apontando para um servidor local; as URLs remotas do novo arquivo canônico só estarão disponíveis depois do envio deste commit. Nenhuma publicação ou envio remoto foi realizado nesta validação.

O exemplo de renderização existente em `metodologia_pns.Rmd` continua indicando `metodologia.html` para não alterar o conteúdo metodológico. Antes de uma futura publicação, a saída dessa renderização deve ser usada como `metodologia_canonico.html` e o gerador precisa ser executado novamente para evitar a substituição do wrapper.
