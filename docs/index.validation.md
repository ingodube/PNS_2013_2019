# Validação do padrão de publicação

Data da validação: 29/07/2026 (America/Bahia)

Resultado geral local: **PASS**

## Artefatos e integridade

- Fonte editável: `docs/metodologia_pns.Rmd`
- Fonte canônica: `docs/metodologia_canonico.html`
- Página pública: `docs/metodologia.html`
- Entrada do site: `docs/index.html`, com redirecionamento para `metodologia.html`
- Gerador: `tools/publication_standard.py`
- Git blob canônico: `1b13ab21178c7b3b913f96e2ca589dd31115ac94`
- SHA-256 canônico: `f18e3d802c47740b5601c8107ed58eb8944e066c0afb4398ee44389a7a001bcf`
- SHA-256 da publicação: `0971b418b030083516ff42f7c1708f2019d3b0063d8322ca0446eae6c466b229`
- Renderização do R Markdown com Pandoc 3.8.3: PASS
- Geração determinística do wrapper contra `docs/metodologia.html`: PASS
- URLs primária e fallback configuradas para `metodologia_canonico.html`: PASS

## Correções editoriais verificadas

- Quadro “Arquivos R documentados neste relatório” removido: PASS
- Treze tabelas analíticas preservadas e numeradas de 1 a 13: PASS
- Primeira coluna e respectivo cabeçalho alinhados à esquerda: PASS
- Demais colunas e respectivos cabeçalhos centralizados: PASS
- Tabela 10 verificada linha a linha com alinhamentos `left/center`: PASS
- Quatro avisos metodológicos convertidos em cards editoriais sóbrios: PASS
- Títulos das 13 tabelas e 10 figuras centralizados, em negrito e acima dos objetos: PASS
- Anos da PNS presentes em todos os títulos de tabelas: PASS
- Cabeçalhos da Tabela 13 iguais a “Indicador” e “Leitura”: PASS
- Declaração de uso de IA presente no corpo e ausente do sumário: PASS
- Data do relatório atualizada para 29/07/2026: PASS

## Estrutura e preservação

| Item | Versão anterior | Nova versão | Resultado |
|---|---:|---:|---|
| Quadro operacional inicial | 1 | 0 | PASS — remoção solicitada |
| Tabelas analíticas | 13 | 13 | PASS |
| Células das tabelas analíticas | iguais | iguais | PASS |
| Figuras | 10 | 10 | PASS |
| Títulos de figuras | 10 | 10 | PASS |
| Links | 27 | 27 | PASS |
| Blocos de código | 4 | 4 | PASS |
| Expressões matemáticas em TeX | 9 | 9 | PASS |
| Referências | 13 | 13 | PASS |
| Cards metodológicos | 4 | 4 | PASS |

As células das Tabelas 1–13, os destinos dos links, os títulos das figuras, as expressões matemáticas, os blocos de código, as referências e a ordem documental foram comparados com a versão anterior. As únicas alterações textuais aceitas foram a inclusão dos anos nos títulos, a capitalização dos cabeçalhos da Tabela 13, a nova data e a remoção do quadro solicitado.

## Validação responsiva no Microsoft Edge

| Largura | `clientWidth` | `scrollWidth` | Sangramento | Sumário | Tabelas | Figuras | Resultado |
|---:|---:|---:|---:|---|---:|---:|---|
| 320 px | 320 | 320 | 0 | abre e fecha | 13 | 10 | PASS |
| 360 px | 360 | 360 | 0 | abre e fecha | 13 | 10 | PASS |
| 390 px | 390 | 390 | 0 | abre e fecha | 13 | 10 | PASS |
| 768 px | 768 | 768 | 0 | abre e fecha | 13 | 10 | PASS |
| 1024 px | 1024 | 1024 | 0 | aberto no desktop | 13 | 10 | PASS |
| 1440 px | 1440 | 1440 | 0 | aberto no desktop | 13 | 10 | PASS |

Em todas as larguras, não houve erro de console nem elemento fora do papel. O sumário permaneceu contido; todos os contêineres de tabela ficaram dentro do viewport; títulos e legendas permaneceram acima de seus objetos; cards, fontes e notas ficaram íntegros; e o texto das notas permaneceu justificado. Nenhuma tabela exigiu rolagem horizontal com os dados atuais, mas os contêineres internos continuam preparados para ativá-la quando necessário.

## Interface pública

- Endereço preservado: `https://ingodube.github.io/PNS_2013_2019/metodologia.html`
- O redirecionamento de `docs/index.html` e o QR code existente não precisam ser alterados.
- A cópia canônica embutida foi validada por `file://`, evitando falha de carregamento local.
- A confirmação da nova versão no GitHub Pages será executada após o push.
