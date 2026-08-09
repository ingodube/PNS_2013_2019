# Preservação e validação

## Baseline obrigatório

Antes de editar, registrar SHA-256 do HTML vigente e extrair:

- texto visível por bloco;
- títulos e ordem de seções;
- células e ordem das tabelas;
- destinos dos links;
- TeX ou MathML das fórmulas;
- texto dos blocos de código;
- bytes decodificados de cada figura;
- referências bibliográficas e seus campos.

## Diferenças autorizadas

Definir explicitamente as diferenças aceitas. Mudanças de estilo podem alterar classes, elementos de apresentação, ordem dos metadados, numeração gerada, formato das citações e ordem bibliográfica, mas não podem alterar afirmações científicas ou campos bibliográficos sem autorização.

## Citações e referências cruzadas

- Usar citações narrativas `Autor (ano)` ou parentéticas `(Autor, ano)`.
- Separar fontes múltiplas por ponto e vírgula.
- Usar IDs semânticos como `ref-autor-ano`, `fig-descricao` e `tab-descricao`.
- Manter referências sem numeração e em ordem alfabética por autor pessoal ou institucional.
- Exigir IDs únicos e um destino existente para todo `href` interno.
- Confirmar que sumário e corpo exibem a mesma numeração.
- Confirmar que todos os destinos recebem realce visual com `:target`.

## Critérios de aceite

- HTML válido, autocontido e com viewport.
- Quantidades esperadas de tabelas, figuras, códigos, fórmulas, links e referências.
- Igualdade das células, fórmulas, códigos, links externos e pixels das figuras em uma atualização apenas editorial.
- Nenhuma citação numérica residual no padrão isolado `[n]` quando o padrão autor-data foi selecionado.
- Títulos de figuras e tabelas imediatamente antes dos objetos.
- Cabeçalhos alinhados como seus valores.
- Zero overflow documental nas larguras 320, 360, 390, 768, 1024 e 1440px.
- Sumário aberto, contido e navegável; links internos sem destinos quebrados.
- Verificação local e, depois da publicação, na URL pública.
