# Padrão visual de publicação

## Tokens

- Fundo externo: `#050505`
- Papel: `#ffffff`
- Texto: `#202124`
- Texto secundário: `#62666b`
- Regra: `#c9cccf`
- Acento: `#e65b2c`
- Acento escuro: `#b8421d`
- Fundo de código: `#f5f5f3`
- Corpo: Georgia, `17px`, altura de linha `1.64`
- Interface: Arial/Helvetica
- Página: `78rem`
- Cabeçalho e sumário: `68rem`
- Coluna de leitura: `46rem`

## Estrutura

- Apresentar título grande alinhado à esquerda e metadados em linhas simples.
- Usar uma regra laranja de 3px abaixo dos metadados.
- Manter o sumário em `<details open>`, com duas colunas acima de 960px e uma abaixo.
- Aplicar regra superior fina apenas aos capítulos posteriores ao primeiro.
- Usar texto serifado e títulos, metadados, sumário, tabelas e legendas sem serifa.
- Manter títulos de figuras e tabelas acima, centralizados, em negrito, `0.78rem` e cor secundária.
- Manter fontes e notas abaixo, justificadas, com a última linha à esquerda e tipografia discreta.
- Usar somente regras horizontais em tabelas, sem preenchimentos coloridos ou zebra.
- Justificar referências em `0.93rem` e alinhar a última linha à esquerda.
- Realçar destinos internos com fundo `#fff3eb` sem alterar o fluxo do documento.

## Responsividade

- Reduzir a página para `3rem 2.1rem 5rem` até 960px e `2rem 0.9rem 4rem` até 640px.
- Usar corpo de 16px até 640px.
- Permitir rolagem apenas em tabelas, códigos ou fórmulas que realmente a exijam.
- Manter `scrollWidth <= clientWidth` em todo o documento.
- Quebrar URLs e palavras longas; nunca ampliar o viewport.

## Proibições

- Gradientes, sombras grandes, chips, badges e cartões decorativos.
- Cores institucionais adicionais.
- Legendas abaixo de figuras ou tabelas.
- Dependências externas de apresentação em HTML autocontido.
- Alteração de dados, pixels, fórmulas ou interpretação para obter melhor encaixe.
