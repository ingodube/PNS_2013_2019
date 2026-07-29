# AGENTS.md

## Fluxo local

- Use `C:\Users\ingon\Github\PNS_2013_2019` como pasta principal deste repositório no Codex e no VSCode.
- O remoto esperado e `origin`, apontando para `https://github.com/ingodube/PNS_2013_2019.git`.
- A branch de trabalho padrao e `main`.

## Verificacoes antes de alterar

Antes de editar, confirme o estado local:

```powershell
git status --short --branch
git remote -v
git log --oneline --max-count=5
```

Se houver commits locais pendentes de envio, revise:

```powershell
git log origin/main..main --oneline
git show --stat --oneline --summary origin/main..main
```

## Sincronizacao

- Para publicar commits locais revisados, use `git push origin main`.
- Depois do envio, `git status --short --branch` deve mostrar `main...origin/main`, sem `ahead`.
- Nao crie outro clone local para este projeto sem necessidade explicita.

## Arquivos gerados

- Mantenha todos os codigos R em `Códigos/` e todas as tabelas analiticas tratadas em `Tabelas tratadas/`.
- Os CSV e XLSX de `Tabelas tratadas/` sao produtos validados e intencionalmente versionados para permitir a renderizacao do relatorio sem recalcular os microdados.
- Mantenha fora do Git arquivos `df_*.csv` e `df_*.xlsx` criados por engano na raiz, alem de `Rplots.pdf`, `~$*.xlsx` e copias locais em `PNS_2013_2019/`.
- O unico poster vigente deve ficar em `poster/poster_pns_asma_morrison.pdf`.
- Nao recrie `outputs/posters/`; versoes anteriores permanecem recuperaveis pelo historico do Git.

## Skill local

- Para relatorios metodologicos em R Markdown, use a skill repo-local em `.codex/skills/methodology-rmarkdown/`.
- Essa skill registra as regras de titulo, introducao, texto justificado, legendas de figuras acima dos graficos, equacoes centralizadas, notacao matematica inline, referencias numeradas, linguagem recomendada e validacao antes de publicar.
