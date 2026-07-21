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

- Mantenha fora do Git os outputs ja cobertos pelo `.gitignore`: `df_*.csv`, `df_*.xlsx`, `Rplots.pdf`, `~$*.xlsx` e `PNS_2013_2019/`.
- Versione preferencialmente scripts R, documentacao e configuracoes do projeto.
- Se algum output precisar ser versionado, registre a razao no commit e ajuste o `.gitignore` de forma especifica.
