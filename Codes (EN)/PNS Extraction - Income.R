# Generated entry point. Definitions and estimators are shared across languages.
roots <- c(Sys.getenv("PNS_REPO_ROOT"), getwd(), dirname(getwd()))
root <- roots[file.exists(file.path(roots, "R", "pns_core.R"))][1]
if (is.na(root)) stop("Run from repository root or set PNS_REPO_ROOT")
source(file.path(root, "R", "pns_core.R"), encoding="UTF-8")
run_group("income", "en")
