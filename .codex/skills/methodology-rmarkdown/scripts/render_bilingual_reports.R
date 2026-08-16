args <- commandArgs(trailingOnly = TRUE)

script_flag <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_flag) != 1L) stop("The skill directory could not be located.")
script_path <- normalizePath(sub("^--file=", "", script_flag), winslash = "/", mustWork = TRUE)
render_script <- file.path(dirname(script_path), "render_report.R")

repo_root <- normalizePath(if (length(args) > 0L) args[[1L]] else getwd(), winslash = "/", mustWork = TRUE)
python <- Sys.which("python")
if (!nzchar(python)) stop("Python was not found; it is required to generate the EN source.")
localizer <- file.path(repo_root, "tools", "localize_report.py")
localize_status <- system2(python, localizer)
if (!identical(localize_status, 0L)) stop("The English report source could not be generated.")

jobs <- list(
  c("--input", file.path(repo_root, "docs", "methodology_report_en.Rmd"),
    "--output", file.path(repo_root, "docs", "metodologia.html")),
  c("--input", file.path(repo_root, "docs", "methodology_report_pt_br.Rmd"),
    "--output", file.path(repo_root, "docs", "methodology-pt-br.html"))
)

for (job in jobs) {
  status <- system2(file.path(R.home("bin"), "Rscript.exe"), c(render_script, job))
  if (!identical(status, 0L)) stop("A localized report failed to render.")
}

message("Both localized reports rendered successfully.")
