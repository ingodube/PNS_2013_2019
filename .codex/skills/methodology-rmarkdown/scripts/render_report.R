args <- commandArgs(trailingOnly = TRUE)

argument <- function(name, default = NULL) {
  index <- match(name, args)
  if (is.na(index)) return(default)
  if (index == length(args)) stop("Valor ausente para ", name)
  args[[index + 1L]]
}

input_arg <- argument("--input")
output_arg <- argument("--output")
if (is.null(input_arg) || is.null(output_arg)) {
  stop("Uso: Rscript render_report.R --input caminho/relatorio.Rmd --output caminho/relatorio.html")
}

script_flag <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_flag) != 1L) stop("Não foi possível localizar o diretório da skill.")
script_path <- normalizePath(sub("^--file=", "", script_flag), winslash = "/", mustWork = TRUE)
skill_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

input_path <- normalizePath(input_arg, winslash = "/", mustWork = TRUE)
output_path <- normalizePath(output_arg, winslash = "/", mustWork = FALSE)
template_path <- normalizePath(
  argument("--template", file.path(skill_dir, "assets", "technical-report.html")),
  winslash = "/",
  mustWork = TRUE
)
css_path <- normalizePath(
  argument("--css", file.path(skill_dir, "assets", "technical-report.css")),
  winslash = "/",
  mustWork = TRUE
)

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("Pacote rmarkdown ausente.")
}

if (!rmarkdown::pandoc_available()) {
  candidates <- c(
    Sys.getenv("RSTUDIO_PANDOC", unset = ""),
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/RStudio/bin/pandoc"
  )
  candidates <- candidates[nzchar(candidates) & file.exists(file.path(candidates, "pandoc.exe"))]
  if (length(candidates) == 0L) {
    stop("Pandoc não localizado. Defina RSTUDIO_PANDOC ou instale RStudio/Pandoc.")
  }
  Sys.setenv(RSTUDIO_PANDOC = candidates[[1L]])
}

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

rmarkdown::render(
  input = input_path,
  output_file = basename(output_path),
  output_dir = dirname(output_path),
  output_options = list(
    self_contained = TRUE,
    toc = TRUE,
    toc_float = FALSE,
    number_sections = TRUE,
    theme = NULL,
    math_method = "mathml",
    template = template_path,
    css = css_path
  ),
  envir = new.env(parent = globalenv()),
  clean = TRUE,
  quiet = FALSE
)

rendered_lines <- readLines(output_path, encoding = "UTF-8", warn = FALSE)
rendered_lines <- sub("[[:blank:]]+$", "", rendered_lines)
writeLines(rendered_lines, output_path, useBytes = TRUE)

message("Relatório renderizado em ", output_path)
