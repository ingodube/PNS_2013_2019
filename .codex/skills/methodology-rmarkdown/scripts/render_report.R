args <- commandArgs(trailingOnly = TRUE)

if (.Platform$OS.type == "windows") {
  locale_result <- suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
  if (!nzchar(locale_result)) stop("A UTF-8 Windows locale is required to render the report safely.")
}

argument <- function(name, default = NULL) {
  index <- match(name, args)
  if (is.na(index)) return(default)
  if (index == length(args)) stop("Missing value for ", name)
  args[[index + 1L]]
}

input_arg <- argument("--input")
output_arg <- argument("--output")
if (is.null(input_arg) || is.null(output_arg)) {
  stop("Usage: Rscript render_report.R --input path/report.Rmd --output path/report.html")
}

script_flag <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_flag) != 1L) stop("The skill directory could not be located.")
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
  stop("The rmarkdown package is not installed.")
}

if (!rmarkdown::pandoc_available()) {
  candidates <- c(
    Sys.getenv("RSTUDIO_PANDOC", unset = ""),
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/RStudio/bin/pandoc"
  )
  candidates <- candidates[nzchar(candidates) & file.exists(file.path(candidates, "pandoc.exe"))]
  if (length(candidates) == 0L) {
    stop("Pandoc was not found. Set RSTUDIO_PANDOC or install RStudio/Pandoc.")
  }
  Sys.setenv(RSTUDIO_PANDOC = candidates[[1L]])
}

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

repo_candidates <- unique(normalizePath(
  c(getwd(), dirname(input_path), dirname(dirname(input_path))),
  winslash = "/",
  mustWork = FALSE
))
repo_root <- repo_candidates[file.exists(file.path(repo_candidates, "README.md"))][1L]
if (is.na(repo_root)) stop("The repository root could not be located.")
Sys.setenv(REPORT_REPO_ROOT = repo_root)

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
output_connection <- file(output_path, open = "wb")
writeLines(rendered_lines, output_connection, sep = "\n", useBytes = TRUE)
close(output_connection)

message("Report rendered to ", output_path)
