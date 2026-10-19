source("R/pns_core.R",encoding="UTF-8")
root <- pns_root()
target <- file.path(pns_cache(),"entrypoint-validation")
for(locale in c("pt","en")) {
  Sys.setenv(PNS_OUTPUT_DIR=file.path(target,locale))
  files <- list.files(file.path(root,if(locale=="pt") "Codes (PT-BR)" else "Codes (EN)"),pattern="\\.R$",full.names=TRUE)
  stopifnot(length(files)==7)
  for(file in files) {
    source(file,encoding="UTF-8")
    cat("Executed ",basename(file),"\n",sep="")
  }
}
Sys.unsetenv("PNS_OUTPUT_DIR")
for(locale in c("pt","en")) for(group in names(groups)) {
  a <- read.csv(file.path(target,locale,paste0(group,".csv")),na.strings="")
  b <- read.csv(file.path(pns_cache(),"release",locale,paste0(group,".csv")),na.strings="")
  stopifnot(identical(a,b))
}
cat("All 14 entry points reproduce the canonical release: PASS\n")
