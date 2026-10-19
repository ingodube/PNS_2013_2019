source("R/pns_core.R",encoding="UTF-8")
args <- commandArgs(trailingOnly=TRUE)
stage <- if(length(args)) args[1] else file.path(pns_cache(),"release")
x <- read.csv(file.path(stage,"estimates.csv"),na.strings="")
checks <- list()
for(year in c(2013,2019)) {
  d <- prepare_variables(load_pns(year),year)
  des <- make_design(d)
  specs <- list(c("asthma_diagnosis","asthma","adult"),c("asthma_attack","attack","diagnosed"),c("current_smoking","smoking","adult"))
  for(s in specs) {
    valid <- d$adult & !is.na(d[[s[2]]]) & (if(s[3]=="adult") TRUE else d[[s[3]]])
    direct <- sum(d$V00291[valid]*d[[s[2]]][valid])/sum(d$V00291[valid])
    # Explicit indexing avoids non-standard evaluation of the year name.
    got <- x[x$indicator==s[1] & x$year==year & x$geo=="BR",]
    stopifnot(nrow(got)==1,abs(direct-got$estimate)<1e-8)
    # Independently construct a final-weight design, then represent calibration.
    ref <- survey::svydesign(ids=~UPA_PNS,strata=~V0024,weights=~V00291,data=d,nest=TRUE)
    pop <- unique(d[c("V00293","V00292")]); names(pop)[2] <- "Freq"
    ref <- survey::postStratify(ref,~V00293,pop)
    sub <- ref[valid,]; sub$variables$reference_value <- sub$variables[[s[2]]]
    estimate <- survey::svymean(~reference_value,sub,na.rm=TRUE)
    stopifnot(abs(as.numeric(survey::SE(estimate))-got$se)<1e-8)
    checks[[length(checks)+1]] <- data.frame(year=year,indicator=s[1],direct_weighted_mean=direct,reported=got$estimate,se_reference=as.numeric(survey::SE(estimate)),se_reported=got$se)
  }
}
write.csv(do.call(rbind,checks),file.path(stage,"independent_checks.csv"),row.names=FALSE)
cat("Independent full-data point and variance checks PASS\n")
