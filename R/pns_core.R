# Canonical analysis. Questionnaire codes, not translated labels, define events.
options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
pns_root <- function() {
  roots <- unique(c(Sys.getenv("PNS_REPO_ROOT"), getwd(), dirname(getwd())))
  hit <- roots[file.exists(file.path(roots, "R", "pns_core.R"))]
  if (!length(hit)) stop("Run from the repository root or set PNS_REPO_ROOT")
  normalizePath(hit[1], winslash = "/")
}
pns_cache <- function() {
  default <- file.path(if (.Platform$OS.type == "windows") Sys.getenv("LOCALAPPDATA") else path.expand("~/.cache"), "PNS_2013_2019", "cache")
  path <- Sys.getenv("PNS_CACHE_DIR", default)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/")
}
num <- function(x) if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
yes_no <- function(x, yes = 1, no = 2) {
  x <- num(x); ifelse(x %in% yes, 1, ifelse(x %in% no, 0, NA_real_))
}
known_or <- function(a, b) {
  ifelse((!is.na(a) & a == 1) | (!is.na(b) & b == 1), 1,
         ifelse(!is.na(a) & !is.na(b) & a == 0 & b == 0, 0, NA_real_))
}
valid_age <- function(x) !is.na(x) & x >= 18 & x <= 130
valid_days <- function(x) ifelse(!is.na(x) & x %in% 1:14, x, NA_real_)
uf_names <- c(`11`="Rondônia", `12`="Acre", `13`="Amazonas", `14`="Roraima", `15`="Pará", `16`="Amapá", `17`="Tocantins", `21`="Maranhão", `22`="Piauí", `23`="Ceará", `24`="Rio Grande do Norte", `25`="Paraíba", `26`="Pernambuco", `27`="Alagoas", `28`="Sergipe", `29`="Bahia", `31`="Minas Gerais", `32`="Espírito Santo", `33`="Rio de Janeiro", `35`="São Paulo", `41`="Paraná", `42`="Santa Catarina", `43`="Rio Grande do Sul", `50`="Mato Grosso do Sul", `51`="Mato Grosso", `52`="Goiás", `53`="Distrito Federal")
source_files <- function(year, cache = pns_cache()) {
  d <- file.path(cache, year)
  list.files(d, pattern = "\\.(zip|xls|txt)$", full.names = TRUE)[!grepl("dictionary.txt$", list.files(d, pattern = "\\.(zip|xls|txt)$", full.names = TRUE))]
}
file_hashes <- function(paths) setNames(vapply(paths, digest::digest, character(1), file=TRUE, algo="sha256"), basename(paths))
load_pns <- function(year) {
  # PNSIBGE 0.2.1 internally looks up its functions on the attached search path.
  suppressPackageStartupMessages(library(PNSIBGE))
  d <- file.path(pns_cache(), year); dir.create(d, showWarnings=FALSE)
  out <- file.path(d, "selected-raw-v2.rds")
  if (file.exists(out)) {
    x <- readRDS(out)
    if (identical(attr(x,"source_hashes"), file_hashes(source_files(year)))) return(x)
  }
  # Select only analysis fields; PNSIBGE also imports the design/selection fields.
  common <- c("V0001","V0020","V0024","UPA_PNS","V0006_PNS","C00301","C008","M001","V0029","V00291","V00292","V00293","Q074","Q076","J002","J003","P050")
  extra <- if(year == 2013) c("Q07701","J004") else c("Q07601","Q07704","Q07707","Q07708","Q07711","J00402","E011","E01401","E01601","E01602","E01603","E01604","E01605","E01801","E01802","E01803","E01804","E01805","F001011","F001021","F007011","F007021","F008011","F008021","VDF001","VDF00102","VDE001","VDE002")
  x <- PNSIBGE::get_pns(year, vars=c(common,extra), selected=TRUE, anthropometry=FALSE, labels=FALSE, design=FALSE, deflator=TRUE, reload=FALSE, savedir=d)
  if (!is.data.frame(x) || nrow(x)<10000 || !all(c(common,extra,"Deflator") %in% names(x))) stop("Incomplete PNS input: ", year)
  x <- as.data.frame(lapply(x, num)); stopifnot(all(x$M001 == 1))
  attr(x,"source_hashes") <- file_hashes(source_files(year))
  saveRDS(x, out); x
}
# A missing amount is zero only when a recorded route proves non-receipt.
amount <- function(value, receipt, absent, ignored = 99999999) {
  value <- num(value)
  valid <- !is.na(value) & is.finite(value) & value >= 0 & !(value %in% ignored)
  out <- rep(NA_real_,length(value))
  out[receipt %in% TRUE & valid] <- value[receipt %in% TRUE & valid]
  # Contradictory positive amounts are not silently discarded.
  out[absent %in% TRUE & (is.na(value) | (valid & value == 0))] <- 0
  out
}
income_components <- function(d) {
  no_job <- d$VDE001 %in% 2 | d$VDE002 %in% 2
  unpaid_main <- d$E01401 %in% 7
  one_job <- d$E011 %in% 1
  main_cash <- d$E01601 %in% 1; main_kind <- d$E01603 %in% 2
  other_cash <- d$E01801 %in% 1; other_kind <- d$E01803 %in% 2
  main_benefit <- d$E01605 %in% 1:3; other_benefit <- d$E01805 %in% 1:3
  # Checkboxes without a yes/no code: a blank unselected modality is a
  # structural zero only if another modality was explicitly selected.
  absent_checkbox <- function(field, alternative, route) route | (is.na(field) & alternative)
  x <- data.frame(
    E01602=amount(d$E01602, main_cash, absent_checkbox(d$E01601,main_kind|main_benefit,no_job|unpaid_main)),
    E01604=amount(d$E01604, main_kind, absent_checkbox(d$E01603,main_cash|main_benefit,no_job|unpaid_main)),
    E01802=amount(d$E01802, other_cash, absent_checkbox(d$E01801,other_kind|other_benefit,no_job|one_job)),
    E01804=amount(d$E01804, other_kind, absent_checkbox(d$E01803,other_cash|other_benefit,no_job|one_job)),
    F001021=amount(d$F001021,d$F001011 %in% 1,d$F001011 %in% 2),
    F007021=amount(d$F007021,d$F007011 %in% 1,d$F007011 %in% 2),
    F008021=amount(d$F008021,d$F008011 %in% 1,d$F008011 %in% 2),
    # Unlike the seven preceding amounts, VDF00102 has no documented
    # 99999999 missing-value code in the official 2019 dictionary.
    VDF00102=amount(d$VDF00102,d$VDF001 %in% 1,d$VDF001 %in% 2,ignored=numeric())
  )
  x
}
income_total <- function(x) rowSums(x, na.rm=FALSE)
ipca_factor <- function(root=pns_root()) {
  x <- jsonlite::fromJSON(file.path(root,"data","ipca_reference.json"))
  stopifnot(x$base_period=="201907", x$target_period=="202508")
  x$target_index / x$base_index
}
prepare_variables <- function(d, year, factor=ipca_factor()) {
  d$adult <- valid_age(d$C008)
  d$asthma <- yes_no(d$Q074); d$attack <- yes_no(d$Q076)
  d$smoking <- yes_no(d$P050, c(1,2), 3)
  d$restricted <- yes_no(d$J002); d$days <- valid_days(d$J003)
  d$diagnosed <- d$Q074 %in% 1
  d$recent_attack <- d$diagnosed & d$Q076 %in% 1
  if(year==2013) {
    d$med_eligible <- d$recent_attack
    d$medication <- yes_no(d$Q07701)
    d$cause <- ifelse(d$J004 %in% 1:24,d$J004,NA_real_)
    d$respiratory <- ifelse(is.na(d$cause),NA_real_,as.numeric(d$cause %in% c(10,11)))
  } else {
    d$med_eligible <- d$recent_attack & d$Q07601 %in% 1
    d$oral <- yes_no(d$Q07704,c(1,2),3); d$inhaler <- yes_no(d$Q07708)
    d$medication <- known_or(d$oral,d$inhaler)
    d$pay_oral <- yes_no(d$Q07707); d$pay_inhaler <- yes_no(d$Q07711)
    d$oral_eligible <- d$med_eligible & d$oral %in% 1
    d$inhaler_eligible <- d$med_eligible & d$inhaler %in% 1
    d$cause <- ifelse(d$J00402 %in% 1:14,d$J00402,NA_real_)
    d$respiratory <- ifelse(is.na(d$cause),NA_real_,as.numeric(d$cause==5))
    parts <- income_components(d)
    d$income_nominal <- income_total(parts)
    d$income_201907 <- d$income_nominal * d$Deflator
    d$income_202508 <- d$income_201907 * factor
    d$income_missing <- as.numeric(is.na(d$income_nominal))
    d$income_components_missing <- rowSums(is.na(parts))
  }
  d$restricted_domain <- d$J002 %in% 1
  d$respiratory_domain <- d$restricted_domain & d$respiratory %in% 1
  d
}
make_design <- function(d, check_weights=TRUE) {
  keys <- c("V0029","V00291","V00292","V00293","V0024","UPA_PNS")
  stopifnot(!anyNA(d[keys]), all(d$V0029>0), all(d$V00291>0))
  pop <- unique(d[c("V00293","V00292")]); names(pop)[2] <- "Freq"
  if(anyDuplicated(pop$V00293)) stop("Conflicting calibration totals")
  prior <- survey::svydesign(ids=~UPA_PNS,strata=~V0024,weights=~V0029,data=d,nest=TRUE)
  des <- survey::postStratify(prior,~V00293,pop)
  rel <- max(abs(weights(des)-d$V00291)/pmax(1,d$V00291))
  # Published weights have finite decimal precision.
  if(check_weights && rel > 1e-6) stop("Official final weights disagree: ", rel)
  recovered <- tapply(weights(des), d$V00293, sum)
  target <- setNames(pop$Freq,pop$V00293)[names(recovered)]
  err <- max(abs(recovered-target)/pmax(1,target))
  if(err > 1e-10) stop("Calibration totals not recovered")
  attr(des,"validation") <- list(max_weight_relative_error=rel,max_total_relative_error=err,selected_n=nrow(d),adult_n=sum(d$adult),excluded_under18=sum(d$C008<18,na.rm=TRUE),invalid_age=sum(is.na(d$C008)|d$C008<0|d$C008>130))
  des
}
estimate_one <- function(des, indicator, variable, domain, year, geo, kind="proportion") {
  d <- des$variables
  eligible <- d$adult & (if(domain=="adult") TRUE else d[[domain]] %in% TRUE) & (if(geo=="BR") TRUE else d$V0001==as.numeric(geo))
  valid <- eligible & !is.na(d[[variable]])
  w <- weights(des)
  n <- sum(valid)
  psu_pairs <- unique(d[valid,c("V0024","UPA_PNS")])
  lonely <- if(n) sum(table(psu_pairs$V0024)==1) else 0L
  out <- data.frame(indicator=indicator,year=year,geo=geo,domain=domain,kind=kind,estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,n_eligible=sum(eligible),n_valid=n,n_missing=sum(eligible)-n,n_event=if(kind=="proportion") sum(d[[variable]][valid]==1) else NA_integer_,weighted_eligible=sum(w[eligible]),weighted_valid=sum(w[valid]),weighted_missing=sum(w[eligible & !valid]),psu_n=length(unique(d$UPA_PNS[valid])),strata_n=length(unique(d$V0024[valid])),df=NA_real_,cv=NA_real_,status="empty",ci_method="none",stringsAsFactors=FALSE)
  out$lonely_strata_n <- lonely
  # The survey package warns repeatedly about this same domain condition.
  # Record it per result; unexpected warnings remain errors requiring review.
  checked <- function(expr) withCallingHandlers(expr,warning=function(w) {
    if(grepl("strata have only one PSU in this subset",conditionMessage(w),fixed=TRUE)) invokeRestart("muffleWarning") else stop(w)
  })
  if(!n) return(out)
  sub <- checked(des[valid,]); sub$variables$.value <- sub$variables[[variable]]
  # Calibrated survey subsets may retain out-of-domain rows with zero weight.
  est <- checked(survey::svymean(~.value,sub,na.rm=TRUE))
  out$estimate <- as.numeric(coef(est)); out$se <- as.numeric(survey::SE(est)); out$df <- survey::degf(sub)
  out$cv <- if(out$estimate!=0) out$se/abs(out$estimate) else NA_real_
  if(kind=="proportion" && out$estimate %in% c(0,1)) {out$status <- "boundary"; return(out)}
  if(out$df<=0 || !is.finite(out$se) || out$se<=0) {out$status <- "variance_unavailable"; return(out)}
  if(kind=="proportion") {
    ci <- checked(as.numeric(confint(survey::svyciprop(~.value,sub,method="beta",level=.95,na.rm=TRUE))))
    out$ci_method <- "beta"
  } else {
    ci <- out$estimate + c(-1,1)*qt(.975,out$df)*out$se
    out$ci_method <- "t"
  }
  if(length(ci)!=2 || any(!is.finite(ci))) {out$status <- "interval_unavailable"; return(out)}
  out$lower <- ci[1]; out$upper <- ci[2]; out$status <- "ok"; out
}
compute_year <- function(year, factor=ipca_factor()) {
  d <- prepare_variables(load_pns(year),year,factor)
  des <- make_design(d)
  specs <- list(c("asthma_diagnosis","asthma","adult"), c("asthma_attack","attack","diagnosed"), c("asthma_medication_conditional","medication","med_eligible"),c("current_smoking","smoking","adult"),c("activity_restriction","restricted","adult"),c("restricted_days","days","restricted_domain","mean"),c("respiratory_share","respiratory","restricted_domain"),c("respiratory_days","days","respiratory_domain","mean"))
  if(year==2019) {
    specs <- c(specs,list(c("payment_oral","pay_oral","oral_eligible"),c("payment_inhaler","pay_inhaler","inhaler_eligible"),c("income_composed","income_202508","adult","mean"),c("income_incomplete","income_missing","adult")))
    for(k in 1:14) {
      v <- paste0("cause_",k); d[[v]] <- ifelse(is.na(d$cause),NA_real_,as.numeric(d$cause==k))
      dm <- paste0(v,"_domain"); d[[dm]] <- d$restricted_domain & d$cause %in% k
      des$variables[[v]] <- d[[v]]; des$variables[[dm]] <- d[[dm]]
      specs <- c(specs,list(c(paste0(v,"_share"),v,"restricted_domain"),c(paste0(v,"_days"),"days",dm,"mean")))
    }
  }
  rows <- list()
  for(s in specs) {
    message(year," ",s[1])
    geos <- if(startsWith(s[1],"cause_")) "BR" else c(names(uf_names),"BR")
    for(g in geos) rows[[length(rows)+1]] <- estimate_one(des,s[1],s[2],s[3],year,g,if(length(s)==4) s[4] else "proportion")
  }
  list(results=do.call(rbind,rows),validation=attr(des,"validation"))
}
analyze_year <- function(year, factor=ipca_factor()) {
  raw <- load_pns(year)
  versions <- vapply(c("survey","PNSIBGE"),function(p) as.character(packageVersion(p)),character(1))
  key <- digest::digest(list(file_hashes(file.path(pns_root(),"R","pns_core.R")),attr(raw,"source_hashes"),factor,R.version.string,versions),algo="sha256")
  file <- file.path(pns_cache(),year,paste0("estimates-",key,".rds"))
  if(file.exists(file)) return(readRDS(file))
  x <- compute_year(year,factor); saveRDS(x,file); x
}
make_contrasts <- function(x) {
  a <- subset(x, year==2013 & indicator %in% c("asthma_diagnosis","asthma_attack","current_smoking"))
  b <- subset(x, year==2019 & indicator %in% a$indicator)
  z <- merge(a,b,by=c("indicator","geo"),suffixes=c("_2013","_2019"))
  v1 <- z$se_2013^2; v2 <- z$se_2019^2
  df <- (v1+v2)^2/(v1^2/z$df_2013+v2^2/z$df_2019)
  delta <- 100*(z$estimate_2019-z$estimate_2013); se <- 100*sqrt(v1+v2)
  valid <- z$status_2013=="ok" & z$status_2019=="ok" & is.finite(df) & df>0
  margin <- ifelse(valid,qt(.975,df)*se,NA_real_)
  data.frame(indicator=z$indicator,geo=z$geo,difference_pp=delta,se_pp=se,lower_pp=delta-margin,upper_pp=delta+margin,df=df,status=ifelse(valid,"ok","unavailable"),assumption="independent_editions",analysis=ifelse(z$geo=="BR","national","exploratory"))
}
groups <- list(asthma="asthma_diagnosis",attacks="asthma_attack",medication="asthma_medication_conditional",smoking="current_smoking",payment=c("payment_oral","payment_inhaler"),activity=c("activity_restriction","restricted_days","respiratory_share","respiratory_days",paste0("cause_",rep(1:14,each=2),rep(c("_share","_days"),14))),income=c("income_composed","income_incomplete"))
export_group <- function(x, group, locale, destination) {
  z <- x[x$indicator %in% groups[[group]],]
  z$UF <- ifelse(z$geo=="BR",if(locale=="pt") "Brasil" else "Brazil",uf_names[z$geo])
  dir.create(destination,recursive=TRUE,showWarnings=FALSE)
  # Stable machine-readable field names and IDs across languages, schema v2.
  write.csv(z,file.path(destination,paste0(group,".csv")),row.names=FALSE,na="",fileEncoding="UTF-8")
  writexl::write_xlsx(z,file.path(destination,paste0(group,".xlsx")))
}
run_group <- function(group, locale="pt") {
  years <- if(group %in% c("payment","income")) 2019 else c(2013,2019)
  x <- do.call(rbind,lapply(years,function(y) analyze_year(y)$results))
  dest <- Sys.getenv("PNS_OUTPUT_DIR",file.path(pns_root(),paste0("Spreadsheets (",if(locale=="pt") "PT-BR" else "EN",")")))
  export_group(x,group,locale,dest)
  invisible(x[x$indicator %in% groups[[group]],])
}
