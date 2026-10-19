# Presentation only. Both reports consume the same validated numerical inputs.
language <- params$language
L <- function(pt,en) if(language=="pt") pt else en
root <- Sys.getenv("REPORT_REPO_ROOT",unset="")
if(!nzchar(root)) root <- if(file.exists("R/pns_core.R")) getwd() else dirname(getwd())
results_dir <- Sys.getenv("PNS_RESULTS_DIR",file.path(root,"data"))
results <- read.csv(file.path(results_dir,"estimates.csv"),na.strings="",fileEncoding="UTF-8")
contrasts <- read.csv(file.path(results_dir,"contrasts.csv"),na.strings="",fileEncoding="UTF-8")
manifest <- jsonlite::fromJSON(file.path(results_dir,"analysis_manifest.json"))
stopifnot(manifest$schema_version==2)
source(file.path(root,"R","pns_core.R"),encoding="UTF-8")
stopifnot(identical(unname(file_hashes(file.path(root,"R","pns_core.R"))),manifest$analysis_sha256))
stopifnot(identical(unname(file_hashes(file.path(results_dir,"estimates.csv"))),manifest$output_sha256$estimates.csv))
fmt <- function(x,d=2) ifelse(is.na(x),L("não estimável","not estimable"),formatC(x,format="f",digits=d,decimal.mark=L(",",".")))
geo_label <- function(x) ifelse(x=="BR",L("Brasil","Brazil"),uf_names[as.character(x)])
value_ci <- function(z) {
  scale <- ifelse(z$kind=="proportion",100,1)
  unit <- ifelse(z$kind=="proportion","%","")
  v <- paste0(fmt(z$estimate*scale),unit," [",fmt(z$lower*scale),unit,"; ",fmt(z$upper*scale),unit,"]")
  v[z$status=="boundary"] <- paste0(fmt(z$estimate[z$status=="boundary"]*scale[z$status=="boundary"]),unit[z$status=="boundary"],L(" [IC não estimável]"," [CI not estimable]"))
  v
}
caption <- function(text,id,figure=FALSE) cat('<p id="',id,'" class="',if(figure) 'figure-title' else 'table-title','">',text,'</p>\n',sep="")
table_html <- function(z,title,id) {
  cat('<div class="table-scroll" role="region" aria-label="',L("Tabela com rolagem horizontal","Horizontally scrollable table"),'">\n',sep="")
  h <- knitr::kable(z,format="html",caption=title,table.attr='class="report-table"',row.names=FALSE)
  cat(sub("<caption>",paste0('<caption id="',id,'">'),h,fixed=TRUE),"\n</div>\n")
  cat('<p class="table-source">',L("Fonte: cálculos dos autores com microdados oficiais da PNS.","Source: authors' calculations using official PNS microdata."),'</p>\n',sep="")
}
pick <- function(ids,national=FALSE) {
  z <- results[results$indicator %in% ids & (!national | results$geo=="BR"),]
  z <- z[order(z$indicator,z$geo=="BR",geo_label(z$geo),z$year),]; z
}
show_results <- function(ids,title,id,national=FALSE,labels=NULL) {
  z <- pick(ids,national)
  a <- data.frame(UF=geo_label(z$geo),year=z$year,value=value_ci(z),n=z$n_valid,missing=fmt(100*z$weighted_missing/z$weighted_eligible))
  names(a) <- c("UF",L("Ano","Year"),L("Estimativa [IC 95%]","Estimate [95% CI]"),L("n válido","Valid n"),L("Ausentes (%)","Missing (%)"))
  if(!is.null(labels)) a <- cbind(setNames(data.frame(unname(labels[z$indicator])),L("Indicador","Indicator")),a)
  table_html(a,title,id)
}
plot_results <- function(ids,axis_label,national=FALSE,labels=NULL) {
  z <- pick(ids,national)
  z$unit <- if(is.null(labels)) geo_label(z$geo) else unname(labels[z$indicator])
  z$unit <- factor(z$unit,levels=rev(unique(z$unit)))
  scale <- ifelse(z$kind=="proportion",100,1)
  z$value <- z$estimate*scale; z$lo <- z$lower*scale; z$hi <- z$upper*scale
  z$year <- factor(z$year)
  print(ggplot2::ggplot(z,ggplot2::aes(x=unit,y=value,color=year))+
    ggplot2::geom_point(position=ggplot2::position_dodge(width=.5),size=2,na.rm=TRUE)+
    ggplot2::geom_errorbar(ggplot2::aes(ymin=lo,ymax=hi),position=ggplot2::position_dodge(width=.5),width=.25,na.rm=TRUE)+
    ggplot2::coord_flip()+ggplot2::scale_color_manual(values=c(`2013`="#655fba",`2019`="#9b702a"))+
    ggplot2::labs(x=NULL,y=axis_label,color=L("Ano","Year"))+
    ggplot2::theme_minimal(base_size=12)+ggplot2::theme(legend.position="top",panel.grid.major.y=ggplot2::element_blank()))
}
national_summary <- function(id) {
  z <- pick(id,TRUE)
  cat(paste(paste0(z$year,": ",value_ci(z),"; n = ",z$n_valid),collapse=". "),".\n\n",sep="")
}
cause_pt <- c("Ossos e articulações","Cefaleia/enxaqueca","Gineco-obstétricos","Odontológicos","Respiratórios","Gastrointestinais","Arboviroses","Cardiovasculares","Diabetes","Câncer","Neurológicos","Saúde mental","Lesões/fraturas","Outros")
cause_en <- c("Bones and joints","Headache/migraine","Gynaecological/obstetric","Dental","Respiratory","Gastrointestinal","Arboviruses","Cardiovascular","Diabetes","Cancer","Neurological","Mental health","Injuries/fractures","Other")
cause_labels <- function(suffix) setNames(L(cause_pt,cause_en),paste0("cause_",1:14,suffix))
