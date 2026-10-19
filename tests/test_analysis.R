source("R/pns_core.R",encoding="UTF-8")
library(testthat)
test_that("age, codes, uncertain OR and days preserve missingness", {
  expect_equal(valid_age(c(15,17,18,130,131,NA)),c(FALSE,FALSE,TRUE,TRUE,FALSE,FALSE))
  expect_equal(yes_no(factor(c("1","2","9"))),c(1,0,NA))
  expect_equal(known_or(c(1,NA,0,0,NA),c(NA,1,0,NA,NA)),c(1,1,0,NA,NA))
  expect_equal(valid_days(c(0,1,14,15,99,NA)),c(NA,1,14,NA,NA,NA))
})
test_that("income amounts distinguish structural zero, unknown, factor and sentinel", {
  expect_equal(amount(factor(c("100","99999999",NA)),c(TRUE,TRUE,FALSE),c(FALSE,FALSE,TRUE)),c(100,NA,0))
  expect_equal(amount(c(100,NA),c(FALSE,FALSE),c(TRUE,FALSE)),c(NA_real_,NA_real_))
  expect_equal(amount(99999999,TRUE,FALSE,ignored=numeric()),99999999)
  expect_equal(income_total(data.frame(a=c(1,0,1),b=c(2,0,NA))),c(3,0,NA))
})
income_fixture <- function(n=1) {
  vars <- c("VDE001","VDE002","E011","E01401","E01601","E01602","E01603","E01604","E01605","E01801","E01802","E01803","E01804","E01805","F001011","F001021","F007011","F007021","F008011","F008021","VDF001","VDF00102")
  z <- as.data.frame(setNames(rep(list(rep(NA_real_,n)),length(vars)),vars))
  z$VDE001 <- 2; z$F001011 <- 2; z$F007011 <- 2; z$F008011 <- 2; z$VDF001 <- 2; z
}
test_that("income retains cash, in-kind and supplementary sources", {
  z <- income_fixture(5)
  z$VDE001[1:4] <- 1; z$VDE002[1:4] <- 1; z$E011[1:4] <- 1
  z$E01601[1:3] <- 1; z$E01602[1:2] <- c(100,200)
  z$E01603[1] <- 2; z$E01604[1] <- 50
  z$VDF001[1] <- 1; z$VDF00102[1] <- 30
  z$E01401[4] <- 7
  expect_equal(income_total(income_components(z)),c(180,200,NA,0,0))
  z$E01603[2] <- 9
  expect_true(is.na(income_total(income_components(z))[2]))
  z$E011[2] <- 2
  expect_true(is.na(income_total(income_components(z))[2]))
})
test_that("questionnaire routing, respiratory codes and monetary factors are explicit", {
  z <- income_fixture(5)
  z$C008 <- c(17,18,50,40,60); z$Q074 <- c(1,1,1,1,2); z$Q076 <- c(1,1,2,1,NA)
  z$Q07601 <- c(1,1,NA,2,NA); z$Q07704 <- c(1,3,NA,NA,NA); z$Q07708 <- c(NA,2,NA,NA,NA)
  z$Q07707 <- NA; z$Q07711 <- NA; z$P050 <- c(1,2,3,9,NA)
  z$J002 <- c(1,1,1,2,1); z$J003 <- c(1,14,99,NA,4); z$J00402 <- c(5,1,99,NA,14)
  z$Deflator <- 2
  z$F001011 <- 1; z$F001021 <- 100
  p <- prepare_variables(z,2019,factor=3)
  expect_equal(p$med_eligible,c(TRUE,TRUE,FALSE,FALSE,FALSE))
  expect_equal(p$medication,c(1,0,NA,NA,NA))
  expect_equal(p$respiratory,c(1,0,NA,NA,0))
  expect_equal(p$income_202508,rep(600,5))
  z$J004 <- c(10,11,12,NA,24); z$Q07701 <- c(1,2,NA,1,NA)
  p13 <- prepare_variables(z,2013,factor=3)
  expect_equal(p13$respiratory,c(1,1,0,NA,0))
  expect_equal(p13$med_eligible,c(TRUE,TRUE,FALSE,TRUE,FALSE))
})
test_that("calibrated domains agree with direct weighted reference estimates", {
  z <- data.frame(UPA_PNS=rep(1:8,each=2),V0024=rep(1:2,each=8),V00293=rep(1:2,each=8),V00292=160,V0029=1,V00291=20,adult=TRUE,V0001=11,x=rep(c(0,1,NA,1),4),dom=rep(c(TRUE,TRUE,FALSE,FALSE),4))
  des <- make_design(z)
  a <- estimate_one(des,"test","x","dom",2019,"BR")
  expect_equal(a$estimate,.5,tolerance=1e-12); expect_equal(a$n_valid,8)
  expect_equal(a$weighted_valid,160)
  des$variables$x <- 0
  expect_equal(estimate_one(des,"test","x","dom",2019,"BR")$status,"boundary")
  des$variables$dom <- FALSE
  expect_equal(estimate_one(des,"test","x","dom",2019,"BR")$status,"empty")
  bad <- z; bad$V00292[1] <- 100
  expect_error(make_design(bad),"Conflicting")
})
cat("Synthetic analytical validation PASS\n")
