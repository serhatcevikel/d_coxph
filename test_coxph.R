#' ----------------------------------------------------------------------------
#' title: test_coxph.R
#' description:
#'   Unit tests for the distributed Cox Proportional Hazards algorithm.
#'   Tests in this file should not be run directly. Instead the file
#'   'run_tests.R' should be sourced/run.
#' author:
#'   Melle Sieswerda <m.sieswerda@iknl.nl>
#'   Anna van der Zalm <a.vanderzalm@iknl.nl>
#'   Gijs Geleijnse <g.geleijnse@iknl.nl>
#' date: 09-may-2018
#' license: MIT License
#' ----------------------------------------------------------------------------

library(testthat)

# Source the R file that contains the functions to test
source("dl_coxph.R")

#' Test to ensure the distributed implementation returns the same result
#' as the regular CoxPH algorithm when run with a single site.
test_that("test single site", {
  digits <- 5
  df <- read.csv("SeerMetHeader.csv")

  # Run the regular implementation
  cox_result <- coxph(
    Surv(df$Time, df$Censor) ~ Age + Race2 + Race3 + Mar2 + Mar3 + Mar4 + Mar5 + Mar9 + Hist8520 + hist8522 + hist8480 + hist8501 + hist8201 + hist8211 + grade + ts + nne + npn + er2 + er4,
    method="breslow",
    df
  )

  cox_coefficients <- round(cox_result$coefficients, digits)

  expl_vars <- c("Age","Race2","Race3","Mar2","Mar3","Mar4","Mar5","Mar9","Hist8520","hist8522","hist8480","hist8501","hist8201","hist8211","grade","ts","nne","npn","er2","er4")
  time_col <- "Time"
  censor_col <- "Censor"

  dlcox_result <- mock(df, expl_vars, time_col, censor_col, splits=1)
  dlcox_coefficients <- round(dlcox_result$coef, digits)
  names(dlcox_coefficients) <- rownames(dlcox_result)

  expect_equal(dlcox_coefficients, cox_coefficients)
})


#' Test to ensure the distributed implementation returns the same result
#' as the regular CoxPH algorithm when run with multiple sites.
test_that("test 5 sites", {
  digits <- 5
  df <- read.csv("SeerMetHeader.csv")

  # Run the regular implementation
  cox_result <- coxph(
    Surv(df$Time, df$Censor) ~ Age + Race2 + Race3 + Mar2 + Mar3 + Mar4 + Mar5 + Mar9 + Hist8520 + hist8522 + hist8480 + hist8501 + hist8201 + hist8211 + grade + ts + nne + npn + er2 + er4,
    method="breslow",
    df
  )

  cox_coefficients <- round(cox_result$coefficients, digits)

  expl_vars <- c("Age","Race2","Race3","Mar2","Mar3","Mar4","Mar5","Mar9","Hist8520","hist8522","hist8480","hist8501","hist8201","hist8211","grade","ts","nne","npn","er2","er4")
  time_col <- "Time"
  censor_col <- "Censor"

  dlcox_result <- mock(df, expl_vars, time_col, censor_col, splits=5)
  dlcox_coefficients <- round(dlcox_result$coef, digits)
  names(dlcox_coefficients) <- rownames(dlcox_result)

  expect_equal(dlcox_coefficients, cox_coefficients)
})
