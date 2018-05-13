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
library(survival)

# Source the R file that contains the functions to test
# source("dl_coxph.R")
source("run_coxph.R")

#' Test to ensure the distributed implementation returns the same result
#' as the regular CoxPH algorithm when run with a single site.
test_that("test single site", {
  # Specify the number of digits behind the comma that need to match
  # to consider a float equal
  digits <- 5

  # Run the regular implementation
  df <- read.csv("SeerMetHeader.csv")
  cox_result <- coxph(
    Surv(df$Time, df$Censor) ~ Age + Race2 + Race3 + Mar2 + Mar3 + Mar4 + Mar5 + Mar9 + Hist8520 + hist8522 + hist8480 + hist8501 + hist8201 + hist8211 + grade + ts + nne + npn + er2 + er4,
    method="breslow",
    df
  )

  cox_coefficients <- round(cox_result$coefficients, digits)

  # Run the distributed implementation with one dataset
  dlcox_result <- mock.SEER(splits=1)
  dlcox_coefficients <- round(dlcox_result$coef, digits)
  names(dlcox_coefficients) <- rownames(dlcox_result)

  # Compare results
  expect_equal(dlcox_coefficients, cox_coefficients)
})


#' Test to ensure the distributed implementation returns the same result
#' as the regular CoxPH algorithm when run with multiple sites.
test_that("test 5 sites", {
  # Specify the number of digits behind the comma that need to match
  # to consider a float equal
  digits <- 5

  # Run the regular implementation
  df <- read.csv("SeerMetHeader.csv")
  cox_result <- coxph(
    Surv(df$Time, df$Censor) ~ Age + Race2 + Race3 + Mar2 + Mar3 + Mar4 + Mar5 + Mar9 + Hist8520 + hist8522 + hist8480 + hist8501 + hist8201 + hist8211 + grade + ts + nne + npn + er2 + er4,
    method="breslow",
    df
  )

  cox_coefficients <- round(cox_result$coefficients, digits)

  # Run the distributed implementation with 5 datasets
  dlcox_result <- mock.SEER(splits=5)
  dlcox_coefficients <- round(dlcox_result$coef, digits)
  names(dlcox_coefficients) <- rownames(dlcox_result)

  # Compare results
  expect_equal(dlcox_coefficients, cox_coefficients)
})


#' Test to ensure the distributed implementation returns the same result
#' as the regular CoxPH algorithm when run with a single site.
test_that("test prepared datasets", {
  # Specify the number of digits behind the comma that need to match
  # to consider a float equal
  digits <- 5

  # Run the regular implementation
  df <- read.csv("SeerMetHeader.csv")
  cox_result <- coxph(
    Surv(df$Time, df$Censor) ~ Age + Race2 + Race3 + Mar2 + Mar3 + Mar4 + Mar5 + Mar9 + Hist8520 + hist8522 + hist8480 + hist8501 + hist8201 + hist8211 + grade + ts + nne + npn + er2 + er4,
    method="breslow",
    df
  )

  cox_coefficients <- round(cox_result$coefficients, digits)

  # Run the distributed implementation with the custom prepared datasets
  # i.e. Dataset1.csv and Dataset2.csv
  dlcox_result <- mock.SEER.custom.split()
  dlcox_coefficients <- round(dlcox_result$coef, digits)
  names(dlcox_coefficients) <- rownames(dlcox_result)

  expect_equal(dlcox_coefficients, cox_coefficients)
})

