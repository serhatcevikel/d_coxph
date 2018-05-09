#!/usr/bin/env Rscript

#' ----------------------------------------------------------------------------
#' title: run_tests.R
#' description:
#'   Script to run all unittests for the distributed Cox Proportional Hazards
#'   algorithm.
#' author:
#'   Melle Sieswerda <m.sieswerda@iknl.nl>
#'   Anna van der Zalm <a.vanderzalm@iknl.nl>
#'   Gijs Geleijnse <g.geleijnse@iknl.nl>
#' date: 09-may-2018
#' license: MIT License
#' ----------------------------------------------------------------------------

library(testthat)
source("dl_coxph.R")

# Find and run the tests in the current directory
test_results <- test_dir("./", reporter="summary")
