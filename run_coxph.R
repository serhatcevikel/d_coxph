#!/usr/bin/env Rscript

#' ----------------------------------------------------------------------------
#' title: run_coxph.R
#' description:
#'   User script to run the Cox Proportional Hazards algorithm on the
#'   PyTaskManager distributed learning infrastructure.
#'
#'   This implementation can be used with the PyTaskManager distributed learning
#'   infrastructure (see https://github.com/mellesies/pytaskmanager/tree/DEV)
#'   and works with commit [befd87d].
#' author:
#'   Melle Sieswerda <m.sieswerda@iknl.nl>
#'   Anna van der Zalm <a.vanderzalm@iknl.nl>
#'   Gijs Geleijnse <g.geleijnse@iknl.nl>
#' date: 02-may-2018
#' license: MIT License
#' ----------------------------------------------------------------------------


source("Client.R")
source("dl_coxph.R")


# ******************************************************************************
# ---- Run the distributed CoxPH algorithm locally ----
# ******************************************************************************

#' Run the computation locally on a SEER dataset
#' FIXME: add attribution
mock.SEER <- function(splits=5) {
  # Load the entire dataset and split it into parts
  df <- read.csv("SeerMetHeader.csv")

  # Variables frequently used as input for the RPC calls
  expl_vars <- c("Age","Race2","Race3","Mar2","Mar3","Mar4","Mar5","Mar9","Hist8520","hist8522","hist8480","hist8501","hist8201","hist8211","grade","ts","nne","npn","er2","er4")
  time_col <- "Time"
  censor_col <- "Censor"

  return(mock(df, expl_vars, time_col, censor_col, splits=splits))
}

#' Test using different numbers of splits.
#' Results (betas) should be equal.
mock.SEER.multiple <- function(max_splits) {
  results <- list()

  for (s in 1:max_splits) {
    results[[s]] <- mock.SEER(s)
  }

  return(results)
}

#' Test using the split dataset created with 'datasetSplit.R'
#' Results (betas) should be equal.
mock.SEER.custom.split <- function() {
  datasets <- list()
  datasets[[1]] <- read.csv("Dataset1.csv")
  datasets[[2]] <- read.csv("Dataset2.csv")

  client <- MockClient(datasets)

  expl_vars <- c("Age","Race2","Race3","Mar2","Mar3","Mar4","Mar5","Mar9","Hist8520","hist8522","hist8480","hist8501","hist8201","hist8211","grade","ts","nne","npn","er2","er4")
  time_col <- "Time"
  censor_col <- "Censor"

  results <- run(client, expl_vars, time_col, censor_col, call.method=mock.call)

  return(results)
}

#' Run the computation locally on the UIS Drug treatment study dataset from
#' https://vincentarelbundock.github.io/Rdatasets/doc/quantreg/uis.html
mock.UMASS <- function(splits=5) {
  # Load the entire dataset and split it into parts
  df <- read.csv("UMASS-p.csv", sep=";")

  # Variables frequently used as input for the RPC calls
  expl_vars <- c("AAE", "BDS", "HU", "CU", "IVDUPN", "IVDURN", "NPDT", "RACE", "TREAT", "SITE")
  time_col <- "TIME"
  censor_col <- "CENSOR"

  return(mock(df, expl_vars, time_col, censor_col, splits=splits))
}

#' Run the computation locally on the UIS Drug treatment study dataset from
#' https://vincentarelbundock.github.io/Rdatasets/doc/quantreg/uis.html
#' but use only a single covariate.
mock.UMASS.univariate <- function(splits=5) {
  # Load the entire dataset and split it into parts
  df <- read.csv("UMASS-uni.csv", sep=";")

  # Variables frequently used as input for the RPC calls
  expl_vars <- c("AAE")
  time_col <- "TIME"
  censor_col <- "CENSOR"

  return(mock(df, expl_vars, time_col, censor_col, splits=splits))
}



# ******************************************************************************
# ---- Run the CoxPH algorithm on the distributed infrastructure ----
# ******************************************************************************

#' Apply CoxPH to the SEER dataset
run.SEER <- function(host, username, password, collaboration_id) {
  # Create a client object to communicate with the server.
  client <- Client(host, username, password, collaboration_id)
  client$authenticate()

  # Parameters used to interpret the hub's datastore
  expl_vars <- c("Age","Race2","Race3","Mar2","Mar3","Mar4","Mar5","Mar9",
                 "Hist8520","hist8522","hist8480","hist8501","hist8201",
                 "hist8211","grade","ts","nne","npn","er2","er4")
  time_col <- "Time"
  censor_col <- "Censor"

  results <- run(client, expl_vars, time_col, censor_col)
  return(results)
}






