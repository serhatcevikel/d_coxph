#!/usr/bin/env Rscript

# External libraries
library(abind)
library(rjson)
library(httr)


# ******************************************************************************
# ---- Helper functions ----
# ******************************************************************************

#' Write a string to STDOUT without the standard '[1]' prefix.
writeln <- function(x="", sep=" ") {
    cat(paste(paste(x, collapse=sep), "\n"))
}

#' Convert a list of rows into a dataframe
list.to.matrix <- function(l) {
  # sapply applies c() to the list and returns a matrix.
  # The matrix is then transposed
  return(t(sapply(l, c)))
}

#' Split the dataframe column wise into covariate, censor and time columns.
pre_process_data <- function(df, expl_vars, censor_col, time_col) {
  # Sort the dataframe/matrix by time
  sort_idx <- order(df[, time_col])
  df <- df[sort_idx, ]

  # Split dataframe into explanatory variables, time and censor columns
  Z <- df[, expl_vars]
  time <- df[, time_col]
  censor <- df[, censor_col]

  # Return the list (dict)
  return(list(
    Z=Z,
    time=time,
    censor=censor
  ))
}

# ******************************************************************************
# ---- RPC entry points ----
# ******************************************************************************

#' Return a dataframe of unique event times
#'
#' Params:
#'   df: dataframe
#'   time_col: name of the column that contains the event/censor times
#'
#' Return:
#'   dataframe with columns time and Freq
RPC_get_unique_event_times_and_counts <- function(df, time_col, censor_col) {

  time <- df[df[, censor_col]==1, time_col]
  time <- sort(time)

  df_time <- as.data.frame(table(time), stringsAsFactors=F)
  return(df_time)
}

#' Compute the aggregate statistic of step 2
#' sum over all distinct times i
#'   sum the covariates of cases in the set of cases with events at time i.
#'
#' Params:
#'   df: dataframe
#'   expl_vars: list of explanatory variables (covariates) to use
#'   time_col: name of the column that contains the event/censor times
#'   censor_col: name of the colunm that explains whether an event occured or
#'               the patient was censored
#'
#' Return:
#'   numeric vector with sums and named index with covariates.
RPC_compute_summed_z <- function(df, expl_vars, time_col, censor_col) {

  # Since an item can only be in a single set of events, we're essentially
  # summing over all cases with events.
  data <- pre_process_data(df, expl_vars, censor_col, time_col)
  cases_with_events <- data$Z[data$censor == 1, ]
  return(colSums(cases_with_events))

  # ---- (Alternative implementation) ----
  # The following implementation stays closer to the original formula
  # but is fairly inefficient.
  #
  # D_k <- as.numeric(names(get_unique_event_times_and_counts(df, time_col)))
  # summed <- rep(0, ncol(data$Z))
  #
  # for (i in 1:length(D_k)) {
  #   idx <- (data$time == D_k[i]) & (data$censor == 1)
  #
  #   cases_with_events <- data$Z[idx, ]
  #
  #   if (nrow(cases_with_events) > 1) {
  #     summed <- summed + colSums(cases_with_events)
  #   } else if (nrow(cases_with_events) == 1) {
  #     summed <- summed + cases_with_events
  #   }
  # }
  #
  # return(summed)
}

#' Compute the three aggretated statistics needed for an iteration
#'
#' Params:
#' df: dataframe
#' expl_vars: list of explanatory variables (covariates) to use
#' time_col: name of the column that contains the event/censor times
#' censor_col: name of the colunm that explains whether an event occured or
#'             the patient was censored
#' beta: vector of beta coefficients (length(beta) == length(expl_vars))
#' times: vector of *globally* unique event times
#'
#' Return: list containing aggretated statistics
RPC_perform_iteration <- function(df, expl_vars, time_col, censor_col, beta, unique_event_times) {
    data <- pre_process_data(df, expl_vars, censor_col, time_col)

    D <- length(unique_event_times)
    m <- length(expl_vars)

    # initialize matrices for the aggregates we're about to compute

    agg1 <- array(dim=c(D))
    agg2 <- array(dim=c(D, m))
    dimnames(agg2) <- list(NULL, expl_vars)

    agg3 <- array(dim=c(D, m, m))
    dimnames(agg3) <- list(NULL, expl_vars, expl_vars)

    for (i in 1:D) {
      # Compute the risk set at time t; this includes *all* patients that have a
      # survival time greater than or equal to the current time
      R_i <- as.matrix(data$Z[data$time >= unique_event_times[i], ])

      # aggregate 1: SUM_risk[exp(beta * z)]
      ebz <- exp(R_i %*% beta)
      agg1[i] <- sum(ebz)

      # aggregate 2: SUM_risk[z_r *exp(beta * z)]
      # Use apply to multiply each column (element-wise) in R_i with ebz
      z_ebz <- apply(R_i, 2, '*', ebz)

      # Undo the simplification that apply does in case of a single row in R_i
      if (nrow(R_i) == 1) {
        z_ebz <- t(z_ebz)
      }

      agg2[i, ] <- colSums(z_ebz)

      # aggregate 3: SUM_risk[z_r * z_q * exp(beta * z)]
      for (j in 1:nrow(R_i)) {
        # z_ebz[j, ]: numeric vector
        # the outer product creates a matrix:
        # | z1*z1 | z1*z2 | ... | z1*zn |
        # | z2*z1 | z2*z2 | ... | z2*zn |
        # | ...   | ...   | ... | ...   |
        # | zn*z1 | zn*z2 | ... | zn*zn |
        s1 <- z_ebz[j, ] %*% t(R_i[j, ])

        if (j == 1) {
          summed <- s1
        } else {
          summed <- summed + s1
        }
      }

      agg3[i, , ] <- summed
    }

    fp <- textConnection("agg1_data", open="w")
    saveRDS(agg1, fp, ascii=T)
    close(fp)

    fp <- textConnection("agg2_data", open="w")
    saveRDS(agg2, fp, ascii=T)
    close(fp)

    fp <- textConnection("agg3_data", open="w")
    saveRDS(agg3, fp, ascii=T)
    close(fp)

    return(
      list(
        agg1=agg1_data,
        agg2=agg2_data,
        agg3=agg3_data
      )
    )
}

# ******************************************************************************
# ---- Server/orchestrator functions ----
# ******************************************************************************

#' Compute/count the *global* number of ties at each time point using the
#' tie-counts from the individual sites.
#'
#' Params:
#' Ds: list of *local* tie-counts, indexed by site nr
#'
#' Return: numeric vector with tie-counts and named index with unique
#'         event times
compute_combined_ties <- function(Ds) {

  # Merge the list of event times & counts into a single data frame
  for (k in 1:length(Ds)) {
    # This only works if the joined columns have different names
    site_name <- sprintf("site_%i", k)
    colnames(Ds[[k]]) <- c("time", site_name)

    if (k == 1) {
      D <- Ds[[k]]
    } else {
      D <- merge(D, Ds[[k]], by="time", all=T)
    }
  }

  # Cast the time column to numeric to enable proper sorting
  D[, "time"] <- as.numeric(D$time)
  D <- D[order(D$time), ]

  # Set the time column as the index
  rownames(D) <- D$time

  # Drop the time column; drop=F ensures that the data frame is not
  # coerced to a vector in case of a single column.
  D <- D[, -1, drop=F]

  # The merge/join will have introduced NAs: set these to 0
  D[is.na(D)] <- 0

  # Sum the columns to get the total nr of ties for each time
  D_all <- rowSums(D)

  return(D_all)
}

#' Compute the primary derivative (vector of length beta)
compute_derivatives <- function(z_hat, D_all, aggregates) {

  # Sum the aggregate statistics for each site
  for (k in 1:length(aggregates)) {
    if (k == 1) {
      summed_agg1 <- aggregates[[k]]$agg1
      summed_agg2 <- aggregates[[k]]$agg2
      summed_agg3 <- aggregates[[k]]$agg3
    } else {
      summed_agg1 <- summed_agg1 + aggregates[[k]]$agg1
      summed_agg2 <- summed_agg2 + aggregates[[k]]$agg2
      summed_agg3 <- summed_agg3 + aggregates[[k]]$agg3
    }
  }

  # summed_agg1: vector with scalar for each time point
  # summed_agg2: vector of length m for each time point
  # summed_agg3: matrix of size (m x m) for each time point
  for (i in 1:length(D_all)) {
    # primary
    s1 <- D_all[[i]] * (summed_agg2[i, ] / summed_agg1[i])

    # secondary
    first_part <- (summed_agg3[i, , ] / summed_agg1[i])

    # the numerator is the outer product of agg2
    numerator <- summed_agg2[i, ] %*% t(summed_agg2[i, ])
    denominator <- summed_agg1[i] * summed_agg1[i]
    second_part <- numerator / denominator

    s2 <- D_all[[i]] * (first_part - second_part)

    if (i == 1) {
      total_p1 <- s1
      total_p2 <- s2

    } else {
      total_p1 <- total_p1 + s1
      total_p2 <- total_p2 + s2
    }
  }

  primary_derivative <- z_hat - total_p1
  secondary_derivative <- -total_p2

  return(list(
    primary=primary_derivative,
    secondary=secondary_derivative
  ))
}



# ******************************************************************************
# ---- Infrastructure functions ----
# ******************************************************************************

#' Run the method requested by the server
run_RPC <- function(df, input_data) {
  # Determine which method was requested and combine arguments and keyword
  # arguments in a single variable
  method <- sprintf("RPC_%s", input_data$method)
  args <- c(list(df), input_data$args, input_data$kwargs)

  # Call the method
  writeln(sprintf("Calling %s", method))
  result <- do.call(method, args)
  return(result)
}


#' Mock an RPC call to all sites.
mock.call <- function(datasets, method, ...) {

  writeln(sprintf('** Mocking call to "%s" **', method))

  # Construct the input_data list from the ellipsis.
  # This list is normally provided as JSON by the server.
  arguments <- list(...)

  if (is.null(names(arguments))) {
    args <- arguments
    kwargs <- list()

  } else {
    args <- arguments[names(arguments) == ""]
    kwargs <- arguments[names(arguments) != ""]
  }

  input_data <- list(
    method=method,
    args=args,
    kwargs=kwargs
  )

  # Create a list to store the responses from the individual sites
  result <- list()

  # Mock calling the RPC method on each site
  for (k in 1:length(datasets)) {
    result[[k]] <- run_RPC(datasets[[k]], input_data)
  }

  writeln()
  return(result)
}

#' Run the computation locally
mock <- function(df, expl_vars, time_col, censor_col, splits=5) {

  datasets <- list()

  for (k in 1:splits) {
    datasets[[k]] <- df[seq(k, nrow(df), by=splits), ]
  }

  m <- length(expl_vars)

  # Mocking the actual procedure starts here
  # First create the vector of distinct event times & tie-counts
  Ds <- mock.call(datasets, "get_unique_event_times_and_counts", time_col, censor_col)
  D_all <- compute_combined_ties(Ds)
  unique_event_times <- as.numeric(names(D_all))

  # Compute the summed zs
  summed_zs <- mock.call(datasets, "compute_summed_z", expl_vars, time_col, censor_col)

  # z_hat: vector of same length m
  z_hat <- list.to.matrix(summed_zs)
  z_hat <- matrix(apply(z_hat, 2, as.numeric), ncol=m, dimnames=list(NULL, expl_vars))
  z_hat <- colSums(z_hat)

  # Initialize the betas to 0 and start iterating
  beta <- beta_old <- rep(0, m)
  delta <- 0

  i = 1
  while (i <= 6) {
    writeln(sprintf("-- Iteration %i --", i))
    writeln("Beta's:")
    print(beta)
    writeln()

    writeln("delta: ")
    print(delta)
    writeln()

    aggregates <- mock.call(datasets, "perform_iteration", expl_vars, time_col, censor_col, beta, unique_event_times)

    for (k in 1:length(aggregates)) {
      aggregates[[k]]$agg1 <- readRDS(textConnection(aggregates[[k]]$agg1))
      aggregates[[k]]$agg2 <- readRDS(textConnection(aggregates[[k]]$agg2))
      aggregates[[k]]$agg3 <- readRDS(textConnection(aggregates[[k]]$agg3))
    }

    # Compute the primary and secondary derivatives
    derivatives <- compute_derivatives(z_hat, D_all, aggregates)
    # print(derivatives)

    # Update the betas
    beta_old <- beta
    beta <- beta_old - (solve(derivatives$secondary) %*% derivatives$primary)

    delta <- abs(sum(beta - beta_old))
    if (delta <= 10^-8) {
      writeln("Betas have settled! Finished iterating!")
      # computing the standard errors
      SErrors <- NULL
      fisher <- solve(-derivatives$secondary)
      # standard errors are the squared root of the diagonal
      for(k in 1:dim(fisher)[1]){
        se_k <- sqrt(fisher[k,k])
        SErrors <- c(SErrors,se_k)
      } 
      #Calculating P and Z values
      zvalues <- (exp(beta)-1)/SErrors
      pvalues<- 2*pnorm(-abs(zvalues))
      pvalues<-format.pval(pvalues, digits = 1)
      # 95%CI = beta +- 1.96 * SE
      library(dplyr)
      results <- data.frame("coef" = round(beta,5), "exp(coef)"=round(exp(beta),5), "SE" = round(SErrors,5))
      results <- mutate(results, lower_ci = round(exp(coef - 1.96 * SE),5))
      results <- mutate(results, upper_ci = round(exp(coef + 1.96 * SE),5))
      results <- mutate(results, "Z"=round(zvalues, 2), "P"=pvalues)
      row.names(results) <- rownames(beta)
      return(results)
      break
    }

    # Again!!?
    i <- i + 1
  }
}

#' Run the computation locally on a SEER dataset
mock.SEER <- function() {
  # Load the entire dataset and split it into parts
  df <- read.csv("SeerMetHeader.csv", sep=";")

  # Variables frequently used as input for the RPC calls
  expl_vars <- c("Age","Race2","Race3","Mar2","Mar3","Mar4","Mar5","Mar9","Hist8520","hist8522","hist8480","hist8501","hist8201","hist8211","grade","ts","nne","npn","er2","er4")
  time_col <- "Time"
  censor_col <- "Censor"

  return(mock(df, expl_vars, time_col, censor_col))
}

#' Run the computation locally on the UIS Drug treatment study dataset from
#' https://vincentarelbundock.github.io/Rdatasets/doc/quantreg/uis.html
mock.UMASS <- function() {
  # Load the entire dataset and split it into parts
  df <- read.csv("UMASS-p.csv", sep=";")

  # Variables frequently used as input for the RPC calls
  expl_vars <- c("AAE", "BDS", "HU", "CU", "IVDUPN", "IVDURN", "NPDT", "RACE", "TREAT", "SITE")
  time_col <- "TIME"
  censor_col <- "CENSOR"

  return(mock(df, expl_vars, time_col, censor_col))
}

#' Entrypoint when excecuting this script using Rscript
#'
#' Wraps the docker input/output for run_RPC().
main <- function() {
  database_uri <- Sys.getenv("DATABASE_URI")
  writeln(sprintf("Using '%s' as database", database_uri))
  df <- read.csv(database_uri, sep=";")

  writeln("Loading input.txt")
  input_data <- fromJSON(readLines('input.txt'))

  writeln("Dispatching ...")
  result <- run_RPC(df, input_data)

  # Write result to disk
  writeln("Writing result to disk .. ")
  writeLines(toJSON(result), "output.txt")

  writeln("")
  writeln("[DONE!]")
}

if (!interactive()) {
    main()
}


