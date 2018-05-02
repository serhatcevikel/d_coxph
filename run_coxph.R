# ******************************************************************************
# ---- Run the distributed CoxPH algorithm ----
# ******************************************************************************
source("Client.R")
source("dl_coxph.R")

#' Wait for the results of a distributed task and return the task,
#' including results.
#'
#' Params:
#'   client: ptmclient::Client instance.
#'   task: list with the key id (representing the task id)
#'
#' Return:
#'   task (list) including results
wait_for_results <- function(client, task) {

  path = sprintf('/api/task/%s', task$id)

  while(TRUE) {
    r <- client$GET(path)

    if (content(r)$complete) {
      break

    } else {
      # Wait 30 seconds
      writeln("Sleeping ...")
      Sys.sleep(5)
    }
  }

  path = sprintf('/api/task/%s?include=results', task$id)
  r <- client$GET(path)

  return(content(r))
}


#' Execute a method on the distributed learning infrastructure.
#'
#' This entails creating a task and letting the hubs execute the method
#' specified in the 'input' parameter.
#'
#' Params:
#'   client: ptmclient::Client instance.
#'   method: name of the method to call on the distributed learning
#'           infrastructure
#'   ...: (keyword) arguments to provide to method; need to be JSON
#'        serializable.
call <- function(client, method, ...) {
  # Create the json structure for the call to the server
  input <- create_task_input(method, ...)

  task = list(
    "name"="CoxPH",
    "image"="localhost:5001/dl_coxph",
    "collaboration_id"=client$get("collaboration_id"),
    "input"=input,
    "description"=""
  )

  # Create the task on the server; this returs the task with its id
  r <- client$POST('/api/task', task)

  # Wait for the results to come in
  result_dict <- wait_for_results(client, content(r))

  # result_dict is a list with the keys _id, id, description, complete, image,
  # collaboration, results, etc. the entry "results" is itself a list with
  # one entry for each site. It needs a bit of work to convert the JSON response
  # within a JSON response back to R data types.
  sites <- result_dict$results
  results <- list()

  for (k in 1:length(sites)) {
    results[[k]] <- fromJSON(sites[[k]]$result)
  }

  return(results)
}




#' Apply CoxPH to a dataset
main <- function() {
  # Create a client object to communicate with the server.
  client <- Client('http://localhost:5000', 'admin@southpark.example', 3, 'password')
  client$authenticate()

  # Parameters used to interpret the hub's datastore
  expl_vars <- c("Age","Race2","Race3","Mar2","Mar3","Mar4","Mar5","Mar9","Hist8520","hist8522","hist8480","hist8501","hist8201","hist8211","grade","ts","nne","npn","er2","er4")
  time_col <- "Time"
  censor_col <- "Censor"

  m <- length(expl_vars)

  # Ask all hubs to return their unique event times with counts
  writeln("Getting unique event times and counts")
  results <- call(client, "get_unique_event_times_and_counts", time_col, censor_col)
  Ds <- lapply(results, as.data.frame)

  D_all <- compute_combined_ties(Ds)
  unique_event_times <- as.numeric(names(D_all))

  # Ask all hubs to compute the summed Z statistic
  writeln("Getting the summed Z statistic")
  summed_zs <- call(client, "compute_summed_z", expl_vars, time_col, censor_col)

  # z_hat: vector of same length m
  # Need to jump through a few hoops because apply simplifies a matrix with one row
  # to a numeric (vector) :@
  z_hat <- list.to.matrix(summed_zs)
  z_hat <- matrix(apply(z_hat, 2, as.numeric), ncol=m, dimnames=list(NULL, expl_vars))
  z_hat <- colSums(z_hat)


  # Initialize the betas to 0 and start iterating
  writeln("Starting iterations ...")
  beta <- beta_old <- rep(0, m)
  delta <- 0

  i = 1
  while (i <= 30) {
    writeln(sprintf("-- Iteration %i --", i))
    writeln("Beta's:")
    print(beta)
    writeln()

    writeln("delta: ")
    print(delta)
    writeln()

    aggregates <- call(client, "perform_iteration", expl_vars, time_col, censor_col, beta, unique_event_times)

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
      break
    }

    # Again!!?
    i <- i + 1
  }

  return(beta)
}

if (!interactive()) {
  main()
}
