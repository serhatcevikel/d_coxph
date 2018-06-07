# dcoxph
R implementation of the distributed Cox Proportional Hazards algorithm as described by [Lu et al.](https://www.ncbi.nlm.nih.gov/pubmed/26159465) that can be used with the [PyTaskManager](https://www.github.com/IKNL/pytaskmanager) distributed learning infrastructure.

The code is split into a local and a distributed part. Both parts are implemented in the same R script `dl_coxph.R`. The Docker registry at https://docker-registry.distributedlearning.ai already hosts an image with the distributed code. If you are using your own installation of the infrastructure, your/the researcher should create a Docker image that holds the distributed code (see also `build_docker.sh`) and push the image to a (private) Docker registry. 

A researcher would start the analysis by:
1. Creating a client that communicates with the distributed learning infrastructure
1. Calling the method `dcoxph` with the appropriate parameters


This is illustrated by the following R code:
```R
source("Client.R")
source("dl_coxph.R")

# Create a client object to communicate with the server.
client <- Client(host, username, password, collaboration_id)
client$authenticate()

# Parameters used to interpret the hub's datastore
expl_vars <- c("Age","Race2","Race3","Mar2","Mar3","Mar4","Mar5","Mar9",
             "Hist8520","hist8522","hist8480","hist8501","hist8201",
             "hist8211","grade","ts","nne","npn","er2","er4")
time_col <- "Time"
censor_col <- "Censor"

results <- dcoxph(client, expl_vars, time_col, censor_col)
```

For an overview of the working of the algorithm, see the figure below:
![Systems overview](https://raw.githubusercontent.com/IKNL/dcoxph/master/img/flowchart_dcoxph.png)

