# The Dockerfile tells Docker how to construct the image with your algorithm.
# Once pushed to a repository, images can be downloaded and executed by the
# network hubs.

# Use R as the base image.
FROM r-base:latest

RUN apt-get update
RUN apt-get install -y libssl-dev libcurl4-openssl-dev

# Change directory to '/app’. This means the subsequent ‘RUN’ steps will
# execute in this directory.
WORKDIR /app

# Create two files that will later serve for input/output
RUN touch input.txt
RUN touch output.txt
RUN touch database

COPY dl_coxph.R /app
COPY install_packages.R /app

RUN Rscript install_packages.R


# Tell docker to execute the script in `WORKDIR` when the image is run.
CMD ["Rscript","dl_coxph.R"]
