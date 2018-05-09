#!/usr/bin/env Rscript

#' ----------------------------------------------------------------------------
#' title: split_SEER_dataset.R
#' description:
#'   Script to split the CSV file with the SEER dataset (SeerMetHeader.csv)
#'   into two separate files (Dataset1.csv and Dataset2.csv) that can be used
#'   to test the distributed learning algorithm and infrastructure.
#' author:
#'   Melle Sieswerda <m.sieswerda@iknl.nl>
#'   Anna van der Zalm <a.vanderzalm@iknl.nl>
#'   Gijs Geleijnse <g.geleijnse@iknl.nl>
#' date: 09-may-2018
#' license: MIT License
#' ----------------------------------------------------------------------------

# Load the original (full) CSV file
full_data <- as.matrix(read.csv("SeerMetHeader.csv"))

# Odd rows go into dataset 1, even in dataset 2
data1 <- full_data[seq(1, nrow(full_data), by=2), ]
data2 <- full_data[seq(2, nrow(full_data), by=2), ]

# Move row 12 from dataset 1 to dataset 2 to ensure unequal number of
# eventtimes
tmp <- data1[12, ]
data1 <- data1[-12,]
data2 <- rbind(data2, tmp)

# Write the end result to disk
write.csv(data1, file = "Dataset1.csv", row.names=FALSE)
write.csv(data2, file = "Dataset2.csv", row.names=FALSE)