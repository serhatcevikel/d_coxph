############################################################################
#### Code used to create dataset1.csv and dataset2.csv
#### Odd numbers in dataset 1, even in dataset 2
#### Removed row 12 from set 1 and added to set 2 to ensure unequal number of eventtimes

# full_data <- as.matrix(read.csv("SeerMetHeader.csv", sep=";"))
full_data <- as.matrix(read.csv("SeerMetHeader.csv"))

m <- full_data
full_data1 <- m[seq(1, nrow(m), by = 2), ]
full_data2 <- m[seq(2, nrow(m), by = 2), ]

v <- full_data1[12, ]
full_data1 <- full_data1[-12,]
full_data2 <- rbind(full_data2, v)


nrow(full_data1)
nrow(full_data2)

write.csv(full_data1, file = "Dataset1.csv", row.names=FALSE)
write.csv(full_data2, file = "Dataset2.csv", row.names=FALSE)