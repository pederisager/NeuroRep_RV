#### Goal C pilot experiment - stimuli sampling script ####

###########################################################
# This script samples X rows from Neurorep dataset A for  #
# inclusion in goal C pilot. Data will be sampled from    # 
# the highest, middle, and lowest formula RV studies in   #
# the set.                                                #
###########################################################

# Load packages

library(tidyverse)

# Load dataset A
data.A <- readRDS("../processed_data/dataset_A_updated_data.rds")  # Load updated version of dataset A that contains updated WoS citation scores
excluded <- which(data.A$excluded == 1)
too_recent <- which(data.A$PY > 2018)
exclude <- unique(c(excluded, too_recent))

data.A <- data.A[-exclude,]  # Remove excluded studies


# Calculate RV
data.A$RV <- (1/data.A$sample_size) * (as.numeric(data.A$TC_2020)/(2020-data.A$PY+1))
excludeRV <- which(is.na(data.A$RV))
data.A <- data.A[-excludeRV,] 
data.A <- data.A[order(-data.A$RV, data.A$sample_size),]  # Order data by RV in descending order



# Draw sample

## Selection rules
## 30 highest, 30 lowest, 30 from the "center" of the scale. Scale center is determined by trimming the 30 highest and lowest values from the set, and then taking the average of the range of the remaining values.
## Exclude articles more recent that 2018. 

## Sample 30 highest RV studies

highest <- data.A[1:30,]
highest$RV_dist_location = "top"

## Sample 30 studies from trimmed RV range/2

trim_edges_by <- 30

trimmedRVcenter <- sum(range(data.A$RV[(1+trim_edges_by):(length(data.A$RV)-trim_edges_by)]))/2
closesttocenter <- length(data.A$RV)-findInterval(trimmedRVcenter, sort(data.A$RV))+1

middle <- data.A[(closesttocenter-14):(closesttocenter+15),]
middle$RV_dist_location = "center"

## Sample 30 lowest RV studies

lowest <- data.A[nrow(data.A):(nrow(data.A)-29),]
lowest$RV_dist_location = "bottom"

## Combine samples

data.C.pilot <- rbind(highest, middle, lowest)

## Clean data

data.C.pilot <- select(data.C.pilot, 
                       UT, AU, TI, PY, DI, study_number,
                       TC_2020, sample_size, 
                       RV_dist_location, RV)



# Save data

write.csv(x = data.C.pilot, file = "../processed_data/dataset_C_pilot_data.csv", row.names = F)
