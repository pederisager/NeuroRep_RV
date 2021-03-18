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
data <- readRDS("../collaborator_directories/peder/neurorep_data_exploration_data_and_scripts/data_all.Rds")  # Load datafile (we can change the file path to whatever file we want)
data <- data[-which(data$PY > 2018),]  # Remove studies that are more recent than 2018
data <- data[order(-data$RV, data$sample_size),]  # Order data by RV in descending order


# Draw sample

## Selection rules
## 30 highest, 30 lowest, 30 from the "center" of the scale. Scale center is determined by trimming the 30 highest and lowest values from the set, and then taking the average of the range of the remaining values.
## Exclude articles more recent that 2018. 

## Sample 30 highest RV studies

highest <- data[1:30,]
highest$RV_dist_location = "top"

## Sample 30 studies randomly from .45 > RV > .25

middle <- data[which(data$RV > .25 & data$RV < .45),] %>% .[sample(nrow(.), 30, replace = F), ]
middle$RV_dist_location = "middle"

## Sample 15 of studies with RV=0 that have the highest sample size, and 15 studies with RV>0 that has the lowest RV. 

lowest <- rbind(data[nrow(data):(nrow(data)-14),], 
                data[nrow(data[!data$RV==0,]):(nrow(data[!data$RV==0,])-14),])
lowest$RV_dist_location = "bottom"

## Combine samples

data.C.pilot <- rbind(highest, middle, lowest)

## Clean data

data.C.pilot <- select(data.C.pilot, 
                       UT, AU, TI, PY, DI, study_number,
                       TC_2020, sample_size, 
                       RV_dist_location, RV, AB, SO)

# Save data

write.csv(x = data.C.pilot, file = "../processed_data/dataset_C_pilot_data.csv", row.names = F)





# Resample goal C studies to replace studies where sample size was found to be erroneous, or study topic was found to meet exclusion criteria.

data.C.pilot <- read.csv("../processed_data/dataset_C_pilot_data.csv")
data <- readRDS("../processed_data/dataset_A_updated_cleaned.rds")
data <- data[-which(data$PY > 2018),]  # Remove studies that are more recent than 2018
data$years.since.pub <- 2020 - data$PY
data$RV <- (data$TC_2020/ (data$years.since.pub + 1) ) * (1/data$sample_size)
data <- data[order(-data$RV, data$sample_size),]  # Order data by RV in descending order

data <- data[-which(data$UT == "WOS:000377917500044" & data$study_number == 1),]  # Exclude. This is study 1 of the same paper that has study 2 also in this dataset. 
data <- data[-which(data$UT == "WOS:000309203500010" & data$study_number == 1),]  # Exclude on the basis this is about the device used. It is methodological.
data <- data[-which(data$UT == "WOS:000404656000005" & data$study_number == 1),]  # Exclude, methodological: this examines the validity of a method used by studies in social neuroscience.

## Resample top
top <- select(data[1:30, ], UT, AU, TI, PY, DI, study_number, TC_2020, sample_size, RV, AB, SO) 
top$RV_dist_location <- "top"
top <- select(top, 
              UT, AU, TI, PY, DI, study_number,
              TC_2020, sample_size, 
              RV_dist_location, RV, AB, SO)
data.C.pilot[data.C.pilot$RV_dist_location == "top", ] <- top

## Resample bottom
lowest <- rbind(data[nrow(data):(nrow(data)-14),], 
                data[nrow(data[!data$RV==0,]):(nrow(data[!data$RV==0,])-14),])
lowest <- select(lowest, UT, AU, TI, PY, DI, study_number, TC_2020, sample_size, RV, AB, SO) 
lowest$RV_dist_location <- "bottom"
lowest <- select(lowest, 
                 UT, AU, TI, PY, DI, study_number,
                 TC_2020, sample_size, 
                 RV_dist_location, RV, AB, SO)
data.C.pilot[data.C.pilot$RV_dist_location == "bottom", ] <- lowest 

## Correct middle where sample size was incorrectly coded
i <- which(data.C.pilot$UT == "WOS:000326953700045")
data.C.pilot$sample_size[i] <- 20
data.C.pilot$RV[i] <- (data.C.pilot$TC_2020[i] / (2020-data.C.pilot$PY[i] + 1) ) * (1/data.C.pilot$sample_size[i])

# Save data
write.csv(data.C.pilot, file = "../processed_data/dataset_C_pilot_data_updated.csv", row.names = F)
