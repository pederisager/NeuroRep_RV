# Load packages
library(tidyverse)

# Set directory
setwd("c:/Users/peder/Dropbox/jobb/PhD/Projects/2019_NeuroRep_Replication_Value/analysis")

# Load data
data <- readRDS("../processed_data/dataset_A_updated_data.rds")  # Load dataset A

# Remove excluded rows
data <- data[is.na(data$excluded),]  # Remove excluded rows from data

## Add missing values in TC_2020 and PY to data and recalculate RV
missing.data <- read.csv("../raw_data/dataset_A_missing_TC_2020_and_PY.csv")  # missing PY and TC_2020 values, manually identified in WoS
for (i in 1:nrow(data)) {
  if (is.na(data$PY[i]))
    data$PY[i] <- missing.data$PY[which(missing.data$UT==data$UT[i])[1]]
  if (is.na(data$TC_2020[i]))
    data$TC_2020[i] <- missing.data$TC_2020[which(missing.data$UT==data$UT[i])[1]]
}
rm(missing.data, i)

# Convert variable data type
data$TC_2020 <- as.numeric(data$TC_2020)

# Save data
saveRDS(object = data, file = "../processed_data/dataset_A_updated_cleaned.rds")  # Save cleaned version of dataset A
