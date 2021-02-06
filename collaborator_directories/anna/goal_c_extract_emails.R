# for: creating a list of emails from the NeuroRep dataset to invite authors for Expert RV judgment study (goal C)
# code reads total data (the dataset after filtering on keywords [is this correct?] 
# and removes emails from dataset A [this should be updated to only be the 90 study IDs we select for Goal C])
# date: jan 06 2021, Anna van 't Veer

library(readr)
library(dplyr)

# Read in total dataset of wos papers ----
setwd("/Users/veeraevant1/surfdrive/8_Tools/5_git/NeuroRep_RV/processed_data")
data_total <- read_csv("wos+cwts_nonexcluded_records_data.csv") # assuming this is the total data we want this from, or from the ~8000, @peder?

# keep only relevant columns from the total data for extracting emails
data_total_emails <- data_total[, c("DI", "UT", "EM")]

# read in dataset A
data_A <- readRDS("dataset_A_data.rds")

# removing UT's present in dataset A (note this to be replaced with 90 UTs) from the total data
omit <- data_A$UT
data_emails_excl_datasetA <- filter(data_total_emails, !(UT %in% omit))

# verification: data_total has 2296 obs, data_A has 1681 obs, result should be
2296 - 1681 # 615, yet there are 774 in the new email list...


