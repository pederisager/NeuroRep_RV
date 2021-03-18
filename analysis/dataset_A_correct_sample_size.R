
# Set directory
setwd("c:/Users/peder/Dropbox/jobb/PhD/Projects/2019_NeuroRep_Replication_Value/analysis")

# Load data
data <- readRDS("../processed_data/dataset_A_updated_data.rds")  # Load dataset A

## Correct erroneously coded sample sizes discovered when estimating sample size interrater relaibility.
data.irr <- read.csv(file = "../raw_data/dataset_B_inter_rater_reliability_discrepancies_resolved_by_PhD.tsv", header = T, sep = "\t", quote = "")

for (i in 1:nrow(data.irr)) {
  if(isFALSE(data.irr$sample_size_final[i] == data$sample_size[which(data$UT == data.irr$UT[i] & data$study_number == data.irr$study_number[i])])) {
    data$sample_size[which(data$UT == data.irr$UT[i] & data$study_number == data.irr$study_number[i])] <- data.irr$sample_size_final[i]
  }
}

## Correct erronously coded sample sizes discovered when inspecting goal C stimulus dataset
data[which(data$UT == "WOS:000265095500017"), "sample_size"] <- 16  # Sample size should be 16
data[which(data$UT == "WOS:000434825200001"), "sample_size"] <- 16  # Sample size should be 16
data[which(data$UT == "WOS:000349669600023"), "sample_size"] <- 21  # Sample size should be 21
data[which(data$UT == "WOS:000326953700045"), "sample_size"] <- 20  # Sample size should be 20
data[which(data$UT == "WOS:000377048600037" & data$study_number == 1), "sample_size"] <- 114  # Sample size should be 114

# Save data
saveRDS(data, "../processed_data/dataset_A_updated_data.rds")
write.table(file = "../processed_data/dataset_A_updated_data.tsv", sep = "\t", x = data, row.names = FALSE)