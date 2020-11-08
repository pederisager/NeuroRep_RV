
#### Add Scopus citation counts to WoS bibliometrics ####

# Load packages

library(rAltmetric)

# Load and prepare data

data <- readRDS("../processed_data/metadata_WoS/wos_all_records_data.Rds")
data$DI[data$DI==""] <- NA

# Add Scopus citations

for (i in 1:nrow(data)) {
  
  print(data$DI[i])
  
  altmetric_score <- try(altmetrics(doi = data$DI[i])[["score"]])  # Extract bibliometric JSON and fetch citation count from the object
  
  data$altmetric_score[i] <- ifelse(test = is.character(altmetric_score), 
                                    yes = NA, 
                                    no = altmetric_score)  # if null is returned, Scopus failed to find a reference for the DOI, and NA is returned to data$scopus_citations
  
}

# Save data
saveRDS(file = "../processed_data/metadata_WoS/wos_all_records_data.Rds", object = data)
write.table(x = data, file = "../processed_data/metadata_WoS/wos_all_records_data.tsv", sep = "\t", row.names = F)
