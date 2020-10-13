
#### Add Crossref citation counts to WoS bibliometrics ####

# Load packages
library(rcrossref)
# Load and prepare data
data <- readRDS("../processed_data/metadata_WoS/wos_all_records_data.Rds")
data$DI[data$DI==""] <- NA

# Add Crossref citations
cr_citations <- cr_citation_count(doi = data$DI)  # Search Crossref for and extract citations

data$crossref_citations <- cr_citations$count  # Add citation counts to dataframe

# Save data
saveRDS(file = "wos_all_records_data.Rds", object = data)
write.table(x = data, file = "wos_all_records_data.tsv", sep = "\t", row.names = F)
