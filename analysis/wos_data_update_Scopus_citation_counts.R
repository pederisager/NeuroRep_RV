
#### Add Scopus citation counts to WoS bibliometrics ####

# Load packages
library(rscopus)
set_api_key('9ac91fdbf147316eeb53a16681ddc95f')
# Load and prepare data
data <- readRDS("../processed_data/metadata_WoS/wos_all_records_data.Rds")
data$DI[data$DI==""] <- NA
data$scopus_citations <- NA

# Add Scopus citations

for (i in 1:nrow(data)) {
  print(data$DI[i])
  
  citation_count <- as.numeric(abstract_retrieval(id = data$DI[i], identifier = "doi")[["content"]][["abstracts-retrieval-response"]][["coredata"]][["citedby-count"]])  # Extract bibliometric JSON and fetch citation count from the object
  
  data$scopus_citations[i] <- ifelse(test = is.null(citation_count), 
                                  yes = NA, 
                                  no = citation_count)  # if null is returned, Scopus failed to find a reference for the DOI, and NA is returned to data$scopus_citations
  
}

# Save data
saveRDS(file = "../processed_data/metadata_WoS/wos_all_records_data.Rds", object = data)
write.table(x = data, file = "../processed_data/metadata_WoS/wos_all_records_data.tsv", sep = "\t", row.names = F)
