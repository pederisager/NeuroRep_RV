
#### Load packages and data #### 

library(tidyverse)

bibliometrics <- readRDS("../processed_data/wos+cwts_all_records_data.Rds")
data.A <- readRDS("../raw_data/dataset_A_data.rds")

#### Remove old versions of to-be-updated variables and clean bibliometrics data #### 

data.A.updated <- select(data.A, -c(p_oa, p_oa_gold, p_oa_bronze, p_oa_hybrid, p_oa_green))

bibliometrics <- select(bibliometrics, 
                        UT, DI,
                        crossref_citations, scopus_citations, altmetric_score, TC_2020, 
                        cluster_id1, n_pubs, label1, label2, label3, label4, label5, 
                        tcs, mcs, tncs, mncs, mnjs, pp_top_perc, pp_uncited, prop_self_cits, int_cov, 
                        p_oa, p_oa_gold, p_oa_bronze, p_oa_hybrid, p_oa_green)

bibliometrics$crossref_citations[4415] <- bibliometrics$crossref_citations[4416]  # These indeces refer to the same record, and should have the same Crossref citation count. citation count in Crossref may perhaps have been updated during the API call. Setting citation count to the higher of the two available counts.  
bibliometrics$crossref_citations[5773] <- bibliometrics$crossref_citations[5772]  # These indeces refer to the same record, and should have the same Crossref citation count. citation count in Crossref may perhaps have been updated during the API call. Setting citation count to the higher of the two available counts.  


#### Update data #### 

data.A.updated <- left_join(data.A.updated, unique(bibliometrics), by = c("UT", "DI"))


#### Save updated data #### 

saveRDS(data.A.updated, "../processed_data/dataset_A_updated_data.rds")
write.table(file = "../processed_data/dataset_A_updated_data.tsv", sep = "\t", x = data.A.updated, row.names = FALSE)
