
#### Load functions #### 

library(tidyverse)

#### Load data ####

data <- readRDS("../processed_data/metadata_WoS/wos_all_records_data.Rds")

wos2020 <- rbind(
  
  wos2020.searchterm <- rbind(
    
    read.delim(file = "../raw_data/source_data/metadata_WoS/updated_TC_counts/searchterm_records_1-5000.txt", 
               header = FALSE, 
               sep = "\t", 
               encoding = "UTF-8", 
               check.names = FALSE, # otherwise R will mangle the names
               fill = TRUE, 
               quote = "", 
               stringsAsFactors = FALSE), 
    
    read.delim(file = "../raw_data/source_data/metadata_WoS/updated_TC_counts/searchterm_records_5001-5936.txt", 
               header = FALSE, 
               sep = "\t", 
               encoding = "UTF-8", 
               check.names = FALSE, # otherwise R will mangle the names
               fill = TRUE, 
               quote = "", 
               stringsAsFactors = FALSE)
  ),
  
  wos2020.journal <- read.delim(file = "../raw_data/source_data/metadata_WoS/updated_TC_counts/journal_records_1-2947.txt", 
                                header = FALSE, 
                                sep = "\t", 
                                encoding = "UTF-8", 
                                check.names = FALSE, # otherwise R will mangle the names
                                fill = TRUE, 
                                quote = "", 
                                stringsAsFactors = FALSE)
  
)


#### clean 2020 data ####

wos2020 <- unique(wos2020)
names(wos2020) <- as.matrix(wos2020[1, ])
wos2020$TC_2020 <- wos2020$TC
wos2020 <- wos2020[-1,c("UT", "TC_2020")]


#### Add 2020 TC counts as new variable to main dataset ####

data <- left_join(data, wos2020, by = "UT", copy = FALSE)  # 445 WoS TC scores missing after join


#### save output ####

saveRDS(file = "../processed_data/metadata_WoS/wos_all_records_data.Rds", object = data)
write.table(x = data, file = "../processed_data/metadata_WoS/wos_all_records_data.tsv", sep = "\t", row.names = F)

