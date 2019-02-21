

#setwd("C:/Users/20176239/Dropbox/jobb/PhD/Projects/2019_NeuroRep_Replication_Value/raw_data/source_data/metadata_WoS/temp/")

filenames <- list.files(pattern = "records.*txt")

for (file in filenames) {
  
  x <- read.delim(file = file, 
                  header = FALSE, 
                  sep = "\t", 
                  encoding = "UTF-8", 
                  check.names = FALSE, # otherwise R will mangle the names
                  fill = TRUE, 
                  quote = "", 
                  stringsAsFactors = FALSE)
  
  x[,ncol(x)] <- NULL
  colnames(x) <- x[1, ]
  x <- x[-1, ]
  
  name <- substr(file, 1, nchar(file)-4)  # remove ".txt" (the last 4 characters) of the filename before renaming
  saveRDS(object = x, file = paste0(name, ".Rds"))
  
}




