# Read dataset A

## Read the coded data
data.A.coded <- read.table(file = "../raw_data/dataset_A_coded.tsv", header = T, sep = "\t", quote = "\"", na.strings = "", stringsAsFactors = F)

## Read the full WoS info data
data.A.wos <- readRDS(file = "../raw_data/dataset_A_wos.rds") 

## merge the two versions of the data by WOS number
data.A <- merge(data.A.coded, data.A.wos[, !names(data.A.wos) %in% c("AU", "TI", "PY", "DI")], by = "UT")  # delete duplicate columns and merge datasets by WOS number

## Save the data to processed_data/ - The files will be manually transferred to raw_data/ to make sure that no analysis code can directly overwrite data in the raw_data/ directory
write.table(file = "../processed_data/dataset_A_data.tsv", sep = "\t", x = data.A, row.names = FALSE)
saveRDS(object = data.A, file = "../processed_data/dataset_A_data.rds")  # rds version of this data needed due to file encoding issues with the .tsv file
