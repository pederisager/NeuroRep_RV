
path <- "../raw_data/source_data/metadata_WoS/"
filenames <- list.files(path = path, pattern = "records.*txt")
outpath <- "../processed_data/metadata_WoS/"

files <- lapply(filenames, function(file) {
  x <- read.delim(file = paste0(path, file), 
                  header = FALSE, 
                  sep = "\t", 
                  encoding = "UTF-8", 
                  check.names = FALSE, # otherwise R will mangle the names
                  fill = TRUE, 
                  quote = "", 
                  stringsAsFactors = FALSE)
  
  colnames(x) <- x[1, ]
  x <- x[-1, ]
  x <- x[,-ncol(x)]
  name <- strsplit(file, "_")[[1]][[1]]  # remove ".tsv" (the last 4 characters) of the filename before adding filename as variable
  x$source <- name
  x$file <- file
  x
  })

data <- do.call("rbind", files)

write.table(x = data, file = paste0(outpath, "wos_all_records_data.tsv"), quote = TRUE, sep = "\t", na = "NA", row.names = FALSE, fileEncoding = "UTF-8")
saveRDS(object = data, file =  paste0(outpath, "wos_all_records_data.Rds"))
