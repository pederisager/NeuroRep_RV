

library(tidyverse)

filenames <- list.files(pattern = "records.*txt")

files <- lapply(filenames, function(file) {
  read.delim(file = file, 
             sep = "\t", 
             header = FALSE, 
             fill = TRUE, 
             fileEncoding = "UTF-16",
             quote = "")
  })

data <- bind_rows(files)

x <- read.delim(file = filenames[13], header = FALSE, fileEncoding = "UTF-16", sep = "\t", fill = TRUE, quote = "")






library(readr)
options(stringsAsFactors = FALSE)

x <- read_delim(file = filenames[15], delim = "\t", quote = "", locale = locale(encoding = "UTF-16LE"))
