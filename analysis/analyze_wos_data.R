data <- readRDS("../processed_data/metadata_WoS/wos_all_records_data.Rds")


# Make a word cloud of the 200 most commonly mentioned keywords ####

library(wordcloud)
library(stringr)
library(RColorBrewer)

## Author keywords

words <- strsplit(data$DE, "; ")
pal = brewer.pal(9,"Dark2")
wordcloud(unlist(words), max.words = 100, random.order = FALSE, random.color = TRUE, colors = pal)

## WoS Keywords Plus

words <- strsplit(data$ID, "; ")
pal = brewer.pal(9,"Dark2")
wordcloud(unlist(words), max.words = 100, random.order = FALSE, random.color = TRUE, colors = pal)


# filter dataset based on keywords ####

library(tidyverse)

tit.abs.keyw <- paste(data$TI, data$AB, data$ID, data$DE) # Combine title, abstract and keyword information into one column for grep to search

include <- c("\\bfmri\\b")  # include only records containing the following terms. "\\b" wrappers makes sure only whole words are matched.

exclude <- c("\\badhd\\b",
             "\\banimal\\b",
             "\\bmonkey\\b",
             "\\bochrogaster\\b",
             "\\brat\\b",
             "\\bmouse\\b",
             "\\bautism\\b", 
             "\\beeg\\b", 
             "\\bdepression\\b", 
             "\\bdisorder\\b", 
             "\\bdisorders\\b", 
             "\\bschizophrenia\\b")  # Define list of terms to exclude. "\\b" wrappers makes sure only whole words are matched.


include.l <- grepl(pattern = paste(include, collapse = "|"), x = tit.abs.keyw, ignore.case = TRUE)
exclude.l <- grepl(pattern = paste(exclude, collapse = "|"), x = tit.abs.keyw, ignore.case = TRUE)
data.filt <- data[include.l & !exclude.l,]  # filter rows based on identified terms. 

## Create list of remaining unique keywords

keys <- unlist(list(strsplit(data.filt$ID, "; "), strsplit(data.filt$DE, "; ")))  # merge all author and plus keywords in filtered data
keys.unique <- unique(tolower(keys))  # find all unique keywords 
write.csv(keys.unique, "../processed_data/metadata_WoS/wos_filtered_unique_search_terms.csv", row.names = FALSE)  # write list of unique keywords to file
