data <- readRDS("../raw_data/articles.Rds") # read wos data with open access data from CWTS included


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


# Filter dataset based on keywords ####

## Filter initial dataset based on popular excludable terms from the word clouds

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


include.l <- grepl(pattern = paste(include, collapse = "|"), x = tit.abs.keyw, ignore.case = TRUE)  # identify indexes in data matching term(s)
exclude.l <- grepl(pattern = paste(exclude, collapse = "|"), x = tit.abs.keyw, ignore.case = TRUE)  # identify indexes in data matching term(s)
data.filt <- data[include.l & !exclude.l,]  # filter rows based on identified terms

## Create list of remaining unique keywords

keys <- unlist(list(strsplit(data.filt$ID, "; "), strsplit(data.filt$DE, "; ")))  # merge all author and plus keywords in filtered data
keys.unique <- unique(tolower(keys))  # find all unique keywords 
write.csv(keys.unique, "../processed_data/metadata_WoS/wos_filtered_unique_keywords.csv", row.names = FALSE)  # write list of unique keywords to file

## Load and filter by extended list of excluded terms

exclude.list <- read.csv("../raw_data//step_1_excluded_keywords.csv", stringsAsFactors = FALSE)  # Load list of to-be-excluded terms, as decided on by the authors

exclude.words <- exclude.list$Term[exclude.list$exclude...final=="yes"]
exclude.words <- paste0("\\b", exclude.words, "\\b")  # Define list of terms to exclude. "\\b" wrappers makes sure only whole words are matched
include.l <- grepl(pattern = paste(include, collapse = "|"), x = tit.abs.keyw, ignore.case = TRUE)  # identify indexes in data matching term(s)
exclude.l <- grepl(pattern = paste(exclude.words, collapse = "|"), x = tit.abs.keyw, ignore.case = TRUE)  # identify indexes in data matching term(s)
data.filt <- data[include.l & !exclude.l,]  # filter rows based on identified terms
data.filt <- data.filt[!duplicated(data.filt$DI),]  # Exclude duplicate records, identified through duplicate DOIs. The higher row index duplicate will be excluded.


# Filter dataset based on keywords ####

## Set seed to make sure dataset A can be reproduced from the code

set.seed(030142019)

## Create dataset A by extracting 1000 random records from the keyword-filtered data

indexes <- sort(sample(nrow(data.filt), 1000))  # define a set of random indexes to include in dataset A
datasetA <- data.filt[indexes, ]  # Extract rows based on indexes
datasetA_sparse <- datasetA[, c("AU", "TI", "PY", "DI", "UT")]  # make a sparse version of the dataset for manual coding purposes

write.table(file = "../processed_data/studies.tsv", sep = "\t", x = datasetA, row.names = FALSE)
write.table(file = "../processed_data/studies_sparse.tsv", sep = "\t", x = datasetA_sparse, row.names = FALSE)

## Append 500 additional random records to A from the keyword-filtered data to make up for missing data in the first set:

indexes_500 <- sort(sample(c(1:nrow(data.filt))[-indexes], 500))  ## define a set of random indexes not already included in the "indexes" variable

datasetA <- data.filt[c(indexes, indexes_500), -1]  # Extract rows based on indexes and indexes_500
datasetA_sparse <- datasetA[, c("AU", "TI", "PY", "DI", "UT")]  # make a sparse version of the dataset for manual coding purposes
datasetA_added500 <- data.filt[c(indexes_500), c("AU", "TI", "PY", "DI", "UT")]

write.table(file = "../processed_data/studies.tsv", sep = "\t", x = datasetA, row.names = FALSE)
saveRDS(object = datasetA, file = "../processed_data/studies.rds")  # rds version of this data needed due to file encoding issues with the .tsv file
write.table(file = "../processed_data/studies_sparse.tsv", sep = "\t", x = datasetA_sparse, row.names = FALSE)
write.table(file = "../processed_data/studies_added500.tsv", sep = "\t", x = datasetA_added500, row.names = FALSE)
