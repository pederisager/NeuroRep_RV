#### Load packages ####

library(tidyverse)



#### Load data #### 

data.bib <- readRDS("../raw_data/articles.Rds")  # Load dataset containing Web of Science and CWTS bibliometrics for all records identified by our initial literature search.
data.all <- readRDS("../raw_data/studies.Rds")  # Load dataset randomly sampled from data.bib for which sample size has also been coded 
data.irr <- readRDS("../raw_data/studies_irr.Rds")  # Load dataset randomly sampled from data.all, used for checking sample size coding inter-rater reliability
codebook <- read.table("../raw_data/codebook.tsv", header = T, row.names = 1, sep = "\t")



#### Clean data ####

# Exclude cases from data.bib deemed irrelevant for our study selection based on keywords

tit.abs.keyw <- paste(data.bib$TI, data.bib$AB, data.bib$ID, data.bib$DE) # Combine title, abstract and keyword information into one column for grep to search

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

exclude.list <- read.csv("../raw_data/step_1_excluded_keywords.csv", stringsAsFactors = FALSE)  # Load list of to-be-excluded terms, as decided on by the authors

exclude.words <- exclude.list$Term[exclude.list$exclude...final=="yes"]
exclude.words <- paste0("\\b", exclude.words, "\\b")  # Define list of terms to exclude. "\\b" wrappers makes sure only whole words are matched
include.l <- grepl(pattern = paste(include, collapse = "|"), x = tit.abs.keyw, ignore.case = TRUE)  # identify indexes in data matching term(s)
exclude.l <- grepl(pattern = paste(exclude.words, collapse = "|"), x = tit.abs.keyw, ignore.case = TRUE)  # identify indexes in data matching term(s)
data.bib <- data.bib[include.l & !exclude.l,]  # filter rows based on identified terms
data.bib <- data.bib[!duplicated(data.bib$DI),]  # Exclude duplicate records, identified through duplicate DOIs. The higher row index duplicate will be excluded.

rm(include.l, exclude.l, exclude.list, exclude.words, include, exclude, tit.abs.keyw)

# transpose codebook to facilitate easy look-up of variable definitions

codebook <- as.data.frame(t(codebook))

# Set numeric variables to numeric in data.bib and data.all

data.bib$TC <- as.numeric(data.bib$TC)
data.bib$TC_2020 <- as.numeric(data.bib$TC_2020)
data.bib$PY <- as.numeric(data.bib$PY)

data.all$TC <- as.numeric(data.all$TC)
data.all$TC_2020 <- as.numeric(data.all$TC_2020)
data.all$PY <- as.numeric(data.all$PY)

# Remove excluded rows from data.all

data.all <- data.all[is.na(data.all$excluded),]

# Add missing values in TC_2020 and PY to data.all and recalculate RV

missing.data.all <- read.csv("../raw_data/studies_missing_TC_2020_and_PY.csv")  # missing PY and TC_2020 values, manually identified in WoS

for (i in 1:nrow(data.all)) {
  if (is.na(data.all$PY[i]))
    data.all$PY[i] <- missing.data.all$PY[which(missing.data.all$UT==data.all$UT[i])[1]]
  
  if (is.na(data.all$TC_2020[i]))
    data.all$TC_2020[i] <- missing.data.all$TC_2020[which(missing.data.all$UT==data.all$UT[i])[1]]
  
}

rm(missing.data.all, i)

# Calculate RV for data.all

data.all$years.since.pub <- 2020 - data.all$PY
data.all$RV <- (data.all$TC_2020 / (data.all$years.since.pub + 1)) * (1 / data.all$sample_size)  # Calculate RV for all records in data.all

# Trim data.irr down to relevant variables

data.irr <- select(data.irr, 
                   UT, DI, AU, TI, PY, 
                   coder, sample_size_orig, sample_size_BA, sample_size_PhD, sample_size_final,
                   matches_orig_BA, matches_orig_PhD, matches_BA_PhD, matches_all, irr_resolver, irr_resolver_comment)

saveRDS(data.bib, file = "../processed_data/data_bib.Rds")
write.csv(data.bib, file = "../processed_data/data_bib.csv", row.names = F)
saveRDS(data.all, file = "../processed_data/data_all.Rds")
write.csv(data.all, file = "../processed_data/data_all.csv", row.names = F)
saveRDS(data.irr, file = "../processed_data/data_irr.Rds")
write.csv(data.irr, file = "../processed_data/data_irr.csv", row.names = F)
