# Read dataset A


data.A <- readRDS(file = "../raw_data/dataset_A_data.rds")

# Wrangle dataset 

## Filter out excluded rows
data.A$excluded[is.na(data.A$excluded)] <- 0
data.A.filt <- data.A[data.A$excluded != 1,]

## Filter out records from 2019 - for these records, bibliometric info would not be accurate
data.A.filt <- data.A.filt[-which(data.A.filt$PY == 2019),]

## Reformat key columns
data.A.filt$study_number <- as.factor(data.A.filt$study_number)
data.A.filt$coder <- as.factor(data.A.filt$coder)
data.A.filt$resolver <- as.factor(data.A.filt$resolver)

## Calculate RV

data.A.filt$sample_bins <- cut(as.numeric(data.A.filt$sample_size), breaks = round(seq(1, max(as.numeric(data.A.filt$sample_size), na.rm = T), length.out = 10), 0))

data.A.filt$TC <- as.numeric(data.A.filt$TC)
data.A.filt$PY <- as.numeric(data.A.filt$PY)
data.A.filt$sample_size <- as.numeric(data.A.filt$sample_size)

current.year <- 2019
data.A.filt$RV <- (data.A.filt$TC / (current.year-data.A.filt$PY) ) / (data.A.filt$sample_size - 3)


# Sample 250 rows randomly from dataset A to generate dataset B

set.seed(11282019)  # Set seed to ensure reproducibility
sample.rows <- sample(x = nrow(data.A.filt), size = 250, replace = F)
data.B <- data.A.filt[sample.rows,]

# After discussion, it was decided that we should sample 250 papers (rather than studies) randomly.
# Original random sample includes 247 papers. Therefore, we sample three additional studies from papers not already in the dataset.
# We also randomly exclude studies from the same paper until all papers in dataset B has only 1 paper included in dataset B.

duplicates <- data.B$DI[duplicated(data.B$DI)] # Find rows of papers with more than one study in dataset B

set.seed(12112019)
excluded <- sapply(duplicates, function(i) {  
  studies <- which(data.B$DI == i)
  keep <- sample(1:length(studies), 1)
  studies[-keep]
})

data.B <- data.B[-excluded,] # Remove all but one study from each paper

set.seed(12112019)
added.papers <- sample(data.A.filt$DI[!data.A.filt$DI %in% data.B$DI], 3) # Sample 3 additional unique papers from datast A not already in dataset B

added.studies <- sapply(added.papers, function(i) {  # Sample 1 study from each selected paper
  studies <- which(data.A.filt$DI == i)
  keep <- sample(1:length(studies), 1)
  studies[keep]
})
data.B <- rbind(data.B, data.A.filt[added.studies,]) # Add additional studies to dataset B





# Save dataset B in processed_data/

write.table(file = "../processed_data/dataset_B.tsv", sep = "\t", x = data.B, row.names = FALSE)
saveRDS(object = data.B, file = "../processed_data/dataset_B.rds")  # rds version of this data needed due to file encoding issues with the .tsv file
