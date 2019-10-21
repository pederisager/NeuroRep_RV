setwd("C:/Users/20176239/Dropbox/jobb/PhD/Projects/2019_NeuroRep_Replication_Value/collaborator_directories/peder/sample_size_merge")  # Change path to local version of this git repo

jvb <- read.csv("JvB_complete_dataset_A_neuro.csv", header = T, stringsAsFactors = F)
rvb <- read.csv("RvB_complete_dataset_A_neuro.csv", header = T, stringsAsFactors = F)
gm <- read.csv("dataset_A_coded.csv", header = T, stringsAsFactors = F)[]



# Old checks (data fixed and no longer available)
  # data <- read.csv("all_data_excel_copy_errors.csv", header = T, stringsAsFactors = F)
  # 
  # data_old <- read.csv("dataset_A_old.tsv", header = T, sep =  "\t")
  # 
  # 
  # 
  # #check if all studies in data_old are in data
  # 
  # sum(!(data_old$UT %in% data$UT))  # 295 articles in the old data not in the new data for some reason.
  # 
  # data[!(data$UT %in% data_old$UT),] # OBS!! WOS number gone likely due to overwrite. Also: Study flagged for exclusion with no comment.
  # 
  # missing <- data_old[!(data_old$UT %in% data$UT),]  # the missing records in the new data
  # 
  # length(unique(data_old$UT))
  # length(unique(data$UT))
  # 
  # sum(data$study_number==4)


# fix JvB's data

## Problem: UT and DI has been erroneously overwritten in JvB's data. 
## Solution: Retrieve data from GM's data using TI and PY as unique identifier

jvb.dat <- jvb[jvb$coder == "JvB",]

jvb.UT <- gm[(gm[,"TI"] %in% jvb.dat[,"TI"]),c("UT", "TI")]
jvb.DI <- gm[(gm[,"TI"] %in% jvb.dat[,"TI"]),c("DI", "TI")]

jvb.dat$UT <- NULL
jvb.dat <- merge(jvb.dat, jvb.UT, "TI")

jvb.dat$DI <- NULL
jvb.dat <- merge(jvb.dat, jvb.DI, "TI")

## check that fix seems valid
jvb.dat$study_number[duplicated(jvb.dat[,c("UT", "DI")])]  # Only UT and DI for study numbers larger than 1 are duplicates
sum(!(jvb.dat$UT %in% data_old$UT))  # no UT in old data is missing from jvb

rvb.dat <- rvb[rvb$coder == "RvB",]
rvb.dat$study_number[duplicated(rvb.dat[,c("UT", "DI")])]  # Only UT and DI for study numbers larger than 1 are duplicates
sum(!(rvb.dat$UT %in% data_old$UT))  # no UT in old data is missing from rvb

# merge data

all_data <- gm
all_data$X <- NULL
all_data$X.1 <- NULL
all_data <- all_data[!(all_data$UT %in% jvb.dat$UT),]  # remove data contained in jvb
all_data <- all_data[!(all_data$UT %in% rvb.dat$UT),]  # remove data contained in rvb
all_data <- rbind(all_data, jvb.dat, rvb.dat)

sum(!(data_old$UT %in% all_data$UT)) # no data in old data is missing from new merged data


write.csv(all_data, "all_data_excel_copy_errors.csv")  


# check for missing sample size data

sum(all_data$sample_size != "")
length(unique(
  (all_data[all_data$sample_size != "", c("DI", "sample_size")])$DI)
)


# Erroneous DOI issue

length(unique(all_data$UT)) - length(unique(all_data$DI))  # OBS!! More unique DIs than UTs should not happen!
x <- unique(all_data[,c("UT", "DI")])
x[duplicated(x$UT),] # DOIs that were destroyed by Excel instead of copied. Stupid Excel! 



# Fixing copy errors from Excel manually in all_data_excel_copy_errors_fixed

## Fixing manually

all_data_fixed <- read.csv("all_data_excel_copy_errors_fixed.csv", header = T, sep =  ",")  # loading fixed data

length(unique(all_data_fixed$UT)) - length(unique(all_data_fixed$DI))  # No discrepancy after manual fix


# Final estimate of number of unique articles in sample. 
length(unique(
  (all_data_fixed[all_data_fixed$sample_size != "", c("DI", "sample_size")])$DI)
)

