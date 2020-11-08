
#### NEW ####

# load data to be merged

wos.data <- readRDS("../processed_data/metadata_WoS/wos_all_records_data.Rds")

cwts.clusters <- read.csv("../raw_data/CWTS_cluster_information.csv")
cwts.metrics <- read.csv("../raw_data/CWTS_research_metrics.csv")
cwts.oa.status <- read.csv("../raw_data/CWTS_open_access_information.csv")


# convert string percentages to numeric proportions in cwts.metrics

## replace "NULL" with NA
for (i in 1:ncol(cwts.metrics)) {
  cwts.metrics[cwts.metrics[,i]=="NULL",i] <- NA
}

## Convert percentages to proportions
for (i in 6:ncol(cwts.metrics)) {
  cwts.metrics[,i] <- as.numeric(sub("%", "", cwts.metrics[,i]))/100
}

cwts.metrics$tcs <- as.numeric(cwts.metrics$tcs)
cwts.metrics$mcs <- as.numeric(cwts.metrics$mcs)


# Organize CWTS data for merging

cwts.clusters <- cwts.clusters[, -1]
names(cwts.clusters)[1] <- "UT"
cwts.clusters <- unique(cwts.clusters)

cwts.metrics <- cwts.metrics[, -c(1, 3)]
names(cwts.metrics)[1] <- "UT"

cwts.oa.status <- cwts.oa.status[, -c(1, 3, 4)]
names(cwts.oa.status)[1] <- "UT"
cwts.oa.status <- unique(cwts.oa.status)


# Merge

data <- merge(wos.data, cwts.clusters, by = "UT", all.x = T)
data <- merge(data, cwts.metrics, by = "UT", all.x = T)
data <- merge(data, cwts.oa.status, by = "UT", all.x = T)


# Save output

saveRDS(data, "../processed_data/wos+cwts_all_records_data.Rds")
write.csv(data, "../processed_data/wos+cwts_all_records_data.csv", row.names = FALSE)

