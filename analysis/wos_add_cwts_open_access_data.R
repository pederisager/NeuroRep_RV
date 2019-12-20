
# load data to be merged
wos.data <- readRDS("../processed_data/metadata_WoS/wos_all_records_data.Rds")
thed.data <- read.csv("../raw_data/source_data/metadata_CWTS/OA_data_from_Thed_for_all_wos_records.csv", header = T)

# check that UT column is identical across datasets
names(thed.data)
thed.wos.data <- thed.data[,names(thed.data)[-(71:75)]]
comp <- thed.wos.data$UT == wos.data$UT
summary(comp)

# select data to be extracted
cwts.oa.data <- thed.data[,c("p_oa", "p_oa_gold", "p_oa_bronze", "p_oa_hybrid", "p_oa_green")]

#merge oa data with wos data
wos.cwts.merged <- cbind(wos.data, cwts.oa.data)

# check that UT and open access info matches up with thed.data
summary(wos.cwts.merged[,c("UT", "p_oa", "p_oa_gold", "p_oa_bronze", "p_oa_hybrid", "p_oa_green")] == thed.data[,c("UT", "p_oa", "p_oa_gold", "p_oa_bronze", "p_oa_hybrid", "p_oa_green")])

#save as RDS file
saveRDS(wos.cwts.merged, "../processed_data/wos_all_records_data+cwts_open_access.Rds")
