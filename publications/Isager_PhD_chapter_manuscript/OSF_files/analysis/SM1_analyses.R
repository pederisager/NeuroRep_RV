library(tidyverse)

pilot.dat <- read.csv("../processed_data/pilot_expert_survey/NeuroRep_Survey_Pilot_data_2020-10-15_cleaned.csv")

# Make summary table

## retrieve data to average over
imp.dat <- pilot.dat[, grep("_imp", names(pilot.dat))]
rank.dat <- pilot.dat[, grep("_rank", names(pilot.dat))]

## create table dataframe
sum.tab <- data.frame(matrix(0, 20, 5))
names(sum.tab) <- c("factor", "n rating", "median rating", "n rank", "median rank")
sum.tab$factor <- c("sample size", "participants excluded", "statistical power", "effect size", "cluster extent", "cluster p-value", "main effect or interaction",
                    "condition assignment", "replication result", "replication close or not", "replication independent or not", 
                    "within or between design", "cluster peak Z-value", "open data available", "preregistered", "statistical errors", 
                    "strongly connected to theory", "effect predicted or exploratory", "participant sampling", "effect unexpected")
sum.tab[, "n rating"] <- sapply(imp.dat, function(x) sum(!is.na(x))) 
sum.tab[, "median rating"] <- sapply(imp.dat, median, na.rm=T)
sum.tab[, "n rank"] <- 10
sum.tab[1, "median rank"] <- median(rank.dat$sam_siz_rank, na.rm = T)
sum.tab[2, "median rank"] <- median(rank.dat$par_exc_rank, na.rm = T)
sum.tab[3, "median rank"] <- median(rank.dat$stat_pow_rank, na.rm = T)
sum.tab[4, "median rank"] <- median(rank.dat$eff_siz_rank, na.rm = T)
sum.tab[5, "median rank"] <- median(rank.dat$clu_ext_rank, na.rm = T)
sum.tab[6, "median rank"] <- median(rank.dat$clu_pval_rank, na.rm = T)
sum.tab[7, "median rank"] <- median(rank.dat$main_int_rank, na.rm = T)
sum.tab[8, "median rank"] <- median(rank.dat$con_ass_rank, na.rm = T)
sum.tab[9, "median rank"] <- median(rank.dat$rep_res_rank, na.rm = T)
sum.tab[10, "median rank"] <- NA  # Replication closeness was not ranked
sum.tab[11, "median rank"] <- NA  # Replication independent was not ranked
sum.tab[12, "median rank"] <- median(rank.dat$wit_bet_rank, na.rm = T)
sum.tab[13, "median rank"] <- median(rank.dat$clu_zpeak_rank, na.rm = T)
sum.tab[14, "median rank"] <- median(rank.dat$open_dat_rank, na.rm = T)
sum.tab[15, "median rank"] <- median(rank.dat$pre_reg_rank, na.rm = T)
sum.tab[16, "median rank"] <- median(rank.dat$stat_err_rank, na.rm = T)
sum.tab[17, "median rank"] <- median(rank.dat$the_con_rank, na.rm = T)
sum.tab[18, "median rank"] <- median(rank.dat$pred_exp_rank, na.rm = T)
sum.tab[19, "median rank"] <- median(rank.dat$par_sam_rank, na.rm = T)
sum.tab[20, "median rank"] <- median(rank.dat$unexp_rank, na.rm = T)
sum.tab <- sum.tab[order(-sum.tab[,3]),]

#write.csv(sum.tab, "../processed_data/pilot_expert_survey/pilot_summary_table.csv", row.names = F)

# Plot histograms of raw data
names(imp.dat) <- c("sample size", "participants excluded", "statistical power", "effect size", "cluster extent", "cluster p-value", "main or interaction",
                    "condition assignment", "replication result", "replication close", "replication independent", 
                    "within or between", "cluster peak Z", "open data", "preregistered", "statistical errors", 
                    "connected to theory", "predicted or exploratory", "participant sampling", "effect unexpected")
ggplot(gather(imp.dat), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key) +
  theme_bw()


names(rank.dat) <- c("sample size", "participants excluded", "statistical power", "effect size", "cluster peak Z", "cluster extent", "cluster p-value", 
                     "main or interaction", "within or between design", "open data", "statistical errors", "strongly connected to theory", 
                     "predicted or exploratory", "participant sampling", "condition assignment", "replication result", "preregistered", "effect unexpected")
ggplot(gather(rank.dat), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key) +
  theme_bw()