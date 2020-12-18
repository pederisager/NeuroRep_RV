
# Load packages

library(tidyverse)
library(psych)

# Load relevant datasets

df.orig <- readRDS("../raw_data/sample_size_inter_rater_reliability/dataset_B_samplesize_interrater_reliability_original.rds")  # Sample size coded by the original (student) coders

df.BA <- read.csv(file = "../raw_data/sample_size_inter_rater_reliability/dataset_B_samplesize_interrater_reliability_doublecoded_by_BA.tsv", 
                  header = TRUE, 
                  sep = "\t", 
                  quote = "")  # Sample size double-coded by bachelor student


df.PhD <- read.table(file = "../raw_data/sample_size_inter_rater_reliability/dataset_B_samplesize_interrater_reliability_doublecoded_by_PhD.tsv", 
                     header = TRUE, 
                     sep = "\t", 
                     quote = "")  # Sample size double-coded by first author (PhD student)


# Create output dataset

df.out <- df.orig
names(df.out)[8] <- "sample_size_orig"

## Combine sample size columns into one dataset

### Check that all three datasets are ordered identically
sum(df.BA$UT==df.PhD$UT)==nrow(df.BA) 
sum(df.BA$UT==df.orig$UT)==nrow(df.BA)

### Combine

df.out$sample_size_BA <- df.BA$sample_size
df.out$sample_size_PhD <- df.PhD$sample_size

## Create dummy columns to denote how often sample size was coded identically

matches <- sapply(list(df.PhD$sample_size, df.BA$sample_size), function(x) x == df.orig$sample_size)  # checks if df.BA and df.PhD sample sizes are identical to df.orig. Returns 1 col per comparison
df.out$matches_orig_BA <- as.numeric(matches[,2])
df.out$matches_orig_PhD <- as.numeric(matches[,1])
df.out$matches_BA_PhD <- ifelse(df.PhD$sample_size == df.BA$sample_size, 1, 0)
df.out$matches_all <- matches[,1]*matches[,2]

## Create "n_final" column that will contain the final sample size after resolving disagreements. Populate this variable with cases where all coders agreed.

df.out$sample_size_final <- ifelse(df.out$matches_all == 1, df.out$sample_size_orig, NA)

## Load data file with resolved discrepancies and final sample size

df.resolved <- read.csv(file = "../raw_data/dataset_B_inter_rater_reliability_discrepancies_resolved_by_PhD.tsv",
                        header = TRUE, 
                        sep = "\t")

df.out <- merge(df.out[, -91], df.resolved[, c("UT", "sample_size_final", "irr_resolver", "irr_resolver_comment")], by = "UT")  # add final sample size back into df.out after manually resolving coder disagreements.

## Save output dataset to file

write.table(x = df.out, 
            file = "../processed_data/dataset_B_inter_rater_reliability.tsv", 
            sep = "\t", 
            row.names = FALSE)

saveRDS(object = df.out, 
        file = "../processed_data/dataset_B_inter_rater_reliability.Rds")


# Calculate inter-rater reliability

## Percentage exact agreement
all.match <- sum(df.out$matches_all)/nrow(df.out)
orig.BA.match <- sum(df.out$matches_orig_BA)/nrow(df.out)
orig.PhD.match <- sum(df.out$matches_orig_PhD)/nrow(df.out)
BA.PhD.match <- sum(df.out$matches_BA_PhD)/nrow(df.out)

## Correlation 

ICC(df.out[, c("sample_size_orig", "sample_size_BA", "sample_size_PhD")])

## Discrepancies between each coder and the final sample size
cor(df.out$sample_size_orig, df.out$sample_size_final)
sum(df.out$sample_size_orig == df.out$sample_size_final)/250  # count number of times coder agreed with final sample size 
sum(df.out[df.out$matches_all==0,]$sample_size_orig == df.out[df.out$matches_all==0,]$sample_size_final)/sum(df.out$matches_all==0)  # count number of times this coder was right when someone was wrong
plot(df.out[sample_size_final], df.out$sample_size_orig, log = "xy")

cor(df.out$sample_size_BA, df.out$sample_size_final)
sum(df.out$sample_size_BA == df.out$sample_size_final)/250  # count number of times coder agreed with final sample size 
sum(df.out[df.out$matches_all==0,]$sample_size_BA == df.out[df.out$matches_all==0,]$sample_size_final)/sum(df.out$matches_all==0)  # count number of times this coder was right when someone was wrong
plot(df.out$sample_size_final, df.out$sample_size_BA, log = "xy")

cor(df.out$sample_size_PhD, df.out$sample_size_final)
sum(df.out$sample_size_PhD == df.out$sample_size_final)/250  # count number of times coder agreed with final sample size 
sum(df.out[df.out$matches_all==0,]$sample_size_PhD == df.out[df.out$matches_all==0,]$sample_size_final)/sum(df.out$matches_all==0)  # count number of times this coder was right when someone was wrong
plot(df.out$sample_size_final, df.out$sample_size_PhD, log = "xy")

# Plot correspondence between coders

df.plot <- rbind(df.BA, df.PhD)
df.plot$orig_sample_size <- c(df.orig$sample_size, df.orig$sample_size)

ggplot(data = df.plot, aes(x = orig_sample_size, 
                          y = sample_size, 
                          col = coder)) +
  geom_point(alpha = 0.4) + 
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  theme_bw() +
  scale_color_manual(values=c("#E69F00", "#56B4E9"), name = "Coder", labels = c("BA", "PhD")) +
  xlab("Originally coded sample size") + ylab("Double-coded sample size")
