################################
# Analysis script for analyses #
# reported in main manuscript  #
# of Isager thesis chapter 5   #
################################


#### Load packages and data ####

setwd("./analysis")  # Replace "./" with path to root directory on local computer where relevant analysis and data files are stored.

library(tidyverse)
library(psych)
library(stringr)
library(gridExtra)
library(corrplot)
library(knitr)
library(pander)
library(RVAideMemoire)

codebook <- read.table(file = "codebook.tsv", header = T, sep = "\t")  # Codebook with variable descriptions of all variables in data.bib, data.all, and data.irr
data.bib <- readRDS("data_bib.Rds")  # Bibliometric data for all articles that survived keyword exclusion
data.all <- readRDS("data_all.Rds")  # Bibliometric data + sample size of all individual studies selected for sample size coding
data.irr <- readRDS("data_irr.Rds")  # Sample size inter-rater reliability data for sample of studies selected for triple-coding







#### Journals most frequently published in ####

# Count frequencies 

jou.freq <- as.data.frame(tail(sort(table(data.bib$SO)), 20))
levels(jou.freq$Var1) <- sub("PROCEEDINGS OF THE NATIONAL ACADEMY OF SCIENCES OF THE UNITED STATES OF AMERICA", "PNAS", levels(jou.freq$Var1))
jou.freq <- jou.freq[order(jou.freq$Freq, decreasing = T),]
jou.freq[,1] <- str_to_title(tolower(jou.freq[,1]))
jou.freq[[13,1]] <- "PNAS"
jou.freq[[4,1]] <- "PLOS One"

# Plot frequencies

g.jou.freq <- ggplot(jou.freq, aes(x = Var1, y = Freq)) +
  geom_col() +
  labs(title="Journals most frequently published in",
       x = NULL,
       y = "Frequency") +
  coord_flip() +
  theme_bw()

# Count unique journals in dataset

n.journals <- nrow(unique(select(data.bib, SO)))  







#### WoS research fields most frequently tagged ####

# Count frequencies 

field.freq <- as.data.frame(tail(sort(table(data.bib$WC)), 20))
field.freq <- field.freq[order(-field.freq$Freq),]
names(field.freq) <- c("field", "frequency")

# Plot frequencies

g.field.freq <- ggplot(field.freq, aes(x = field, y = frequency)) +
  geom_col() +
  labs(title="WoS research fields most frequently tagged",
       x = NULL,
       y = "Frequency") +
  coord_flip() +
  theme_bw()

# Count unique WoS categories in dataset

n.woscats <- nrow(unique(select(data.bib, WC)))  







#### Most frequent primary cluster labels ####

# Count frequencies

labels <- c(data.bib$label1, 
            data.bib$label2, 
            data.bib$label3, 
            data.bib$label4, 
            data.bib$label5)  # Combine all labels into one string variable

lab.freq <- as.data.frame(tail(sort(table(labels)), 50))  # count prevalence of each string and save 50 most prevalent
lab.freq <- lab.freq[order(-lab.freq$Freq),]
names(lab.freq) <- c("label", "frequency")

# Plot frequencies

g.lab.freq <- ggplot(lab.freq, aes(x = label, y = frequency)) +
  geom_col() +
  labs(title="Most frequent primary cluster labels",
       x = NULL,
       y = "Frequency") +
  coord_flip() +
  theme_bw()

n.labels.total <- length(unique(labels))

# Count unique clusters/subfields contained in set

n.clusters <- nrow(unique(select(data.bib, cluster_id1, n_pubs)))  

# Plot distribution of cluster size

clu.siz <- unique(select(data.bib, cluster_id1, n_pubs))

g.clu.siz <- ggplot(clu.siz, aes(x = n_pubs)) +
  geom_histogram() +
  labs(title="Distribution of cluster/subfield size",
       x = "Number of CWTS records included in cluster",
       y = "Frequency") +
  theme_bw()

# Cluster size distribution summary

clu.summary <- describe(data.bib$n_pubs)







#### Citation metrics table ####

metric.names <- c("WoS", "Crossref", "Scopus", "CWTS", "CWTS normalized", "Altmetric", "Total")  # Set variable names
metric.desc <- c(codebook[81, 2], codebook[78, 2], codebook[79, 2], codebook[89, 2], codebook[91, 2], codebook[80, 2], "Number of articles for which all citation metrics were available")  # Get variable descriptions
metric.n <- c(sum(!is.na(data.bib$TC_2020)),
              sum(!is.na(data.bib$crossref_citations)),
              sum(!is.na(data.bib$scopus_citations)),
              sum(!is.na(data.bib$tcs)),
              sum(!is.na(data.bib$tncs)),
              sum(!is.na(data.bib$altmetric_score)),
              sum(complete.cases(data.bib[, c("TC_2020", "scopus_citations", "crossref_citations", "altmetric_score", "tcs", "tncs")])))  # Count sample size for each variable in data.bib

t.cit.metrics <- data.frame("citation metric" = metric.names, 
                            "description" = metric.desc, 
                            "N" = metric.n)  # Combine information above in a table







#### Citation metric reliability across sources ####

# Citation metric distributions

g.raw <- ggplot(data = data.all) +
  geom_density(aes(x = TC_2020), col = "black") +
  geom_density(aes(x = crossref_citations), col = "red") + 
  geom_density(aes(x = scopus_citations), col = "blue") +
  geom_density(aes(x = tcs), col = "orange") +
  theme_bw() +
  labs(title="A", x="raw citation score", y= "")

g.tncs <- ggplot(data = data.all, aes(x = tncs)) +
  geom_density() +
  theme_bw()+
  labs(title="B", x="CWTS cluster-normalized citation score", y= "")

g.alt <- ggplot(data = data.all, aes(x = altmetric_score)) +
  geom_density() +
  theme_bw()+
  xlim(0,500) +
  labs(title="C", x="Altmetric score", y= "")

# Citation metrics correlation matrix

cor.dat <- select(.data = data.bib, 
                  TC_2020, 
                  crossref_citations, 
                  scopus_citations,
                  tcs,
                  tncs,
                  altmetric_score)
names(cor.dat) <- metric.names
cor.mat <- cor(cor.dat, use = "pairwise.complete.obs", method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Set color scheme for plot in manuscript

# Intra-class correlation analysis

icc.cit <- ICC(data.bib[, c("TC_2020", "crossref_citations", "scopus_citations", "tcs")])







#### Relationship between article age and citation metrics ####

# Correlation matrix

cor.dat <- select(.data = data.bib, 
                  PY,
                  TC_2020, 
                  altmetric_score)
cor.dat$PY <- 2020-cor.dat$PY
cor.dat$TC_2020_by_year <- cor.dat$TC_2020/cor.dat$PY
cor.dat$altmetric_score_by_year <- cor.dat$altmetric_score/cor.dat$PY
cor.dat$tncs <- data.bib$tncs
names(cor.dat) <- c("age", "WoS", "Altmetric", "WoS by year", "Altmetrics by year", "CWTS normalized")
cor.mat <- cor(cor.dat, use = "pairwise.complete.obs", method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Set color scheme for plot in manuscript







#### Sample size inter-rater reliability analysis ####

# Percentage exact agreement between coders

all.match <- sum(data.irr$matches_all)/nrow(data.irr)  # agreement between all coders
orig.BA.match <- sum(data.irr$matches_orig_BA)/nrow(data.irr)  # agreement between original coder and undergrad double-coder
orig.PhD.match <- sum(data.irr$matches_orig_PhD)/nrow(data.irr)  # agreement between original coder and PhD double-coder
BA.PhD.match <- sum(data.irr$matches_BA_PhD)/nrow(data.irr)  # agreement between undergrad double-coder and PhD double-coder
## Matches between coders and final coded sample size after resolving disagreements
nonmatches <- data.irr[data.irr$matches_all==0,]
orig.final.match <- sum(nonmatches$sample_size_orig==nonmatches$sample_size_final)  # original coder
BA.final.match <- sum(nonmatches$sample_size_BA==nonmatches$sample_size_final)  # undergrad double-coder
PhD.final.match <- sum(nonmatches$sample_size_PhD==nonmatches$sample_size_final)  # PhD double-coder

# Intra-class correlation analysis

icc <- ICC(data.irr[, c("sample_size_orig", "sample_size_BA", "sample_size_PhD")], lmer = T)

# Plot relationship between each pair of coders

df.ba <- data.frame(sample_size_orig=data.irr$sample_size_orig, sample_size=data.irr$sample_size_BA, coder="undergrad")
df.phd <- data.frame(sample_size_orig=data.irr$sample_size_orig, sample_size=data.irr$sample_size_PhD, coder="PhD")
df.irr <- rbind(df.ba, df.phd)

g.irr <- ggplot(data=df.irr, aes(x=sample_size_orig, y=sample_size, col=coder, shape=coder)) +
  geom_point(alpha = 0.6, size=3) +
  theme_bw() +
  labs(col = "double-coder", shape = "double-coder") +
  scale_x_continuous(trans='log10', name = "sample size, coded by original coder") +
  scale_y_continuous(trans='log10', name = "double-coded sample size")

# Histogram of final sample size distribution

g.samplesize <- ggplot(data = data.all, aes(x=sample_size)) +
  geom_histogram(bins = 50) +
  xlim(c(1, 500)) +
  theme_bw()







#### Replication value analyses #### 

# Calculate RV_WoS and RV_alt for studies in data.all

data.all$RV <- (data.all$TC_2020/ (data.all$years.since.pub + 1) ) * (1/data.all$sample_size)
data.all$RV_alt <- data.all$altmetric_score * (1/data.all$sample_size)

# Spearman correlation between the replication value estimators. Bootstrap confidence intervals based on 10000 simulations.

set.seed(20210722)
RVcor <- spearman.ci(data.all$RV, data.all$RV_alt, nrep = 10000, conf.level = 0.95)

# Plot relationship between RV_WoS and RV_alt

## Grab highest replication value points to be colored in the plot
data.all$highestRV <- 0
data.all$highestRV[which(data.all$RV >= tail(head(data.all$RV[order(-data.all$RV)], 10), 1))] <- 1  # mark 10 highest RV_WoS scores
data.all$highestRV_alt <- 0
data.all$highestRV_alt[which(data.all$RV_alt >= tail(head(data.all$RV_alt[order(-data.all$RV_alt)], 10), 1))] <- 1  # mark 10 highest RV_alt scores
highestRV <- data.all %>% filter(RV >= tail(head(data.all$RV[order(-data.all$RV)], 10), 1))
highestRV_alt <- data.all %>% filter(RV_alt >= tail(head(data.all$RV_alt[order(-data.all$RV_alt)], 10), 1))
highestRV_both <- highestRV %>% filter(RV_alt >= tail(head(data.all$RV_alt[order(-data.all$RV_alt)], 10), 1))
## enerate plot with RV_WoS and RV_alt distributions as bars on the axes
RVcor.p <- ggplot(data = data.all, aes(x = RV, y = RV_alt)) +
  geom_rug() +
  geom_rug(data = highestRV, color="brown1", sides="b", size=1) +
  geom_rug(data = highestRV_alt, color="cornflowerblue", sides="l", size=1) +
  geom_point() +
  geom_point(data = highestRV, aes(x = RV, y = RV_alt), color = "white", size = 3) +
  geom_point(data = highestRV, aes(x = RV, y = RV_alt), color = "brown1", size = 3) +
  geom_point(data = highestRV_alt, aes(x = RV, y = RV_alt), color = "cornflowerblue", size = 3) +
  geom_point(data = highestRV_both, aes(x = RV, y = RV_alt), color = "darkorchid1", size = 4) +
  theme_bw() + 
  theme(axis.title=element_text(size=16,face="bold")) +
  labs(x=expression(RV["WoS"]), y=expression(RV["Alt"])) 
