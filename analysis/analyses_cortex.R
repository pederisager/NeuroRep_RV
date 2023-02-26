#############################################################
# Analysis script for analyses reported in main manuscript  #
# of Isager et al Selecting Studies for Replication in      #
# Social Neuroscience: Exploring a Formal Approach          #
#############################################################

#### Load packages ####

library(tidyverse)
library(psych)
library(stringr)
library(gridExtra)
library(corrplot)
library(knitr)
library(pander)
library(RVAideMemoire)

#### Load data #### 

#setwd("C:/Users/pedisa94/Dropbox/jobb/PhD/Projects/2019_NeuroRep_Replication_Value/publications/Isager_PhD_chapter_manuscript/OSF_files/analysis/")

# source("load_and_clean_data.R")  # Can be run to generate datasets from raw data files


codebook <- read.csv("OSF_files/raw_data/codebook.csv")  # Codebook with variable descriptions of all variables in data.bib, data.all, and data.irr
data.bib <- readRDS("OSF_files/processed_data/data_bib.Rds")  # Bibliometric data for all articles that survived keyword exclusion
data.all <- readRDS("OSF_files/processed_data/data_all.Rds")  # Bibliometric data + sample size of all individual studies selected for sample size coding
data.irr <- readRDS("OSF_files/processed_data/data_irr.Rds")  # Sample size inter-rater reliability data for sample of studies selected for triple-coding

# Add total citation count from Scite data to data.bib
data.scite <- read.csv("OSF_files/processed_data/data_scite.csv")  # Dataset containing total Scite citation count
data.bib <- left_join(data.bib, data.scite[, c("target_doi", "scite_total_citation_count")], by = c("DI" = "target_doi")) %>% unique()
data.all <- left_join(data.all, data.scite[, c("target_doi", "scite_total_citation_count")], by = c("DI" = "target_doi")) %>% unique()

#### Journals most frequently published in ####

# Count frequencies 

jou.freq <- as.data.frame(tail(sort(table(data.bib$SO)), 20))
levels(jou.freq$Var1) <- sub("PROCEEDINGS OF THE NATIONAL ACADEMY OF SCIENCES OF THE UNITED STATES OF AMERICA", "PNAS", levels(jou.freq$Var1))
jou.freq <- jou.freq[order(jou.freq$Freq, decreasing = T),]
jou.freq[,1] <- str_to_title(tolower(jou.freq[,1]))
jou.freq[[13,1]] <- "PNAS"
jou.freq[[4,1]] <- "PLOS One"
names(jou.freq) <- c("Journal", "Frequency")

# Count unique journals in dataset

n.journals <- nrow(unique(select(data.bib, SO)))  
perc.articlesintopjournals <- sum(jou.freq$Frequency)/nrow(data.bib)

#### WoS research fields most frequently tagged ####

# Count frequencies 

field.freq <- as.data.frame(tail(sort(table(data.bib$WC)), 20))
field.freq <- field.freq[order(-field.freq$Freq),]
names(field.freq) <- c("Field", "Frequency")

# Count unique WoS categories in dataset

n.woscats <- nrow(unique(select(data.bib, WC)))  
perc.articlesintopwoscats <- sum(field.freq$Frequency)/nrow(data.bib)

#### Most frequent primary cluster labels ####

# Count frequencies

labels <- c(data.bib$label1, 
            data.bib$label2, 
            data.bib$label3, 
            data.bib$label4, 
            data.bib$label5)  # Combine all labels into one string variable

lab.freq <- as.data.frame(tail(sort(table(labels)), 50))  # count prevalence of each string and save 50 most prevalent
lab.freq <- lab.freq[order(-lab.freq$Freq),]
names(lab.freq) <- c("Label", "Frequency")

# Count unique cluster labels contained in set

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

metric.names <- c("WoS", "Crossref", "Scopus", "CWTS", "CWTS normalized", "scite", "Altmetric", "Total")  # Set variable names
metric.desc <- c(codebook[81, 2], 
                 codebook[78, 2], 
                 codebook[79, 2], 
                 codebook[89, 2], 
                 codebook[91, 2],
                 codebook[113, 2], 
                 codebook[80, 2],
                 "Number of articles for which all citation metrics were available")  # Get variable descriptions
metric.n <- c(sum(!is.na(data.bib$TC_2020)),
              sum(!is.na(data.bib$crossref_citations)),
              sum(!is.na(data.bib$scopus_citations)),
              sum(!is.na(data.bib$tcs)),
              sum(!is.na(data.bib$tncs)),
              sum(!is.na(data.bib$scite_total_citation_count)),
              sum(!is.na(data.bib$altmetric_score)),
              sum(complete.cases(data.bib[, c("TC_2020", "scopus_citations", "crossref_citations", "altmetric_score", "tcs", "tncs", "scite_total_citation_count")])))  # Count sample size for each variable in data.bib

t.cit.metrics <- data.frame("citation metric" = metric.names, 
                            "description" = metric.desc, 
                            "N" = metric.n)  # Combine information above in a table


#### Citation metric reliability across sources ####

# Citation metric distributions

g.raw <- ggplot(data = data.bib) +
  geom_density(aes(x = TC_2020), col = "black") +
  geom_density(aes(x = crossref_citations), col = "red") + 
  geom_density(aes(x = scopus_citations), col = "blue") +
  geom_density(aes(x = tcs), col = "orange") +
  geom_density(aes(x = scite_total_citation_count), col = "darkgreen") +
  theme_bw() +
  labs(title="A", x="raw citation score", y= "")

g.tncs <- ggplot(data = data.bib, aes(x = tncs)) +
  geom_density() +
  theme_bw()+
  labs(title="B", x="CWTS field-normalized citation score", y= "")

g.alt <- ggplot(data = data.bib, aes(x = altmetric_score)) +
  geom_density() +
  theme_bw()+
  xlim(0,200) +
  labs(title="C", x="Altmetric score", y= "")

# Citation metrics correlation matrix

cor.dat.citations <- select(.data = data.bib, 
                            TC_2020, 
                            crossref_citations, 
                            scopus_citations,
                            tcs,
                            tncs,
                            scite_total_citation_count,
                            altmetric_score)
names(cor.dat.citations) <- metric.names[-length(metric.names)]  # Remove "Total" from names
cor.mat.citations <- cor(cor.dat.citations, use = "pairwise.complete.obs", method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Set color scheme for plot in manuscript

# corrplot(corr = cor.mat.citations,
#          method = "color",
#          col = col(200),
#          addCoef.col = "black",
#          tl.col="black",
#          tl.srt=45,
#          diag = F)

# Intra-class correlation analysis

icc.cit <- ICC(data.bib[, c("TC_2020", "crossref_citations", "scopus_citations", "tcs", "scite_total_citation_count")])

#### Relationship between article age and citation metrics ####

# Scite data analysis

## Citation trajectory for each article since publication
p.traj_raw <- data.scite %>% 
  mutate(years_since_pub = year - PY) %>%
  ggplot(aes(x = years_since_pub, y = citation_count)) + 
  geom_line(aes(group=target_doi), alpha =0.1) +
  scale_x_continuous(breaks = seq(0, 11, by = 1)) +
  theme_bw() +
  labs(title = "A", 
       x = "years since publication", 
       y = "yearly citations")

## General log(citation+1) trajectory trend
p.traj_log <- data.scite %>% 
  mutate(years_since_pub = year - PY) %>%
  ggplot(aes(x = years_since_pub, y = log(citation_count+1))) + 
  geom_line(aes(group=target_doi), col="grey", alpha = 0.1)+
  geom_boxplot(aes(group=years_since_pub)) +
  scale_x_continuous(breaks = seq(0, 11, by = 1)) +
  scale_y_continuous(breaks = log(c(0, 10, 100)+1), labels = c(0, 10, 100)) +  # need to add +1 to breaks since added +1 to citation count during log transformation
  theme_bw() +
  labs(title = "B", 
       x = "yearly citations", 
       y = "citations obtained in year x")

## Examine whether c/(y+1) up to 2019 is a good predictor of citations in year 2020
data <- data.scite %>%  # compute c/(y+1) up to 2019
  filter(year<=2019) %>%  
  group_by(target_doi) %>% 
  do(CoverYplus1 = sum(.$citation_count)/max(.$years_since_pub+1)) %>% 
  mutate(CoverYplus1 = as.numeric(CoverYplus1))
data$citations2020 <- data.scite %>%  # extract 2020 citation count
  filter(year==2020) %>% 
  .$citation_count
data <- unique(left_join(data, as_tibble(data.scite[, c(1,4,6)]), by = "target_doi"))  # add publication year to summary data
p.c2020_by_cy1 <- data %>%  # plot c/y+1) vs citations in 2020
  ggplot(aes(x = CoverYplus1, y = citations2020)) +
  geom_point(aes(col=PY), alpha = 0.5) + 
  geom_smooth(method = "lm", col="black") +
  scale_color_gradient(low = "red", high = "yellow") +
  theme_bw() +
  labs(title = "C", 
       x = expression(frac(C[scite],Y+1)),
       y = "scite citations in 2020", 
       col = "publication year") 

set.seed(20220616)
c2020_by_cy1 <- spearman.ci(data$CoverYplus1, data$citations2020, nrep = 2000, conf.level = 0.95)
c2020_by_age <- spearman.ci(2019-data$PY, data$citations2020, nrep = 2000, conf.level = 0.95)



# Age citation correlation matrix

cor.dat.age <- select(.data = data.bib, 
                      PY,
                      scite_total_citation_count, 
                      altmetric_score)
cor.dat.age$PY <- 2020-cor.dat.age$PY
cor.dat.age$scite_by_year <- cor.dat.age$scite_total_citation_count/cor.dat.age$PY
cor.dat.age$altmetric_score_by_year <- cor.dat.age$altmetric_score/cor.dat.age$PY
cor.dat.age$tncs <- data.bib$tncs
names(cor.dat.age) <- c("age", "scite", "Altmetric", "scite by year", "Altmetric by year", "CWTS normalized")
cor.mat.age <- cor(cor.dat.age, use = "pairwise.complete.obs", method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Set color scheme for plot in manuscript

spearman.ci(cor.dat.age$age, cor.dat.age$scite, nrep = 2000, conf.level = 0.95)


cit_by_age <- spearman.ci(cor.dat.age$age, cor.dat.age$scite, nrep = 2000, conf.level = 0.95)
ycit_by_age <- spearman.ci(cor.dat.age$age, cor.dat.age$`scite by year`, nrep = 2000, conf.level = 0.95)






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

icc.n <- ICC(data.irr[, c("sample_size_orig", "sample_size_BA", "sample_size_PhD")], lmer = T)

# Plot relationship between each pair of coders

df.ba <- data.frame(sample_size_orig=data.irr$sample_size_orig, sample_size=data.irr$sample_size_BA, coder="undergrad")
df.phd <- data.frame(sample_size_orig=data.irr$sample_size_orig, sample_size=data.irr$sample_size_PhD, coder="PhD")
df.irr <- rbind(df.ba, df.phd)

g.irr <- ggplot(data=df.irr, aes(x=sample_size_orig, y=sample_size, col=coder, shape=coder)) +
  geom_point(alpha = 0.6, size=4) +
  theme_bw() +
  labs(col = "double-coder", shape = "double-coder") +
  scale_x_continuous(trans='log10', name = "number of participants, coded by original coder") +
  scale_y_continuous(trans='log10', name = "double-coded number of participants") +
  scale_color_manual(values=c("#0C7BDC", "#994F00"))

# Histogram of final sample size distribution

g.samplesize <- ggplot(data = data.all, aes(x=sample_size)) +
  geom_histogram(bins = 100, col = "black") +
  labs(x = "Number of participants", y = "Number of studies") +
  xlim(c(1, 500)) +
  theme_bw()

# Sample size mode, median and bins
mode.samplesize <- as.numeric(names(which.max(table(data.all$sample_size))))
median.samplesize <- median(data.all$sample_size)

zerototen <- sum(data.all$sample_size<=10)
tentotwenty <- sum(data.all$sample_size>10 & data.all$sample_size<=20)
twentytothirty <- sum(data.all$sample_size>20 & data.all$sample_size<=30)
thirtytofourty <- sum(data.all$sample_size>30 & data.all$sample_size<=40)
fourtytofifty <- sum(data.all$sample_size>40 & data.all$sample_size<=50)
fiftytosixty <- sum(data.all$sample_size>50 & data.all$sample_size<=60)
sixtytoseventy <- sum(data.all$sample_size>60 & data.all$sample_size<=70)
seventytoeighty <- sum(data.all$sample_size>70 & data.all$sample_size<=80)
eightytoninety <- sum(data.all$sample_size>80 & data.all$sample_size<=90)
ninetytohundred <- sum(data.all$sample_size>90 & data.all$sample_size<=100)
overhundred <- sum(data.all$sample_size>100)






#### Replication value analyses #### 

# Calculate RV_WoS and RV_alt for studies in data.all

# We have the following variables that contain citation data:

# "crossref_citations","Crossref citation counts, downloaded 2020-10-30"
# "scopus_citations","Scopus citation counts, downloaded 2020-10-30"
# "altmetric_score","Altmetric score, downloaded 2020-10-30"
# "TC_2020","Web of Science Core Collection Times Cited Count, updated 2020-11-07"
# "tcs","Total Citation Score. CWTS citation counts - excluding self-citations, downloaded 2020-10-28"
# "tncs","Total Normalized Citation Score. CWTS citation impact of article relative to the primary cluster to which the article belongs. The score represents how many more times the article is cited relative to the average citation count of an article in its cluster from the same year. I.e. An article that is cited 10 times, and that belongs to a cluster in which articles of the same age are cited 4 times on average, will receive a tncs score of 10/4=2.5"


data.all$RV <- (data.all$TC_2020/ (data.all$years.since.pub + 1) ) * (1/data.all$sample_size)
data.all$RV_alt <- data.all$altmetric_score * (1/data.all$sample_size)

data.all$RV_crossref <- (data.all$crossref_citations/ (data.all$years.since.pub + 1) ) * (1/data.all$sample_size)
data.all$RV_scopus <- (data.all$scopus_citations/ (data.all$years.since.pub + 1) ) * (1/data.all$sample_size)
data.all$RV_tcs <- (data.all$tcs/ (data.all$years.since.pub + 1) ) * (1/data.all$sample_size)
data.all$RV_tncs <- (data.all$tncs/ (data.all$years.since.pub + 1) ) * (1/data.all$sample_size)
data.all$RV_scite <- (data.all$scite_total_citation_count/ (data.all$years.since.pub + 1) ) * (1/data.all$sample_size)

cor.dat.rv <- select(.data = data.all, 
                     RV, 
                     RV_crossref, 
                     RV_scopus,
                     RV_tcs, 
                     RV_tncs,
                     RV_scite,
                     RV_alt)
cor.mat.rv <- cor(cor.dat.rv, use = "pairwise.complete.obs", method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Set color scheme for plot in manuscript


corrplot(corr = cor.mat.rv,
         method = "color",
         col = col(200),
         addCoef.col = "black",
         tl.col="black",
         tl.srt=45,
         diag = F)


# Spearman correlation between the replication value estimators. Bootstrap confidence intervals based on 10000 simulations.

set.seed(20210722)
RVcor <- spearman.ci(data.all$RV, data.all$RV_alt, nrep = 2000, conf.level = 0.95)

# Plot relationship between RV_WoS and RV_alt

## Grab highest replication value points to be colored in the plot
top_n <- 10 #Top how many to plot for?
data.all$highestRV <- 0
data.all$highestRV[which(data.all$RV >= tail(head(data.all$RV[order(-data.all$RV)], top_n), 1))] <- 1  # mark top_n highest RV_WoS scores
data.all$highestRV_alt <- 0
data.all$highestRV_alt[which(data.all$RV_alt >= tail(head(data.all$RV_alt[order(-data.all$RV_alt)], top_n), 1))] <- 1  # mark top_n highest RV_alt scores
highestRV <- data.all %>% filter(RV >= tail(head(data.all$RV[order(-data.all$RV)], top_n), 1))
highestRV_alt <- data.all %>% filter(RV_alt >= tail(head(data.all$RV_alt[order(-data.all$RV_alt)], top_n), 1))
highestRV_both <- highestRV %>% filter(RV_alt >= tail(head(data.all$RV_alt[order(-data.all$RV_alt)], top_n), 1))
## Generate plot with RV_WoS and RV_alt distributions as bars on the axes
RVcor.p <- ggplot(data = data.all, aes(x = RV, y = RV_alt)) +
  geom_rug(alpha = 0.2) +
  geom_rug(data = highestRV, color="brown1", sides="b", size=1) +
  geom_rug(data = highestRV_alt, color="cornflowerblue", sides="l", size=1) +
  geom_point(alpha = 0.2) +
  geom_point(data = highestRV, aes(x = RV, y = RV_alt), color = "brown1", size = 2) +
  geom_point(data = highestRV_alt, aes(x = RV, y = RV_alt), color = "cornflowerblue", size = 2) +
  geom_point(data = highestRV_both, aes(x = RV, y = RV_alt), color = "darkorchid1", size = 2) +
  theme_bw() + 
  theme(axis.title=element_text(size=16,face="bold")) +
  labs(x=expression(RV["WoS"]), y=expression(RV["Alt"])) 


# Repeat for RV and other RV's

# Plot relationship between RV_WoS and RV_scopus

## Grab highest replication value points to be colored in the plot

top_n <- 10 #Top how many to plot for?
data.all$highestRV <- 0
data.all$highestRV[which(data.all$RV >= tail(head(data.all$RV[order(-data.all$RV)], top_n), 1))] <- 1  # mark top_n highest RV_WoS scores
data.all$highestRV_scopus <- 0
data.all$highestRV_scopus[which(data.all$RV_scopus >= tail(head(data.all$RV_scopus[order(-data.all$RV_scopus)], top_n), 1))] <- 1  # mark top_n highest RV_scopus scores
highestRV <- data.all %>% filter(RV >= tail(head(data.all$RV[order(-data.all$RV)], top_n), 1))
highestRV_scopus <- data.all %>% filter(RV_scopus >= tail(head(data.all$RV_scopus[order(-data.all$RV_scopus)], top_n), 1))
highestRV_both <- highestRV %>% filter(RV_scopus >= tail(head(data.all$RV_scopus[order(-data.all$RV_scopus)], top_n), 1))

## Generate plot with RV_WoS and RV_scopus distributions as bars on the axes
RVcor.2 <- ggplot(data = data.all, aes(x = RV, y = RV_scopus)) +
  geom_rug() +
  geom_rug(data = highestRV, color="brown1", sides="b", size=1) +
  geom_rug(data = highestRV_scopus, color="cornflowerblue", sides="l", size=1) +
  geom_point(alpha = 0.1) +
  geom_point(data = highestRV, aes(x = RV, y = RV_scopus), color = "brown1", size = 2) +
  geom_point(data = highestRV_scopus, aes(x = RV, y = RV_scopus), color = "cornflowerblue", size = 2) +
  geom_point(data = highestRV_both, aes(x = RV, y = RV_scopus), color = "darkorchid1", size = 2) +
  theme_bw() + 
  theme(axis.title=element_text(size=16,face="bold")) +
  labs(x=expression(RV["WoS"]), y=expression(RV["scopus"])) 

# WoS vs TNCS

top_n <- 10 #Top how many to plot for?
data.all$highestRV <- 0
data.all$highestRV[which(data.all$RV >= tail(head(data.all$RV[order(-data.all$RV)], top_n), 1))] <- 1  # mark top_n highest RV_WoS scores
data.all$highestRV_tncs <- 0
data.all$highestRV_tncs[which(data.all$RV_tncs >= tail(head(data.all$RV_tncs[order(-data.all$RV_tncs)], top_n), 1))] <- 1  # mark top_n highest RV_tncs scores
highestRV <- data.all %>% filter(RV >= tail(head(data.all$RV[order(-data.all$RV)], top_n), 1))
highestRV_tncs <- data.all %>% filter(RV_tncs >= tail(head(data.all$RV_tncs[order(-data.all$RV_tncs)], top_n), 1))
highestRV_both <- highestRV %>% filter(RV_tncs >= tail(head(data.all$RV_tncs[order(-data.all$RV_tncs)], top_n), 1))
## Generate plot with RV_WoS and RV_tncs distributions as bars on the axes
RVcor.3 <- ggplot(data = data.all, aes(x = RV, y = RV_tncs)) +
  geom_rug() +
  geom_rug(data = highestRV, color="brown1", sides="b", size=1) +
  geom_rug(data = highestRV_tncs, color="cornflowerblue", sides="l", size=1) +
  geom_point(alpha = 0.1) +
  geom_point(data = highestRV, aes(x = RV, y = RV_tncs), color = "brown1", size = 2) +
  geom_point(data = highestRV_tncs, aes(x = RV, y = RV_tncs), color = "cornflowerblue", size = 2) +
  geom_point(data = highestRV_both, aes(x = RV, y = RV_tncs), color = "darkorchid1", size = 2) +
  theme_bw() + 
  theme(axis.title=element_text(size=16,face="bold")) +
  labs(x=expression(RV["WoS"]), y=expression(RV["tncs"])) 

# WoS vs Scite

top_n <- 10 #Top how many to plot for?
data.all$highestRV <- 0
data.all$highestRV[which(data.all$RV >= tail(head(data.all$RV[order(-data.all$RV)], top_n), 1))] <- 1  # mark 10 highest RV_WoS scores
data.all$highestRV_scite <- 0
data.all$highestRV_scite[which(data.all$RV_scite >= tail(head(data.all$RV_scite[order(-data.all$RV_scite)], top_n), 1))] <- 1  # mark 10 highest RV_scite scores
highestRV <- data.all %>% filter(RV >= tail(head(data.all$RV[order(-data.all$RV)], top_n), 1))
highestRV_scite <- data.all %>% filter(RV_scite >= tail(head(data.all$RV_scite[order(-data.all$RV_scite)], top_n), 1))
highestRV_both <- highestRV %>% filter(RV_scite >= tail(head(data.all$RV_scite[order(-data.all$RV_scite)], top_n), 1))
## Generate plot with RV_WoS and RV_scite distributions as bars on the axes
RVcor.4 <- ggplot(data = data.all, aes(x = RV, y = RV_scite)) +
  geom_rug() +
  geom_rug(data = highestRV, color="brown1", sides="b", size=1) +
  geom_rug(data = highestRV_scite, color="cornflowerblue", sides="l", size=1) +
  geom_point(alpha = 0.1) +
  geom_point(data = highestRV, aes(x = RV, y = RV_scite), color = "brown1", size = 2) +
  geom_point(data = highestRV_scite, aes(x = RV, y = RV_scite), color = "cornflowerblue", size = 2) +
  geom_point(data = highestRV_both, aes(x = RV, y = RV_scite), color = "darkorchid1", size = 2) +
  theme_bw() + 
  theme(axis.title=element_text(size=16,face="bold")) +
  labs(x=expression(RV["WoS"]), y=expression(RV["scite"])) 

sum(data.all$highestRV)

#### Supplementary material SM2. #### 

# Load raw data
data.survey <- read.csv("OSF_files/raw_data/SM2_survey_data.csv", header = T)
imp.dat <- data.survey[, grep("_imp", names(data.survey))]  ## Ratings
rank.dat <- data.survey[, grep("_rank", names(data.survey))]  ## Ranks
# Load summary table
data.survey.summary <- read.csv("OSF_files/processed_data/SM2_summary_table.csv", header = T)
names(data.survey.summary) <- c("Factor", "N ratings", "Median rating", "N rankings", "Median rank")

# Plot histograms of raw data
names(imp.dat) <- c("sample size", "participants excluded", "statistical power", "effect size", "cluster extent", "cluster p-value", "main or interaction",
                    "condition assignment", "replication result", "replication close", "replication independent", 
                    "within or between", "cluster peak Z", "open data", "preregistered", "statistical errors", 
                    "connected to theory", "predicted or exploratory", "participant sampling", "effect unexpected")
g.sur.rat <- ggplot(gather(imp.dat), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key) +
  theme_bw()


names(rank.dat) <- c("sample size", "participants excluded", "statistical power", "effect size", "cluster peak Z", "cluster extent", "cluster p-value", 
                     "main or interaction", "within or between design", "open data", "statistical errors", "strongly connected to theory", 
                     "predicted or exploratory", "participant sampling", "condition assignment", "replication result", "preregistered", "effect unexpected")
g.sur.rank <- ggplot(gather(rank.dat), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key) +
  theme_bw()


#### Supplementary material SM4 figures. ####

# WoS correlation matrix
cor.dat <- select(.data = data.bib, 
                  PY,
                  TC_2020, 
                  altmetric_score)

cor.dat$PY <- 2020-cor.dat$PY
cor.dat$TC_2020_by_year <- cor.dat$TC_2020/cor.dat$PY
cor.dat$altmetric_score_by_year <- cor.dat$altmetric_score/cor.dat$PY
cor.dat$tncs <- data.bib$tncs
names(cor.dat) <- c("age", "WoS", "Altmetric", "WoS by year", "Altmetrics by year", "CWTS normalized")
cor.mat.wos <- cor(cor.dat, use = "pairwise.complete.obs", method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Set color scheme for corrplot
# corrplot(corr = cor.mat.wos, 
#          method = "color", 
#          col = col(200), 
#          addCoef.col = "black", 
#          tl.col="black", 
#          tl.srt=45,  
#          diag = F)

# Crossref correlation matrix
cor.dat <- select(.data = data.bib, 
                  PY,
                  crossref_citations, 
                  altmetric_score)

cor.dat$PY <- 2020-cor.dat$PY
cor.dat$crossref_citations_by_year <- cor.dat$crossref_citations/cor.dat$PY
cor.dat$altmetric_score_by_year <- cor.dat$altmetric_score/cor.dat$PY
cor.dat$tncs <- data.bib$tncs
names(cor.dat) <- c("age", "Crossref", "Altmetric", "Crossref by year", "Altmetrics by year", "CWTS normalized")
cor.mat.crossref <- cor(cor.dat, use = "pairwise.complete.obs", method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Set color scheme for corrplot
# corrplot(corr = cor.mat.crossref, 
#          method = "color", 
#          col = col(200), 
#          addCoef.col = "black", 
#          tl.col="black", 
#          tl.srt=45,  
#          diag = F)

# Scopus correlation matrix
cor.dat <- select(.data = data.bib, 
                  PY,
                  scopus_citations, 
                  altmetric_score)

cor.dat$PY <- 2020-cor.dat$PY
cor.dat$scopus_citations_by_year <- cor.dat$scopus_citations/cor.dat$PY
cor.dat$altmetric_score_by_year <- cor.dat$altmetric_score/cor.dat$PY
cor.dat$tncs <- data.bib$tncs
names(cor.dat) <- c("age", "Scopus", "Altmetric", "Scopus by year", "Altmetrics by year", "CWTS normalized")
cor.mat.scopus <- cor(cor.dat, use = "pairwise.complete.obs", method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Set color scheme for corrplot
# corrplot(corr = cor.mat.scopus, 
#          method = "color", 
#          col = col(200), 
#          addCoef.col = "black", 
#          tl.col="black", 
#          tl.srt=45,  
#          diag = F)

# CWTS correlation matrix
cor.dat <- select(.data = data.bib, 
                  PY,
                  tcs, 
                  altmetric_score)

cor.dat$PY <- 2020-cor.dat$PY
cor.dat$tcs_by_year <- cor.dat$tcs/cor.dat$PY
cor.dat$altmetric_score_by_year <- cor.dat$altmetric_score/cor.dat$PY
cor.dat$tncs <- data.bib$tncs
names(cor.dat) <- c("age", "CWTS", "Altmetric", "CWTS by year", "Altmetrics by year", "CWTS normalized")
cor.mat.cwts <- cor(cor.dat, use = "pairwise.complete.obs", method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Set color scheme for corrplot
# corrplot(corr = cor.mat.cwts, 
#          method = "color", 
#          col = col(200), 
#          addCoef.col = "black", 
#          tl.col="black", 
#          tl.srt=45,  
#          diag = F)

# Scite correlation matrix
cor.dat <- select(.data = data.bib, 
                  PY,
                  scite_total_citation_count, 
                  altmetric_score)

cor.dat$PY <- 2020-cor.dat$PY
cor.dat$scite_by_year <- cor.dat$scite_total_citation_count/cor.dat$PY
cor.dat$altmetric_score_by_year <- cor.dat$altmetric_score/cor.dat$PY
cor.dat$tncs <- data.bib$tncs
names(cor.dat) <- c("age", "Scite", "Altmetric", "Scite by year", "Altmetrics by year", "CWTS normalized")
cor.mat.scite <- cor(cor.dat, use = "pairwise.complete.obs", method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Set color scheme for corrplot
# corrplot(corr = cor.mat.scite,
#          method = "color",
#          col = col(200),
#          addCoef.col = "black",
#          tl.col="black",
#          tl.srt=45,
#          diag = F)

