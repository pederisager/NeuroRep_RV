###############################
# Exploratory analysis script #
###############################


#### Load packages #### 

library(tidyverse)
library(psych)
library(gridExtra)
library(corrplot)



#### Load and clean data #### 

source("load_and_clean_data.R")



#### Analyses #### 

# Using the codebook 

## To check the description of a variable, use $ to find the variable in codebook.data.all. E.g.:

codebook$UT
codebook$coder
codebook$tcs

## To view the entire codebook: 

view(t(codebook))

## The codebook contains a description of all variables contained in data.all and data.bib. 



# Dataset summaries

## data.bib
 
pubyear.freq <- table(data.bib$PY)
barplot(pubyear.freq, xlab = "Publication year")  # Plot frequency of publication years

hist(data.bib$TC_2020, breaks = 50, xlab = "Citation count (WoS)", main = "")  # Plot citation count distribution

corrplot(cor(select(data.bib, 
           TC, 
           TC_2020, 
           crossref_citations, 
           scopus_citations, 
           tcs, 
           altmetric_score), 
    use = "complete.obs") ) # Print correlation matrix for various citation count sources - all are highly intercorrelated, except altmetric count, which is weakly correlated with all other sources (as is known from literature). 



## data.all


describe(select(data.all, 
                TC_2020,
                years.since.pub, 
                sample_size,
                RV))  # Descriptive statistics for RV and all variables used to calculate it

par(mfrow=c(2,2))

hist(data.all$sample_size, breaks = 300, xlab = "Sample size", main = "")  #Plot sample size (cut values over 200)
hist(data.all$TC_2020, breaks = 50, xlab = "Citation counts", main = "")  #Plot citation count
barplot(table(data.all$PY), xlab = "Publication year", ylab = "Frequency")  # Plot log sample size
hist(data.all$RV, breaks = 50, xlab = "Replicaton value", main = "")  #Plot replication value

par(mfrow=c(1,1))  # Reset par(mfrow)

### Test if sample size is uncorrelated with yearly citation count, as is predicted by the psychometric model in thesis chapter 2

cor.test(data.all$sample_size, data.all$TC_2020/data.all$years.since.pub, na.action = "na.rm")  # correlation test - likely incorrectly specified given the massive skew of both variables

par(mfrow=c(2,1))

plot(data.all$sample_size, data.all$TC_2020/data.all$years.since.pub, xlab="sample size", ylab="citation count per year")  # plot sample size by citation count, raw
plot(log(data.all$sample_size), log(data.all$TC_2020/data.all$years.since.pub), xlab="log sample size", ylab="log citation count per year")  # plot sample size by citation count, log

par(mfrow=c(1,1))  # Reset par(mfrow)

## data.irr

ICC(data.irr[, c("sample_size_orig", "sample_size_BA", "sample_size_PhD")])  # Intraclass correlation between the three sample size coders

corrplot(cor(select(.data = data.irr, 
           sample_size_orig, 
           sample_size_BA, 
           sample_size_PhD, 
           sample_size_final)))  # Correlation matrix between the three different coders and the final sample size after resolving disagreements








# Part 1 - describe the sample of candidates in terms of their bibliometrics 
#
# RQ: Is our sample representative of the population "fmri research in social psychology"?

## 1. Plot VOS viewer word cloud (done outside of R)

write.csv(x = data.bib, file = "../../../processed_data/wos+cwts_nonexcluded_records_data.csv", row.names = F)  # Save dataset to be used by VOSviewer

## 2. Describe number of CWTS clusters, distribution of cluster size, and most frequent keyterms used to describe clusters

nrow(unique(select(data.bib, cluster_id1, n_pubs)))  # Number of unique clusters/subfields contained in set

### Plot most frequent primary cluster labels

labels <- c(data.bib$label1, 
            data.bib$label2, 
            data.bib$label3, 
            data.bib$label4, 
            data.bib$label5)  # Combine all labels into one string variable

lab.freq <- as.data.frame(tail(sort(table(labels)), 50))  # count prevalence of each string and save 50 most prevalent

g.lab.freq <- ggplot(lab.freq, aes(x = labels, y = Freq)) +
  geom_col() +
  labs(title="Most frequent primary cluster labels",
       x = NULL,
       y = "Frequency") +
  coord_flip() +
  theme_bw()

### Plot distribution of cluster size

clu.siz <- unique(select(data.bib, cluster_id1, n_pubs))

g.clu.siz <- ggplot(clu.siz, aes(x = n_pubs)) +
  geom_histogram() +
  labs(title="Number of publications in clusters/subfields",
       x = "Cluster size",
       y = "Frequency") +
  theme_bw()


### Plot journals most frequently published in

jou.freq <- as.data.frame(tail(sort(table(data.bib$SO)), 20))
levels(jou.freq$Var1) <- sub("PROCEEDINGS OF THE NATIONAL ACADEMY OF SCIENCES OF THE UNITED STATES OF AMERICA", "PNAS", levels(jou.freq$Var1))


g.jou.freq <- ggplot(jou.freq, aes(x = Var1, y = Freq)) +
  geom_col() +
  labs(title="Journals most frequently published in",
       x = NULL,
       y = "Frequency") +
  coord_flip() +
  theme_bw()


### Plot WoS research fields most frequently tagged

field.freq <- as.data.frame(tail(sort(table(data.bib$WC)), 20))


g.field.freq <- ggplot(field.freq, aes(x = Var1, y = Freq)) +
  geom_col() +
  labs(title="WoS research fields most frequently tagged",
       x = NULL,
       y = "Frequency") +
  coord_flip() +
  theme_bw()


### Plot frequent cluster labels and cluster size distribution side by side
grid.arrange(g.lab.freq, g.clu.siz, ncol=2)



#### Analyses: Variation in RV across variation in input and calculation ####

# Separate out the top 100 RV studies in the dataset for separate analyses 

data.all.top100 <- data.all %>% arrange(., -RV, ) %>% head(100)



# Analyze "value" indicator distributions and their cross-correlations

## data.bib

### Distributions

ggplot(data = data.bib) +
  geom_density(aes(x = TC_2020), col = "black") +
  geom_density(aes(x = crossref_citations), col = "red") + 
  geom_density(aes(x = scopus_citations), col = "blue") +
  geom_density(aes(x = tcs), col = "green") +
  geom_density(aes(x = TC), col = "purple") +
  theme_bw()

ggplot(data = data.bib, aes(x = tncs)) +
  geom_density() 

ggplot(data = data.bib, aes(x = altmetric_score)) +
  geom_density() 

### Correlation matrix

cor(select(.data = data.bib, 
           TC_2020, 
           TC, 
           crossref_citations, 
           scopus_citations,
           tcs,
           tncs,
           altmetric_score), use = "pairwise.complete.obs", method = "spearman")

## data.all

### Distributions

g.raw <- ggplot(data = data.all) +
  geom_density(aes(x = TC_2020), col = "black") +
  geom_density(aes(x = crossref_citations), col = "red") + 
  geom_density(aes(x = scopus_citations), col = "blue") +
  geom_density(aes(x = tcs), col = "green") +
#  geom_density(aes(x = TC), col = "purple") +
  theme_bw() +
  labs(title="A")

g.tncs <- ggplot(data = data.all, aes(x = tncs)) +
  geom_density() +
  theme_bw()+
  labs(title="B")

g.alt <- ggplot(data = data.all, aes(x = altmetric_score)) +
  geom_density() +
  theme_bw()+
  labs(title="C")

grid.arrange(g.raw, g.tncs, g.alt)

### Correlation matrix

cor.dat <- select(.data = data.all, 
       TC_2020, 
       # TC, 
       # crossref_citations, 
       # scopus_citations,
       # tcs,
       # tncs,
       # altmetric_score, 
       sample_size, 
       years.since.pub)

cor.dat$TC_2020_by_year <- cor.dat$TC_2020/cor.dat$years.since.pub

cor.mat <- cor(cor.dat, use = "pairwise.complete.obs", method = "spearman")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Set color scheme for corrplot

corrplot(corr = cor.mat, 
         method = "color", 
         col = col(200), 
         addCoef.col = "black", 
         tl.col="black", 
         tl.srt=45, 
         order="alphabet", 
         diag = F)




## Top 100 data

cor.dat <- select(.data = data.all.top100, 
                  TC_2020, 
                  TC, 
                  crossref_citations, 
                  scopus_citations,
                  tcs,
                  tncs,
                  altmetric_score, 
                  sample_size, 
                  years.since.pub)

cor.dat$TC_2020_by_year <- cor.dat$TC_2020/cor.dat$years.since.pub

cor.mat <- cor(cor.dat, use = "pairwise.complete.obs", method = "spearman")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Set color scheme for corrplot

corrplot(corr = cor.mat, 
         method = "color", 
         col = col(200), 
         addCoef.col = "black", 
         tl.col="black", 
         tl.srt=45, 
         order="alphabet", 
         diag = F)




# Analyze sample size distribution and inter-rater reliability

## All data

### Distribution

describe(data.all$sample_size)

ggplot(data = data.all, aes(x = sample_size)) + 
  geom_density() +
  theme_bw()

### Inter-rater reliability

#### Intra-class correlation coefficent
ICC(select(data.irr, sample_size_orig, sample_size_BA, sample_size_PhD))

#### Correlation matrix
irr.mat <- cor(select(data.irr, sample_size_orig, sample_size_BA, sample_size_PhD, sample_size_final))

corrplot(corr = irr.mat, 
         method = "color", 
         col = col(200), 
         addCoef.col = "black", 
         tl.col="black", 
         tl.srt=45, 
         order="hclust", 
         diag = F)

#### Coder-by-coder scatter plots
p.orig_BA <- ggplot(data = data.irr, aes(x = sample_size_orig, y = sample_size_BA)) +
  geom_point() +
  theme_bw()

p.orig_PhD <- ggplot(data = data.irr, aes(x = sample_size_orig, y = sample_size_PhD)) +
  geom_point() +
  theme_bw()

p.BA_PhD <- ggplot(data = data.irr, aes(x = sample_size_BA, y = sample_size_PhD)) +
  geom_point() +
  theme_bw()

grid.arrange(p.orig_BA, p.orig_PhD, p.BA_PhD)

## Top 100 data



# Publication year, distribution and analyses

## All data

### Distribution of publication year/age

plot(TC_2020~PY, data = data.all)
abline(lm(TC_2020 ~ PY, data = data.all), col = "blue")

plot(log(sample_size)~PY, data = data.all)
abline(lm(log(sample_size) ~ PY, data = data.all), col = "blue")

plot(log(TC_2020)~log(sample_size), data = data.all)


### Correlation between age and citation count

plot(TC_2020^(1/3) ~ PY, data = data.all)
abline(lm(TC_2020^(1/3) ~ PY, data = data.all), col = "blue")

### Correlation between age and (citation count) / age

plot(TC_2020^(1/3) ~ PY, data = data.all)
abline(lm(TC_2020^(1/3) ~ PY, data = data.all), col = "blue")

### Correlation between age and sample size

plot(sample_size^(1/3) ~ PY, data = data.all)
abline(lm(sample_size^(1/3) ~ PY, data = data.all), col = "blue")

### Correlation between citation count and sample size, controlling for age

cor.test(data.all$TC_2020, data.all$sample_size, method = "spearman")
ppcor::pcor.test(data.all$TC_2020, data.all$sample_size, data.all$years.since.pub, method = "spearman")

x <- data.all
x$TC2020_plus1 <- x$TC_2020+1 

model.1 <- glm(formula = TC2020_plus1 ~ sample_size, 
               data = x, 
               family = Gamma(link=log))

coefficients(model.1)

summary(model.1)


model.2 <- glm(formula = TC2020_plus1 ~ sample_size + years.since.pub, 
              data = x, 
              family = Gamma(link=log))

coefficients(model.2)

summary(model.2)

reghelper::beta(model.1)
reghelper::beta(model.2)






# Analyze RV distribution and cross correlation

## data.all

RV.dat <- select(data.all, TC_2020, TC, crossref_citations, scopus_citations, tcs, tncs, altmetric_score)
RV.dat$TC_2020_plus_altmetric <- RV.dat$TC_2020 + RV.dat$altmetric_score

RV.dat <- (RV.dat/data.all$years.since.pub+1) * (1/data.all$sample_size)  # Calculate RV for all value indicators 

RV.mat <- cor(RV.dat, use = "pairwise.complete.obs", method = "spearman")

corrplot(corr = RV.mat, 
         method = "color", 
         col = col(200), 
         addCoef.col = "black", 
         tl.col="black", 
         tl.srt=45, 
         order="hclust", 
         diag = F)


## data.all.top100

RV.dat <- select(data.all.top100, TC_2020, TC, crossref_citations, scopus_citations, tcs, tncs, altmetric_score)
RV.dat$TC_2020_plus_altmetric <- RV.dat$TC_2020 + RV.dat$altmetric_score

RV.dat <- (RV.dat/data.all$years.since.pub+1) * (1/data.all$sample_size)  # Calculate RV for all value indicators 

RV.mat <- cor(RV.dat, use = "pairwise.complete.obs", method = "spearman")

corrplot(corr = RV.mat, 
         method = "color", 
         col = col(200), 
         addCoef.col = "black", 
         tl.col="black", 
         tl.srt=45, 
         order="hclust", 
         diag = F)






