#### Notes ####

# Years with 0 citations not counted. Need
# article pub year to know how many years
# prior to first cited year to count as 0.

# Need to fill in 0 for each article for
# each year with zero citations. 

# Decide how to deal with citations older
# than from 2009 (so older than the pub 
# date of the cited paper). 

#### Install packages ####

library(tidyverse)



#### Load data ####

# Load data 
data.scite <- read.csv("../raw_data/scite_citations_by_year_for_all_records_downloaded_2021-08-24.csv")  # load citations per year from scite
data.wos <- readRDS("../processed_data/wos+cwts_all_records_data.Rds")  # load wos data with publication years



# Wrangle data

## Get publication year of dois in data.scite
data.scite <- left_join(data.scite, data.wos[, c("DI", "PY")], by = c("target_doi" = "DI"))  # populate data.scite with publication year data
data.scite <- unique(data.scite)
data.scite$PY <- as.integer(data.scite$PY)

## Remove citation years older than the publication year of the doi or more recent than 2020 - the latest year for which data are available for the whole year
data.scite <- data.scite %>% filter(year>=PY, year<=2020)

## Fill in missing 0 years in data
for (i in 1:nrow(data.scite)) {  # for all rows in data
  
  if (i==1 || isFALSE(data.scite$target_doi[i] == data.scite$target_doi[i-1])) {  # if first recorded year for this doi (doi new since last row)
    years_since_pub <- data.scite$year[i] - data.scite$PY[i]  # check how many years passed until first citation
    if (years_since_pub > 0) {  # if at least one year passed until first citation
      rows_to_fill <- (nrow(data.scite)+1):(nrow(data.scite)+years_since_pub)  # add rows at end of data for missing 0-citation years
      data.scite[rows_to_fill, "target_doi"] <- data.scite$target_doi[i]
      data.scite[rows_to_fill, "year"] <- data.scite$PY[i]:(data.scite$PY[i]+years_since_pub-1)
      data.scite[rows_to_fill, "citation_count"] <- 0
      data.scite[rows_to_fill, "PY"] <- data.scite$PY[i]
    }
  } 
  
  if (isFALSE(data.scite$target_doi[i] == data.scite$target_doi[i+1])) {  # if last recorded year for this doi (new doi in next row)
    years_to_2020 <- 2020 - data.scite$year[i]  # check if any years missing until 2021 (last year recorded by scite)
    if (years_to_2020 > 0) {  # if any years missing until 2021
      rows_to_fill <- (nrow(data.scite)+1):(nrow(data.scite)+years_to_2020)  # add rows at end of data for missing 0-citation years
      data.scite[rows_to_fill, "target_doi"] <- data.scite$target_doi[i]
      data.scite[rows_to_fill, "year"] <- (2020-years_to_2020+1):2020
      data.scite[rows_to_fill, "citation_count"] <- 0
      data.scite[rows_to_fill, "PY"] <- data.scite$PY[i]
    }
  }
  
  years_to_next <- data.scite$year[i+1] - data.scite$year[i]  # check how many years to next citation
  if (years_to_next > 1 && isTRUE(data.scite$target_doi[i] == data.scite$target_doi[i+1])) {  # if more than one year to next, and still the same doi
    rows_to_fill <- (nrow(data.scite)+1):(nrow(data.scite)+years_to_next-1)  # add rows at end of data for missing 0-citation years
    data.scite[rows_to_fill, "target_doi"] <- data.scite$target_doi[i]
    data.scite[rows_to_fill, "year"] <- (data.scite$year[i]+1):(data.scite$year[i]+years_to_next-1)
    data.scite[rows_to_fill, "citation_count"] <- 0
    data.scite[rows_to_fill, "PY"] <- data.scite$PY[i]
  }
  
}

## Sort data after filling in missing zeros
data.scite <- data.scite %>% arrange(target_doi, year)

# Create variables
## Number of years from paper was published to current citation year
data.scite <- data.scite %>% mutate(years_since_pub = year - PY)
## Change in citation count since last year
data.scite$cit_change <-  
  data.scite$citation_count - 
  c(0, data.scite$citation_count[-nrow(data.scite)])
data.scite$cit_change[data.scite$years_since_pub==0] <- NA
## Percentage change in citation count since last year
data.scite$cit_change_prop <-  
  (data.scite$citation_count - 
  c(0, data.scite$citation_count[-nrow(data.scite)])) / 
  c(0, data.scite$citation_count[-nrow(data.scite)])
data.scite$cit_change_prop[data.scite$years_since_pub==0] <- NA

#### Plot data ####

# Citation trajectory for each article since publication
data.scite %>% 
  mutate(years_since_pub = year - PY) %>%
  ggplot(aes(x = years_since_pub, y = citation_count)) + 
  geom_line(aes(group=target_doi)) +
  scale_x_continuous(breaks = seq(0, 11, by = 1))

# Citation trajectory, excluding extreme cases
data.scite %>% 
  mutate(years_since_pub = year - PY) %>%
  group_by(target_doi) %>% 
  filter(!any(citation_count > 50)) %>%
  ggplot(aes(x = years_since_pub, y = citation_count)) + 
  geom_line(aes(group=target_doi))+
  scale_x_continuous(breaks = seq(0, 11, by = 1))

# Ceneral citation trajectory trend
data.scite %>% 
  mutate(years_since_pub = year - PY) %>%
  ggplot(aes(x = years_since_pub, y = citation_count, 10)) + 
  geom_smooth(level = 0.99) + 
  #geom_boxplot(aes(group=years_since_pub)) +
  scale_x_continuous(breaks = seq(0, 11, by = 1))




#### Analyze data #### 

# See if c/(y+1) up to 2019 is a good predictor of citations in year 2020
data <- data.scite %>%  # compute c/(y+1) up to 2019
  filter(year<=2019) %>%  
  group_by(target_doi) %>% 
  do(CoverYplus1 = sum(.$citation_count)/max(.$years_since_pub+1)) %>% 
  mutate(CoverYplus1 = as.numeric(CoverYplus1))

data$citations2020 <- data.scite %>%  # extract 2020 citation count
  filter(year==2020) %>% 
  .$citation_count

data <- unique(left_join(data, as_tibble(data.scite[, c(1,4)]), by = "target_doi"))  # add publication year to summary data
  
data %>%  
  ggplot(aes(x = CoverYplus1, y = citations2020)) +
  geom_point(aes(col=PY)) + 
  geom_smooth(method = "lm", col="black")

cor(data$CoverYplus1, data$citations2020, method = ("spearman"))
