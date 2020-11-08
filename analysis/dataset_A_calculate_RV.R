
# Calculate "citations / n" replication value.

## Define RV function
rv <- function(c, y, n) {
  (c/y)*(1/n)
}

## Load data
data <- readRDS("dataset_A_data.rds")  # load dataset
this.year <- 2020  # determine current year

## Calculate RV
c <- as.numeric(data$TC)  # citation count of paper
y <- this.year-data$PY  # years since paper was published
n <- data$sample_size  # sample size of study

data$RV <- rv(c, y, n)

## Save output
write.csv(data, "test.csv")
