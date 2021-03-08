

# Load data
abstracts <- read.csv(file = "../processed_data/dataset_C_pilot_data.csv")  # We can replace this file path with any file path that contains our stimulus data

# Make all possible pairs of indeces from top, middle and bottom RV study groups

l <- list(1:30, 31:60, 61:90)  # This corresponds to the row indeces for studies in the "abstracts" dataset. Row 1:30 are the highest RVs. Row 31:60 are the middle RVs. Row 61:90 are the bottom RVs.
top.middle <- expand.grid(l[c(1, 2)])  # Make all possible pair combos of the top and middle stimuli
top.bottom <- expand.grid(l[c(1, 3)])  # Make all possible pair combos of the top and bottom stimuli
middle.bottom <- expand.grid(l[c(2, 3)])  # Make all possible pair combos of the middle and bottom stimuli

# Draw 3 pairs at random from each group comparison, and a final 10th abstract from a random one out of the group comparison types

n <- sample(c(3,3,4), 3)  # randomize which group comparison type will sample 4 participants

abstract_pairs <- rbind(top.middle[sample(nrow(top.middle),       n[1], replace = F),],  # sample n[1] from top-middle pairs
                        top.bottom[sample(nrow(top.bottom),       n[2], replace = F),],  # sample n[2] from top-bottom pairs
                        middle.bottom[sample(nrow(middle.bottom), n[3], replace = F),]  # sample n[4] from middle-bottom pairs
                        )

# Replace the index numbers with the actual abstracts in "abstracts" that those indeces refer to.

abstract_pairs$abstract1 <-  abstracts$AB[abstract_pairs[,1]]
abstract_pairs$abstract2 <-  abstracts$AB[abstract_pairs[,2]]


# Presenting an abstract pair, randomly varying which side of the screen the pair is presented on, can be done with the following code:

column <- sample(3:4, 2)  # Randomlize whether abstract1 or abstract2 will be presented first.
trial <- 1  # Determines which row in "abstract_pairs" to select as the pair to display.

## abstract 1
abstract_pairs[trial,column[1]]
## abstract 2
abstract_pairs[trial,column[2]]

