# 0 - Set-up

# Exercise: Survey results using learnR modules
# Discussion: Review of key terms from pre-work exercises
# Exercise: Fundamentals of random sampling and descriptive statistics
# Q&A
##################################################################################################################
# This script resets and then populates the environment with some objects to work on
# Clear the workspace
rm(list = ls())

# Load packages
library(tidyverse)

##################################################################################################################
# For the one-sample t-test:
martian <- data.frame(height = c(192.5, 212.2, 202.9, 190.3, 198.0, 202.9, 200.1, 199.6, 199.7, 201.8))

# For the two-sample t-test:
sleep_long <- sleep
levels(sleep_long$group) <- c("Group_1", "Group_2")

sleep_long %>% 
  spread(group, extra) -> sleep_wide

##################################################################################################################
# Exercises

# Create a large (10^6 is good) population 
popln <- rnorm(10^6, 6, 10)

# Calculate the mean of the 
mean(popln)

# Choose x samples of size n
x <- 20 # Number of samples
n <- 3 # observations in each sample

data.frame(xx = replicate(x, mean(sample(xx, n)))) %>% 
  ggplot(aes(xx)) +
  geom_histogram()

