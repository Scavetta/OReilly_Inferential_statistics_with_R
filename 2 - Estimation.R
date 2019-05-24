# 2 - Estimation

# Presentation: From the Normal distribution to the Central Limit Theorem (CLT)
# Exercise: Simulating the CLT
# Presentation: From the CLT to confidence intervals
# Exercise: Calculating confidence intervals
# Q&A
##################################################################################################################

# Load packages
library(tidyverse)

##################################################################################################################
# Simulating the CLT

# Make a large population
set.seed(136)
population <- data.frame(parent = rnorm(10^4, 25, 6))

mean(population$parent)        
sd(population$parent)        

# plot it:
ggplot(population, aes(parent)) +
  geom_histogram() +
  ggtitle("Parental population")

# Draw x number of samples, each with a n observations

x <- 150
n <- c(3, 10, 30, 100, 300)

n %>% 
  map(~ replicate(x, mean(sample(population$parent, .)), simplify = T)) %>% 
  as.data.frame() %>% 
  gather(key, value) %>% 
  mutate(key = rep(n, each = x)) -> samples

ggplot(samples, aes(value)) +
  geom_histogram() +
  facet_grid(key ~ .) +
  ggtitle("Distributiuon of sample means")

##################################################################################################################
# Calculate the SEM for our sample of martian heights




##################################################################################################################
# Calculating confidence intervals

set.seed(136)
n <- c(3, 10, 30, 100, 300)

n %>% 
  map(~ sample(population$parent, .)) %>% 
  unlist() %>% 
  data.frame(value = .,
             sample_size = unlist(map2(.x = n, .y = n, ~ rep(.x, each = .y)))) -> mySamples

mySamples %>% 
  group_by(sample_size) %>% 
  summarise(avg = ...,
            stdev = ...,
            SEM = ...,
            error = 1.96, # from the Normal distribution for now
            lower = ...,
            upper = ...) -> mySummary

##################################################################################################################
# Calculate the 95% CI for our sample of martian heights




