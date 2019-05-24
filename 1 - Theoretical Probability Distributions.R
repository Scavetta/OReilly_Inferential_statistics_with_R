# 1 - Theoretical Probability Distributions

# Presentation: Binomial and Normal distributions
# Exercise: Exploring distributions and related functions
# Presentation: Z-scores as signal:noise ratio
# Exercise: Calculating z-scores
# Exercise Using Q-Q plots to explore distributions
# Q&A
##################################################################################################################

# Load packages
library(tidyverse)

##################################################################################################################
# Exploring distributions

# To access distributuions in R, use the prefix d, p, q or r, followed by the distributuion name:

# e.g. for Normal
# dnorm(x, mean = 0, sd = 1)
# pnorm(q, mean = 0, sd = 1)
# qnorm(p, mean = 0, sd = 1)
# rnorm(n, mean = 0, sd = 1)

# e.g. for t, we'll see it later on:
# dt(x, df)
# pt(q, df)
# qt(p, df)
# rt(n, df)

# dnorm gives the density: i.e. position on the y at any given x
xx_d <- dnorm(seq(-4,4, length.out = 1000))
# plot(xx_d, type = "l")

# Alternatively:
ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, geom = "line")

# pnorm gives the distribution function: i.e. area from -Inf until this value
pnorm(1.96) # 0.975

# qnorm gives the quantile function: i.e. position on the x that contains this area under the curve
qnorm(0.975) # 1.96

# rnorm generates n random values accoding to the distribution
rnorm(10)

##################################################################################################################
# Calculating z-scores



##################################################################################################################
# Using Q-Q plots to explore distributions

n_dist <- 100

set.seed(639)
data.frame(norm = rnorm(n_dist),
           t = rt(n_dist, 1),
           exp = exp(seq(0,3,length.out = n_dist))-10) %>% 
  gather(distribution, value) -> dists

# Raw distributions:
dists %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_grid(distribution ~ ., scales = "free_y") +
  labs(title = "Raw values")

# Q-Q plots:
dists %>% 
  ggplot(aes(sample = value)) +
  stat_qq() + 
  stat_qq_line() +
  facet_grid(distribution ~ ., scales = "free_y") +
  labs(title = "Q-Q plots")
