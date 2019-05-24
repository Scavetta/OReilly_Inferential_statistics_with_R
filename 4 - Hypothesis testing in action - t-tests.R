# 4 - Hypothesis testing in action: t-tests

# Presentation: Putting it all together: one-sample, two-sample and paired t-tests
# Exercise: Calculating t-tests in R
# Discussion: Understanding how to interpret results
# Q & A
##################################################################################################################

# load data set for tests:
source("0 - Set-up.R")


##################################################################################################################
# Calculating t-tests in R: One-sample

n <- 20 # Sample size
xbarObs <- 2 # Observed mean
s <- 2 # Observed Standard deviation

# Generate data
dataOneSam <- data.frame(value = rnorm(n, xbarObs, s)) 

# hypothesised mean
xbarHypo <- 0

# Two-sided
t.test(dataOneSam$value, mu = xbarHypo)

# One-sided, Alternative hypothesis: observed is greater than null hypothesis  
# Does the evidence support the null hypothesis
xbarObs < xbarHypo 
t.test(dataOneSam$value, mu = xbarHypo, alternative = "greater")

# One-sided, Alternative hypothesis: observed is less than null hypothesis  
# Does the evidence support the null hypothesis
xbarObs > xbarHypo 
t.test(dataOneSam$value, mu = xbarHypo, alternative = "less")

##################################################################################################################
# Calculating t-tests in R: Two-sample, Welch's (not assuming equal variance)

# Sample 1
n_1 <- 20 # Sample size
xbarObs_1 <- 2 # Observed mean
s_1 <- 2 # Observed Standard deviation

# Sample 2
n_2 <- 20 # Sample size
xbarObs_2 <- 6 # Observed mean
s_2 <- 2 # Observed Standard deviation

# Generate data
set.seed(136)
dataTwoSam_long <- data.frame(key = c(rep("Group 1", n_1), rep("Group 2", n_2)),
                              value = c(rnorm(n_1, xbarObs_1, s_1),rnorm(n_2, xbarObs_2, s_2))) 

set.seed(136)
dataTwoSam_wide <- data.frame(Group_1 = rnorm(n_1, xbarObs_1, s_1),
                              Group_2 = rnorm(n_2, xbarObs_2, s_2))


# Plot it:
ggplot(dataTwoSam_long, aes(key, value)) +
  geom_point(position = position_jitter(0.2, seed = 631)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), col = "red") +
  labs(title = "Two sample t-tests", x = "", y = "Individual values with mean and standard deviation")

# Two-sided
# For wide format, specify x & y
t.test(dataTwoSam_wide$Group_1, dataTwoSam_wide$Group_2)

# For long format, use formula notation:
t.test(value ~ key, data = dataTwoSam_long)

# Two-sided, Welch's, Alternative hypothesis: mean of group 2 is greater than mean of group 1 
# Does the evidence support the null hypothesis
xbarObs_1 < xbarObs_2
t.test(dataTwoSam_wide$Group_1, dataTwoSam_wide$Group_2, alternative = "greater")
t.test(value ~ key, data = dataTwoSam_long, alternative = "greater")

# Two-sided, Welch's, Alternative hypothesis: mean of group 2 is less than mean of group 1 
# Does the evidence support the null hypothesis
xbarObs_1 > xbarObs_2
t.test(dataTwoSam_wide$Group_1, dataTwoSam_wide$Group_2, alternative = "less")
t.test(value ~ key, data = dataTwoSam_long, alternative = "less")

##################################################################################################################
# Calculating t-tests in R: Two-sample (assuming equal variance)
# use groups defined above:

# Two-sided
# For wide format, specify x & y
t.test(dataTwoSam_wide$Group_1, dataTwoSam_wide$Group_2, var.equal = TRUE)

# For long format, use formula notation:
t.test(value ~ key, data = dataTwoSam_long, var.equal = TRUE)

# Two-sided, Alternative hypothesis: mean of group 2 is greater than mean of group 1 
# Does the evidence support the null hypothesis
xbarObs_1 < xbarObs_2
t.test(dataTwoSam_wide$Group_1, dataTwoSam_wide$Group_2, alternative = "greater", var.equal = TRUE)
t.test(value ~ key, data = dataTwoSam_long, alternative = "greater", var.equal = TRUE)

# Two-sided, Alternative hypothesis: mean of group 2 is less than mean of group 1 
# Does the evidence support the null hypothesis
xbarObs_1 > xbarObs_2
t.test(dataTwoSam_wide$Group_1, dataTwoSam_wide$Group_2, alternative = "less", var.equal = TRUE)
t.test(value ~ key, data = dataTwoSam_long, alternative = "less", var.equal = TRUE)

##################################################################################################################
# Calculating t-tests in R: Two Sample, paired

n <- 20 # Sample size for each group

# Sample 1
xbarObs_1 <- 2 # Observed mean
s_1 <- 2 # Observed Standard deviation

# Sample 2
xbarObs_2 <- 6 # Observed mean
s_2 <- 2 # Observed Standard deviation

# Generate data
set.seed(136)
dataTwoSam_long_paired <- data.frame(key = c(rep("Group 1", n), rep("Group 2", n)),
                                     value = c(rnorm(n, xbarObs_1, s_1),rnorm(n, xbarObs_2, s_2)),
                                     ID = rep(seq_len(n), 2)) 

set.seed(136)
dataTwoSam_wide_paired <- data.frame(Group_1 = rnorm(n, xbarObs_1, s_1),
                                     Group_2 = rnorm(n, xbarObs_2, s_2))


# Plot it:
posn.d <- position_dodge(0.1)
ggplot(dataTwoSam_long_paired, aes(key, value, group = ID)) +
  geom_point(position = posn.d, alpha = 0.5) +
  geom_line(position = posn.d, alpha = 0.5) +
  labs(title = "Two sample t-tests, paired data", x = "", y = "Individual values")

# For wide format, specify x & y
t.test(dataTwoSam_wide$Group_1, dataTwoSam_wide$Group_2, paired = TRUE)

# For long format, use formula notation:
t.test(value ~ key, data = dataTwoSam_long, paired = TRUE)

# Which is exactly the same thing as calculating the differences and then performing a one-sample t-test
# Where the hypothesised mean is 0
t.test(dataTwoSam_wide$Group_1 - dataTwoSam_wide$Group_2)