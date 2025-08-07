# Load packages
library(bayesrules)
library(tidyverse)
library(rstan)
library(bayesplot)
library(broom.mixed)
library(janitor)

# Load data
data("moma_sample")

# Checking which artists are Gen X or younger
moma_sample %>% group_by(genx) %>% tally()
plot_beta_binomial(4, 6, 14, 86+14)
summarize_beta_binomial(4, 6, 14, 86+14)

# The 5% quantiles can be shown:
# "There is a 95% that between 10% and 24% of 
# the artists are Gen X or younger"
qbeta(c(0.025, 0.975), 18, 92)

# One-sided hypothesis testing:
# What is the probability that the proportion is
# below 20%?
post_prob = pbeta(0.2, 18, 92)
post_prob

# Posterior odds are calculated as such
# (for 1-sided tests)
post_odds = post_prob / (1 - post_prob)
post_odds

# Similarly, prior odds
prior_prob = pbeta(0.2, 4, 6)
prior_odds = prior_prob / (1 - prior_prob)

# Bayes Factor measures the ratio between
# posterior and prior odds
BF = post_odds / prior_odds
BF
