# Load packages
library(tidyverse)
library(janitor)
library(rstan)
library(bayesrules)
library(bayesplot)

# Defining a discrete grids of pi values
grid_data = data.frame(pi_grid = seq(0, 1, length=6))

# Evaluating prior and likelihood at each pi
grid_data = grid_data %>% mutate(prior = dbeta(pi_grid, 2, 2),
         likelihood = dbinom(9, 10, pi_grid))

grid_data

# Approximating the posterior
grid_data = grid_data %>% 
  mutate(unnormalized = likelihood*prior,
posterior = unnormalized/sum(unnormalized))

# Assessing that normalized posterior sums up to 1
grid_data %>% summarize(sum(unnormalized), sum(posterior))

# Examining the grid approximated posterior
round(grid_data, 2)

# Plotting the grid approximated posterior
ggplot(grid_data, aes(x = pi_grid, y = posterior)) + geom_point() + geom_segment(aes(x = pi_grid, xend = pi_grid, y = 0, yend = posterior))

# Sampling from the approximated posterior
set.seed(84735)
post_sample = sample_n(grid_data, size = 10000, weight = posterior, replace=TRUE)

post_sample %>% 
  tabyl(pi_grid) %>% 
  adorn_totals("row")

# Histogram of the grid simulation with posterior pdf
ggplot(post_sample, aes(x=pi_grid)) + geom_histogram(aes(y=..density..), color ="white") + stat_function(fun=dbeta, args=list(11, 3)) + lims(x = c(0, 1))

##########################################################################
# REFINING THE GRID ######################################################
##########################################################################

# Defining a discrete grids of pi values
grid_data = data.frame(pi_grid = seq(0, 1, length=101))

# Evaluating prior and likelihood at each pi
grid_data = grid_data %>% mutate(prior = dbeta(pi_grid, 2, 2),
                                 likelihood = dbinom(9, 10, pi_grid))

grid_data

# Approximating the posterior
grid_data = grid_data %>% 
  mutate(unnormalized = likelihood*prior,
         posterior = unnormalized/sum(unnormalized))

# Assessing that normalized posterior sums up to 1
grid_data %>% summarize(sum(unnormalized), sum(posterior))

# Examining the grid approximated posterior
round(grid_data, 2)

# Plotting the grid approximated posterior
ggplot(grid_data, aes(x = pi_grid, y = posterior)) + geom_point() + geom_segment(aes(x = pi_grid, xend = pi_grid, y = 0, yend = posterior))

# Sampling from the approximated posterior
set.seed(84735)
post_sample = sample_n(grid_data, size = 10000, weight = posterior, replace=TRUE)

post_sample %>% 
  tabyl(pi_grid) %>% 
  adorn_totals("row")

# Histogram of the grid simulation with posterior pdf
ggplot(post_sample, aes(x=pi_grid)) + geom_histogram(aes(y=..density..), color ="white") + stat_function(fun=dbeta, args=list(11, 3)) + lims(x = c(0, 1))



