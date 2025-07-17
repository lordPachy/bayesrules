# Load packages
library(tidyverse)
library(janitor)
library(rstan)
library(bayesrules)
library(bayesplot)

# Understanding grid limits
plot_normal(10, 1.2)

# Step 1: Define a grid of 11 mean values
grid_data = data.frame(
  mean_grid = seq(from = 5, to = 15, length = 11))

grid_data

# Step 2: Evaluate the prior & likelihood at each lambda
grid_data = grid_data %>% 
  mutate(prior = dnorm(mean_grid, mean = 10, sd = 1.2), likelihood = dnorm(mean_grid, 7.1, 1.3) * dnorm(mean_grid, 8.9, 1.3) * dnorm(mean_grid, 8.4, 1.3) * dnorm(mean_grid, 8.6, 1.3))

grid_data

# Step 3: Approximate the posterior
grid_data <- grid_data %>% 
  mutate(unnormalized = prior*likelihood,
         posterior = unnormalized/sum(unnormalized))

# Set the seed
set.seed(84735)

# Step 4: sample from the discretized posterior
post_sample <- sample_n(grid_data, size = 10000, 
                        weight = posterior, replace = TRUE)

# Plotting

# Histogram of the grid simulation with posterior pdf
ggplot(post_sample, aes(x=mean_grid)) + geom_histogram(aes(y=..density..), color ="white")+ lims(x = c(5, 15)) + stat_function(fun = dnorm, args = list(10, 1.3))

# Step 1: Define a grid of 11 mean values
grid_data = data.frame(
  mean_grid = seq(from = 5, to = 15, length = 201))

grid_data

# Step 2: Evaluate the prior & likelihood at each lambda
grid_data = grid_data %>% 
  mutate(prior = dnorm(mean_grid, mean = 10, sd = 1.2), likelihood = dnorm(mean_grid, 7.1, 1.3) * dnorm(mean_grid, 8.9, 1.3) * dnorm(mean_grid, 8.4, 1.3) * dnorm(mean_grid, 8.6, 1.3))

grid_data

# Step 3: Approximate the posterior
grid_data <- grid_data %>% 
  mutate(unnormalized = prior*likelihood,
         posterior = unnormalized/sum(unnormalized))

# Set the seed
set.seed(84735)

# Step 4: sample from the discretized posterior
post_sample <- sample_n(grid_data, size = 10000, 
                        weight = posterior, replace = TRUE)

# Plotting

# Histogram of the grid simulation with posterior pdf
ggplot(post_sample, aes(x=mean_grid)) + geom_histogram(aes(y=..density..), color ="white")+ lims(x = c(5, 15)) + stat_function(fun = dnorm, args = list(10, 1.3))

