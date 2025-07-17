# Load packages
library(tidyverse)
library(janitor)
library(rstan)
library(bayesrules)
library(bayesplot)

# Understanding grid limits
plot_gamma_poisson(3, 1, 10, 2, FALSE)

# Step 1: Define a grid of 501 lambda values
grid_data   <- data.frame(
  lambda_grid = seq(from = 0, to = 15, length = 501))

# Step 2: Evaluate the prior & likelihood at each lambda
grid_data <- grid_data %>% 
  mutate(prior = dgamma(lambda_grid, 3, 1),
      likelihood = dpois(2, lambda_grid) * dpois(8, lambda_grid))

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
ggplot(post_sample, aes(x=lambda_grid)) + geom_histogram(aes(y=..density..), color ="white") + stat_function(fun=dgamma, args=list(13, 3)) + lims(x = c(0, 15))
