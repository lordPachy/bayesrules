# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)

# Plotting
plot_gamma(0.5, 1)
plot_gamma_poisson(0.5, 1, sum(c(3, 2, 5, 1, 2)), 20)
summarize_gamma_poisson(0.5, 1, sum(c(3, 2, 5, 1, 2)), 20)
plot_poisson_likelihood(c(3, 2, 5, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), lambda_upper_bound = 2.5)
