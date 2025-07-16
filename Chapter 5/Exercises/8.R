# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)

# Plotting likelihood
plot_normal_likelihood(c(-4.3, 0.7, -19.4), sigma = 10)

plot_normal_likelihood(c(-12, 1.2, -4.5, 0.6), sigma = 6)

plot_normal_likelihood(c(12.4, 6.1), sigma = 5)
