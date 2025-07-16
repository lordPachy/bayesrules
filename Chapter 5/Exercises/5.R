# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)

# Plotting prior
plot_gamma(50, 10)
pgamma(q = 10, shape = 50, rate = 10, lower.tail = FALSE)

# Plotting likelihood and posterior
plot_poisson_likelihood(c(7, 3, 8, 9, 10, 12), lambda_upper_bound = 20)

plot_gamma_poisson(50, 10, sum(c(7, 3, 8, 9, 10, 12)), n = 6)
