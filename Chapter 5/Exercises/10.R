# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)

# Prior
plot_normal(7.2, 2.6)
pnorm(7.6, 7.2, 2.6, lower.tail=FALSE)
pnorm(0, 7.2, 2.6, lower.tail=TRUE)
pnorm(8, 7.2, 2.6, lower.tail=FALSE)

# Likelihood
plot_normal_likelihood(c(-0.7, 1.2, 4.5, -4), 2)

# Posterior
plot_normal_normal(7.2, 2.6, 2, mean(c(-0.7, 1.2, 4.5, -4)), length(c(-0.7, 1.2, 4.5, -4)))

summarize_normal_normal(7.2, 2.6, 2, mean(c(-0.7, 1.2, 4.5, -4)), length(c(-0.7, 1.2, 4.5, -4)))

pnorm(0, 1.1456, 0.933)
pnorm(8, 1.1456, 0.933, lower.tail = FALSE)
