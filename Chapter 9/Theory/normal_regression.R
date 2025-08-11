# Load packages
library(bayesrules)
library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(janitor)
library(broom.mixed)

# Since we are in a Bayesian setting, the usual framework must be adhered to:
# Prior model and data model contribute to forming the posterior model.
# Thus, each parameter of regression has its own prior, and they are considered to be independent

plot_normal(mean = 5000, sd = 1000) + labs(x = "beta_0c", y = "pdf")

plot_normal(mean = 100, sd = 40) + labs(x = "beta_1c", y = "pdf")

plot_gamma(shape = 1, rate = 0.0008) + labs(x = "sigma", y = "pdf")

