# Load packages
library(tidyverse)
library(janitor)
library(rstan)
library(bayesrules)

# Defining the model
bb_model = "
  data{
    int <lower = 0> Y[3];
  }
  parameters {
    real<lower = 0> lambda;
  }
  model {
    Y ~ poisson(lambda);
    lambda ~ gamma(20, 5);
  }
"

# Simulating the posterior
bb_sim = stan(model_code = bb_model, data = list(Y=c(0, 1, 0)), chains=4, iter = 10000*2, seed=84735)


# Plotting
mcmc_trace(bb_sim, pars = "lambda", size = 0.1)
mcmc_dens(bb_sim, pars = "lambda") + yaxis_text(TRUE) + ylab("density")

# Comparing to true value
plot_gamma_poisson(20, 5, 1, 3)
