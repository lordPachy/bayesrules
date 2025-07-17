# Load packages
library(tidyverse)
library(janitor)
library(rstan)
library(bayesrules)
library(bayesplot)

# Defining the model
bb_model = "
  data{
    int <lower = 0, upper = 10> Y;
  }
  parameters {
    real<lower = 0, upper = 1> pi;
  }
  model {
    Y ~ binomial(10, pi);
    pi ~ beta(3, 8);
  }
"

# Simulating the posterior
bb_sim = stan(model_code = bb_model, data = list(Y=2), chains=3, iter = 12000*2, seed=84735)

# Plotting
mcmc_trace(bb_sim, pars = "pi", size = 0.1)
mcmc_hist_by_chain(bb_sim, pars = "pi") + yaxis_text(TRUE) + ylab("count")
mcmc_dens(bb_sim, pars = "pi") + yaxis_text(TRUE) + ylab("density")
