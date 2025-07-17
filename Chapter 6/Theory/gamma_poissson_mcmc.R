# Load packages
library(tidyverse)
library(janitor)
library(rstan)
library(bayesrules)

# Defining the model
bb_model = "
  data{
    int <lower = 0> Y[2];
  }
  parameters {
    real<lower = 0> lambda;
  }
  model {
    Y ~ poisson(lambda);
    lambda ~ gamma(3, 1);
  }
"

# Simulating the posterior
bb_sim = stan(model_code = bb_model, data = list(Y=c(2, 8)), chains=4, iter = 5000*2, seed=84735)

# Showing values of lambda
as.array(bb_sim, pars="lambda") %>% head(4)

# Plotting
mcmc_trace(bb_sim, pars = "lambda", size = 0.1)
mcmc_hist(bb_sim, pars = "lambda") + yaxis_text(TRUE) + ylab("count")
mcmc_dens(bb_sim, pars = "lambda") + yaxis_text(TRUE) + ylab("density")
