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
    pi ~ beta(2, 2);
  }
"

# Simulating the posterior
bb_sim = stan(model_code = bb_model, data = list(Y=9), chains=4, iter = 5000*2, seed=84735)

# Showing values of pi
as.array(bb_sim, pars="pi") %>% head(4)

# Plotting
mcmc_trace(bb_sim, pars = "pi", size = 0.1)
mcmc_hist(bb_sim, pars = "pi") + yaxis_text(TRUE) + ylab("count")
mcmc_dens(bb_sim, pars = "pi") + yaxis_text(TRUE) + ylab("density")


# Density plots of individual chains
mcmc_dens_overlay(bb_sim, pars = "pi") + ylab("density")

# It is possible to see how long we need such chains
bb_sim_short = stan(model_code = bb_model, data = list(Y=9), chains=4, iter=50*2, seed=84735)

# Plotting short chain
mcmc_trace(bb_sim_short, pars="pi")
mcmc_dens_overlay(bb_sim_short, pars="pi")

# Calculating the effective sample size ratios
neff_ratio(bb_sim, pars="pi")
neff_ratio(bb_sim_short, pars="pi")

mcmc_acf(bb_sim, pars="pi")

# Reducing autocorrelation: bad way
thinned_sim = stan(model_code = bb_model, data = list(Y = 9), chains = 4, iter = 5000*2, seed = 84735, thin = 10)

# Check out the results
mcmc_trace(thinned_sim, pars = "pi")
mcmc_acf(thinned_sim, pars = "pi")

