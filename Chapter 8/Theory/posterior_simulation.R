# Load packages
library(bayesrules)
library(tidyverse)
library(rstan)
library(bayesplot)
library(broom.mixed)
library(janitor)

# Load data
data("moma_sample")

# 1. Defining the model
art_model = "
  data {
    int<lower = 0, upper = 100> Y;
  }
  parameters {
    real<lower = 0, upper = 1> pi;
  }
  model {
    Y ~ binomial(100, pi);
    pi ~ beta (4, 6);
  }
"

# 2. Posterior simulation
art_sim = stan(model_code = art_model, data = list(Y = 14), chains = 4, iter = 5000*2, seed = 84735)

# 3. Simulation diagnostics
mcmc_trace(art_sim, pars = "pi", size = 0.5) + xlab("iteration")
mcmc_dens_overlay(art_sim, pars = "pi")
mcmc_acf(art_sim, pars = "pi")

rhat(art_sim, pars = "pi")
neff_ratio(art_sim, pars = "pi")

# 4. Closed-form Beta posterior vs
# MCMC density approximation

plot_beta(alpha = 18, beta = 92) + lims(x = c(0, 0.35))

mcmc_dens(art_sim, pars = "pi") + lims(x = c(0, 0.35))

tidy(art_sim, conf.int = TRUE, conf.level = 0.95)
mcmc_areas(art_sim, pars = "pi", prob = 0.95)

# Mean and mode must be calculated directly
# Store the 4 chains in 1 data frame
art_chains_df <- as.data.frame(art_sim, pars = "lp__", include = FALSE)
dim(art_chains_df)

# Posterior summaries of pi
# Calculate posterior summaries of pi
art_chains_df %>% 
  summarize(post_mean = mean(pi), 
            post_median = median(pi),
            post_mode = sample_mode(pi),
            lower_95 = quantile(pi, 0.025),
            upper_95 = quantile(pi, 0.975))

# 5. Predicting a value of Y' for each pi value in the chain
art_chains_df = art_chains_df %>% mutate(y_predict = rbinom(length(pi), size = 20, prob = pi))

ggplot(art_chains_df, aes(x = y_predict)) + stat_count()
