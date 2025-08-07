# Load packages
library(bayesrules)
library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(janitor)
library(broom.mixed)

# Loading and plotting data
data(bikes)
ggplot(bikes, aes(x = temp_feel, y = rides)) + geom_point(size = 0.5) + geom_smooth(method = "lm", se = FALSE)

# Posterior simulation
bike_model <- stan_glm(rides ~ temp_feel, data = bikes,
                       family = gaussian,
                       prior_intercept = normal(5000, 1000),
                       prior = normal(100, 40), 
                       prior_aux = exponential(0.0008),
                       chains = 4, iter = 5000*2, seed = 84735)

# Simulation diagnostics
neff_ratio(bike_model)
rhat(bike_model)

mcmc_trace(bike_model, size = 0.1)
mcmc_dens_overlay(bike_model)

# Simulation through rstan
# STEP 1: DEFINE the model
stan_bike_model <- "
  data {
    int<lower = 0> n;
    vector[n] Y;
    vector[n] X;
  }
  parameters {
    real beta0;
    real beta1;
    real<lower = 0> sigma;
  }
  model {
    Y ~ normal(beta0 + beta1 * X, sigma);
    beta0 ~ normal(-2000, 1000);
    beta1 ~ normal(100, 40);
    sigma ~ exponential(0.0008);
  }
"

stan_bike_sim = stan(model_code = stan_bike_model, data = list(n = nrow(bikes), Y = bikes$rides, X = bikes$temp_feel, chains = 4, iter = 5000*2, seed = 84725))

# Posterior summary statistics
tidy(bike_model, effects = c("fixed", "aux", conf.int = TRUE, conf.level = 0.8))

# Storing the 4 chains for each parameter in 1 data frame
bike_model_df = as.data.frame(bike_model)

nrow(bike_model_df)
head(bike_model_df, 3)

# We can pick 50 of the simulated lines and plot them out
bikes %>% add_fitted_draws(bike_model, n = 50) %>% ggplot(aes(x = temp_feel, y = rides)) + geom_line(aes(y = .value, group = .draw), alpha = 0.15) + geom_point(data =bikes, size = 0.05)

# We can pick 50 of the simulated lines and plot them out
bikes %>% add_predicted_draws(bike_model, n = 4) %>% ggplot(aes(x = temp_feel, y = rides)) + geom_point(aes(y = .prediction, group = .draw), size = 0.15) + facet_wrap(~.draw)

# Posterior prediction
# Single case
first_set = head(bike_model_df, 1)
mu = first_set$`(Intercept)` + first_set$temp_feel * 75
mu
y_new = rnorm(1, mean = mu, sd = first_set$sigma)
y_new
# Multiple case: we get a distribution
predict_75 = bike_model_df %>% mutate(mu = `(Intercept)` + temp_feel * 75, y_new = rnorm(20000, mean = mu, sd = sigma))

# Construct 80% posterior credible intervals
predict_75 %>% 
  summarize(lower_mu = quantile(mu, 0.025),
            upper_mu = quantile(mu, 0.975),
            lower_new = quantile(y_new, 0.025),
            upper_new = quantile(y_new, 0.975))


# Plot the posterior model of the typical ridership on 75 degree days
ggplot(predict_75, aes(x = mu)) + 
  geom_density()

# Plot the posterior predictive model of tomorrow's ridership
ggplot(predict_75, aes(x = y_new)) + 
  geom_density()

# It is possible to do the same with rstanarm
shortcut_prediction = posterior_predict(bike_model, newdata = data.frame(temp_feel = 75))

posterior_interval(shortcut_prediction, prob = 0.95)
mcmc_dens(shortcut_prediction)

# Default priors
bike_model_default = stan_glm(
  rides ~ temp_feel, data = bikes,
  family = gaussian,
  prior_intercept = normal(5000, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735
)
prior_summary(bike_model_default)

# Perform a prior simulation 
bike_default_priors <- update(bike_model_default, prior_PD = TRUE)

# 200 prior model lines
bikes %>%
  add_fitted_draws(bike_default_priors, n = 200) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_line(aes(y = .value, group = .draw), alpha = 0.15)

# 4 prior simulated datasets
set.seed(3)
bikes %>%
  add_predicted_draws(bike_default_priors, n = 4) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_point(aes(y = .prediction, group = .draw)) + 
  facet_wrap(~ .draw)
