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
ggplot(bikes, aes(x = windspeed, y = rides)) + geom_point(size = 0.5) + geom_smooth(method = "lm", se = FALSE)

# PRIOR UNDERSTANDING
# Posterior simulation
bike_model <- stan_glm(rides ~ windspeed, data = bikes,
                       family = gaussian,
                       prior_intercept = normal(3000, 1500^2),
                       prior = normal(-100, 5^2), 
                       prior_aux = exponential(1/2000),
                       chains = 5, iter = 8000*2, seed = 84735, prior_PD = TRUE)

# Simulation diagnostics
neff_ratio(bike_model)
rhat(bike_model)

mcmc_trace(bike_model, size = 0.1)
mcmc_dens_overlay(bike_model)

# Posterior summary statistics
tidy(bike_model, effects = c("fixed", "aux", conf.int = TRUE, conf.level = 0.8))

# Storing the 4 chains for each parameter in 1 data frame
bike_model_df = as.data.frame(bike_model)

nrow(bike_model_df)
head(bike_model_df, 3)

# We can pick 100 of the simulated lines and plot them out
bikes %>% add_fitted_draws(bike_model, n = 100) %>% ggplot(aes(x = windspeed, y = rides)) + geom_line(aes(y = .value, group = .draw), alpha = 0.15) + geom_point(data = bikes, size = 0.05)

# We can simulate 4 datasets from the simulated lines (!) and plot them out
bikes %>% add_predicted_draws(bike_model, n = 4) %>% ggplot(aes(x = windspeed, y = rides)) + geom_point(aes(y = .prediction, group = .draw), size = 0.15) + facet_wrap(~.draw)

# POSTERIOR UNDERSTANDING
# Posterior simulation
bike_model <- update(bike_model, prior_PD = FALSE)

# Simulation diagnostics
neff_ratio(bike_model)
rhat(bike_model)

mcmc_trace(bike_model, size = 0.1)
mcmc_dens_overlay(bike_model)

# Posterior summary statistics
tidy(bike_model, effects = c("fixed", "aux", conf.int = TRUE, conf.level = 0.95))

# Storing the 4 chains for each parameter in 1 data frame
bike_model_df = as.data.frame(bike_model)

nrow(bike_model_df)
head(bike_model_df, 3)

# We can pick 100 of the simulated lines and plot them out
bikes %>% add_fitted_draws(bike_model, n = 100) %>% ggplot(aes(x = windspeed, y = rides)) + geom_line(aes(y = .value, group = .draw), alpha = 0.15) + geom_point(data = bikes, size = 0.05)

# We can simulate 4 datasets from the simulated lines (!) and plot them out
bikes %>% add_predicted_draws(bike_model, n = 4) %>% ggplot(aes(x = humidity, y = rides)) + geom_point(aes(y = .prediction, group = .draw), size = 0.15) + facet_wrap(~.draw)

# Tabulate the beta_1 values that exceed 0
bike_model_df %>% 
  mutate(exceeds_0 = humidity > 0) %>% 
  tabyl(exceeds_0)

# Posterior prediction
# Multiple case: we get a distribution
predict_90 = bike_model_df %>% mutate(mu = `(Intercept)` + humidity * 90, y_new = rnorm(40000, mean = mu, sd = sigma))

# Construct 80% posterior credible intervals
predict_90 %>% 
  summarize(lower_mu = quantile(mu, 0.1),
            upper_mu = quantile(mu, 0.9),
            lower_new = quantile(y_new, 0.1),
            upper_new = quantile(y_new, 0.9))


# Plot the posterior model of the typical ridership on 75 degree days
ggplot(predict_90, aes(x = mu)) + 
  geom_density()

# Plot the posterior predictive model of tomorrow's ridership
ggplot(predict_90, aes(x = y_new)) + 
  geom_density()

# It is possible to do the same with rstanarm
shortcut_prediction = posterior_predict(bike_model, newdata = data.frame(humidity = 90))

posterior_interval(shortcut_prediction, prob = 0.80)
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
