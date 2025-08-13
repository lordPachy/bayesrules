# Load packages
library(bayesrules)
library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(janitor)
library(broom.mixed)

# Importing data
data("coffee_ratings")
coffee_ratings <- coffee_ratings %>% 
  select(farm_name, total_cup_points, aroma, aftertaste)

# There are multiple observations for each variable, thus
# the observations are not independent
head(coffee_ratings)
summarise(coffee_ratings)
coffee_ratings

# We can just pick an observation per variable
set.seed(84735)
new_coffee <- coffee_ratings %>% 
  group_by(farm_name) %>% 
  sample_n(1) %>% 
  ungroup()
dim(new_coffee)

# Simulation
coffee_model <- stan_glm(total_cup_points ~ aroma, data = new_coffee,
                       family = gaussian,
                       prior_intercept = normal(75, 10^2),
                       chains = 4, iter = 5000*2, seed = 84735)

# Simulation diagnostics
neff_ratio(coffee_model)
rhat(coffee_model)

mcmc_trace(coffee_model, size = 0.1)
mcmc_dens_overlay(coffee_model)

# Plotting simulation lines and generated datasets
# We can pick 50 of the simulated lines and plot them out
new_coffee %>% add_fitted_draws(coffee_model, n = 50) %>% ggplot(aes(x = aroma, y = total_cup_points)) + geom_line(aes(y = .value, group = .draw), alpha = 0.15) + geom_point(data =new_coffee, size = 0.05)

# We can pick 50 of the simulated lines and plot them out
new_coffee %>% add_predicted_draws(coffee_model, n = 4) %>% ggplot(aes(x = aroma, y = total_cup_points)) + geom_point(aes(y = .prediction, group = .draw), size = 0.15) + facet_wrap(~.draw)

# Plotting the distribution of beta 1
ggplot(data = coffee_model, aes(x = ))
coffee_model$aroma

# Posterior predictive check
coffee_model_df = as.data.frame(coffee_model)
ggplot(coffee_model_df, aes(x = aroma)) + geom_density() +
  geom_vline(xintercept = median(coffee_model_df$aroma))
first_set = head(coffee_model_df, 1)
posterior_interval(coffee_model, prob = 0.95)

beta_0 <- first_set$`(Intercept)`
beta_1 <- first_set$temp_feel
sigma  <- first_set$sigma
set.seed(84735)
one_simulation <- bikes %>% 
  mutate(mu = beta_0 + beta_1 * temp_feel,
         simulated_rides = rnorm(500, mean = mu, sd = sigma)) %>% 
  select(temp_feel, rides, simulated_rides)

head(one_simulation, 2)

# Although the simulation is not good pointwise, we can look at the distributions
ggplot(one_simulation, aes(x = simulated_rides)) + 
  geom_density(color = 'lightblue') +   geom_density(aes(x = rides), color = 'darkblue')

s# This has been done for one of the 20000 retrieved lines. We can pick 50 points for each of the retrieved lines and look at the result
pp_check(bike_model, nreps = 50) + xlab("rides")

# Point-wise prediction discussion
bikes %>% 
  filter(date == "2012-10-22") %>% 
  select(temp_feel, rides)

# Simulate the posterior predictive model
set.seed(84735)

predict_75 <- bike_model_df %>% 
  mutate(mu = `(Intercept)` + temp_feel*75,
         y_new = rnorm(20000, mean = mu, sd = sigma))

# Plot the posterior predictive model
ggplot(predict_75, aes(x = y_new)) + 
  geom_density()

predict_75 %>% 
  summarize(mean = mean(y_new), error = 6228 - mean(y_new))

predict_75 %>% 
  summarize(sd = sd(y_new), error = 6228 - mean(y_new),
            error_scaled = error / sd(y_new))

predict_75 %>% 
  summarize(lower_95 = quantile(y_new, 0.025),
            lower_50 = quantile(y_new, 0.25),
            upper_50 = quantile(y_new, 0.75),
            upper_95 = quantile(y_new, 0.975))

# In order to assess the point-wise quality of our predictive model, we can look at what each of the 20000 models predicted for each of the 500 days

predictions = posterior_predict(bike_model, newdata = bikes)
dim(predictions)

ppc_intervals(bikes$rides, yrep = predictions, x = bikes$temp_feel, prob = 0.5, prob_outer = 0.95)

# These statistics can also be summarized numerically
prediction_summary(bike_model, data = bikes)

# Leave-one-out + expected log-predictive density
model_elpd = loo(bike_model)
model_elpd$estimates
