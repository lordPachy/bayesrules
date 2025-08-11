# Load packages
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)

ggplot(bikes, aes(y = rides, x = temp_feel)) + 
  geom_point(size = 0.2) + 
  geom_smooth(method = "lm", se = FALSE)

bike_model <- stan_glm(rides ~ temp_feel, data = bikes,
                       family = gaussian,
                       prior_intercept = normal(5000, 1000),
                       prior = normal(100, 40), 
                       prior_aux = exponential(0.0008),
                       chains = 4, iter = 5000*2, seed = 84735)


# Posterior predictive check
bike_model_df = as.data.frame(bike_model)
first_set = head(bike_model_df, 1)
first_set

beta_0 <- first_set$`(Intercept)`
beta_1 <- first_set$temp_feel
sigma  <- first_set$sigma
set.seed(84735)
one_simulation <- bikes %>% 
  mutate(mu = beta_0 + beta_1 * temp_feel,
         simulated_rides = rnorm(500, mean = mu, sd = sigma)) %>% 
  select(temp_feel, rides, simulated_rides)

head(one_simulation, 2)
