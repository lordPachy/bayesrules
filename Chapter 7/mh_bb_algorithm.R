# Imports
library(tidyverse)
library(bayesplot)
library(bayesrules)

one_mh_iteration = function(y, trials, alpha_par, beta_par, current){
  # 1. Proposing the next chain location (uniformly at random)
  proposal = rbeta(1, alpha_par, beta_par)
  
  # 2. Calculate plausibility
  proposal_plaus = dbeta(proposal, alpha_par, beta_par) * dbinom(x=y, size=trials, prob=proposal)
  current_plaus = dbeta(current, alpha_par, beta_par) * dbinom(x=y, size=trials, prob=current)
  proposal_q = dbeta(proposal, alpha_par, beta_par)
  current_q = dbeta(current, alpha_par, beta_par)
  
  # 3. Running simulation
  alpha = min(1, (proposal_plaus/current_plaus)*(current_q /proposal_q))
  next_stop = sample(c(proposal, current), size = 1, prob = c(alpha, 1-alpha))
  
  return (data.frame(proposal, alpha, next_stop))
}

mh_tour = function(start_pos, y, trials, alpha_par, beta_par, N){
  # 1. Initializing the chain at 3
  current = start_pos
  pi = rep(0, N)
  
  # 2. Simulating N steps
  
  for (i in 1:N){
    sim = one_mh_iteration(y, trials, alpha_par, beta_par, current)
    pi[i] = sim$next_stop
    current = sim$next_stop
  }
  
  return(data.frame(iteration = c(1:N), pi))
}

# Testing the algorithm
set.seed(84735)
mh_simulation_1 = mh_tour(start_pos =0.5, y = 60, trials = 100, alpha_par = 1, beta_par = 1, N = 5000)

# Simulated distribution
ggplot(mh_simulation_1, aes(x = mu)) + 
  geom_histogram(aes(y = ..density..), color = "white", bins = 20)

# Trace plot
ggplot(mh_simulation_1, aes(x = iteration, y = mu)) + geom_line()

# Example using New York rainfall data
#install.packages("nycflights13")
library(nycflights13)
rainfall_data = data(weather)
rainfall_data = weather %>% select(month, day, hour, precip)
rainy_hours_data = rainfall_data %>% filter(month >= 9 & precip > 0) %>% mutate(rainy_hours = n())

# Thus, we have 459 rainy hours on 8656 total hours in fall months. We can now estimate such proportion
plot_beta(1, 3)
mh_simulation_rain = mh_tour(start_pos = 0.3, 459, 8656, alpha_par = 1, beta_par = 3, N = 1000)

# Simulated distribution
ggplot(mh_simulation_rain, aes(x = pi)) + 
  geom_histogram(aes(y = ..density..), color = "white", bins = 20)

# Trace plot
ggplot(mh_simulation_1, aes(x = iteration, y = mu)) + geom_line()