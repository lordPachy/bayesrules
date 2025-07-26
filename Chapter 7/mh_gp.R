# Imports
library(tidyverse)

one_mh_iteration = function(y, s_prior, r_prior, rate, current){
  # 1. Proposing the next chain location (uniformly at random)
  proposal = rexp(1, rate = rate)
  
  # 2. Calculate plausibility
  proposal_plaus = dgamma(proposal, s_prior, r_prior) * dpois(y, proposal)
  current_plaus = dgamma(current, s_prior, r_prior) * dpois(y, current)
  q_proposal = dexp(proposal, rate = rate)
  q_current = dexp(current, rate = rate)
  
  # 3. Running simulation
  alpha = min(1, (proposal_plaus/current_plaus)*(q_current/q_proposal))
  next_stop = sample(c(proposal, current), size = 1, prob = c(alpha, 1-alpha))
  
  return (data.frame(proposal, alpha, next_stop))
}

mh_tour = function(start_pos, y, s_prior, r_prior, rate, N){
  # 1. Initializing the chain at 3
  current = start_pos
  lambda = rep(0, N)
  
  # 2. Simulating N steps
  for (i in 1:N){
    sim = one_mh_iteration(y, s_prior, r_prior, rate, current)
    lambda[i] = sim$next_stop
    current = sim$next_stop
  }
  
  return(data.frame(iteration = c(1:N), lambda))
}

# Testing the algorithm
set.seed(84735)
mh_simulation_1 = mh_tour(start_pos = 2, y = 4, s_prior = 1, r_prior = 0.1, rate = 0.5, N = 5000)

# Simulated distribution
ggplot(mh_simulation_1, aes(x = lambda)) + 
  geom_histogram(aes(y = ..density..), color = "white", bins = 20) + stat_function(fun = dgamma, args = list(1 + 4, 0.1 + 1), color = "blue")


# Trace plot
ggplot(mh_simulation_1, aes(x = iteration, y = lambda)) + geom_line()

# One-step tests
#one_mh_iteration(y=6.25, std=0.75, 3, 3)

