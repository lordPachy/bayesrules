# Imports
library(tidyverse)

set.seed(84735)
mc_tour = data.frame(mu = rnorm(5000, mean = 4, sd = 0.6))
ggplot(mc_tour, aes(x = mu)) + 
  geom_histogram(aes(y = ..density..), color = "white", bins = 15) + 
  stat_function(fun = dnorm, args = list(4, 0.6), color = "blue")

# Sampling uniformly a new position
current = 3
set.seed(8)
proposal = runif(1, min = current - 1, max = current + 1)
proposal

# Evaluating the plausibility of the current and old position
proposal_plaus = dnorm(proposal, 0, 1) * dnorm (6.25, proposal, 0.75)
proposal_plaus
current_plaus = dnorm(current, 0, 1) * dnorm(6.25, current, 0.75)
current_plaus

# Simulation: should we stay in the old position, or go the new one?
alpha = min(1, proposal_plaus/current_plaus)
alpha

next_stop = sample(c(proposal, current), size = 1, prob = c(alpha, 1 - alpha))
next_stop

# Defining a function packing all of this at once
one_mh_iteration = function(w, current){
  # 1. Proposing the next chain location (uniformly at random)
  proposal = runif(1, min = current - w, max = current + w)
  
  # 2. Calculate plausibility
  proposal_plaus = dnorm(proposal, 0, 1) * dnorm(6.25, proposal, 0.75)
  current_plaus = dnorm(current, 0, 1) * dnorm(6.25, current, 0.75)
  
  # 3. Running simulation
  alpha = min(1, proposal_plaus/current_plaus)
  next_stop = sample(c(proposal, current), size = 1, prob = c(alpha, 1-alpha))
  
  return (data.frame(proposal, alpha, next_stop))
}

one_mh_iteration(w=1, current=3)
set.seed(83)
one_mh_iteration(w=1, current=3)
