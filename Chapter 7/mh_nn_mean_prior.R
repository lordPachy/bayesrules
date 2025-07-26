# Imports
library(tidyverse)

one_mh_iteration = function(y, std_prop, std_plaus, current){
  # 1. Proposing the next chain location (uniformly at random)
  proposal = rnorm(1, mean=current, std_prop )
  
  # 2. Calculate plausibility
  proposal_plaus = dnorm(proposal, 0, 1) * dnorm(y, proposal, std_plaus)
  current_plaus = dnorm(current, 0, 1) * dnorm(y, current, std_plaus)
  q_proposal = dnorm(proposal, current, std_prop)
  q_current = dnorm(current, proposal, std_prop)
  
  # 3. Running simulation
  alpha = min(1, (proposal_plaus/current_plaus)*(q_current/q_proposal))
  next_stop = sample(c(proposal, current), size = 1, prob = c(alpha, 1-alpha))
  
  return (data.frame(proposal, alpha, next_stop))
}

mh_tour_normal = function(start_pos, y, std_prop, std_plaus, N){
  # 1. Initializing the chain at 3
  current = start_pos
  mu = rep(0, N)
  
  # 2. Simulating N steps
  
  for (i in 1:N){
    sim = one_mh_iteration(y=y, std_prop=std_prop, std_plaus=std_plaus, current=current)
    mu[i] = sim$next_stop
    current = sim$next_stop
  }
  
  return(data.frame(iteration = c(1:N), mu))
}

# Testing the algorithm
set.seed(84735)
mh_simulation_1 = mh_tour_normal(start_pos =3, y = 6.25, std_plaus = 0.75, std_prop=100, N = 100)

# Simulated distribution
ggplot(mh_simulation_1, aes(x = mu)) + 
  geom_histogram(aes(y = ..density..), color = "white", bins = 20)

# Trace plot
#ggplot(mh_simulation_1, aes(x = iteration, y = mu)) + geom_line()

# One-step tests
#one_mh_iteration(y=6.25, std_plaus=0.75, std_prop=0.5, w=3, current=3)

