# Imports 
library(tidyverse)
library(bayesrules)
library(janitor)

# Plotting
plot_gamma(shape = 10, rate = 2)
plot_gamma_poisson(10, 2, 11, 4)

# Simulating
prior = data.frame(pi = rgamma(10000, 10, 2))
ggplot(prior, aes(x = pi)) + geom_density()
simulation = prior %>% mutate(y1 = rpois(10000, pi), y2 = rpois(10000, pi), y3 = rpois(10000, pi), y4 = rpois(10000, pi))
simulation_exp = simulation %>% filter(y1 + y2 + y3 + y4 == 11)

ggplot(simulation_exp, aes(x = pi)) + geom_density()
