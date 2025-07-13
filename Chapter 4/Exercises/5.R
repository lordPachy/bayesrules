# Imports 
library(tidyverse)
library(bayesrules)
library(janitor)

# Visualization
plot_beta(1, 2)
plot_beta(0.5, 1)
plot_beta(3, 10)
plot_beta(2, 0.1)

# Prior
prior = data.frame(name ='kimya', pi = rbeta(10000, 1, 2))
prior = prior %>% rbind(data.frame(name = 'fernando', pi = rbeta(10000, 0.5, 1)))
prior = prior %>% rbind(data.frame(name = 'ciara', pi = rbeta(10000, 3, 10)))
prior = prior %>% rbind(data.frame(name = 'taylor', pi = rbeta(10000, 2, 0.1)))


# Simulation
ice_cream_sim = prior %>%  mutate(y = rbinom(40000, size = 7, prob = pi))
ice_cream_sim %>%  filter(name == 'taylor') %>% tabyl(y)

# Filtering posterior
ice_cream_exp = ice_cream_sim %>%  filter(y == 3)

ggplot(ice_cream_exp, aes(x = pi)) + 
  geom_density() + facet_wrap(~name)


