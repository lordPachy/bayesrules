# Imports
library(tidyverse)

set.seed(84735)
mc_tour = data.frame(mu = rnorm(5000, mean = 4, sd = 0.6))
ggplot(mc_tour, aes(x = mu)) + 
  geom_histogram(aes(y = ..density..), color = "white", bins = 15) + 
  stat_function(fun = dnorm, args = list(4, 0.6), color = "blue")
