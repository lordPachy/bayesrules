# Imports
library(bayesrules)
library(tidyverse)

# Tuning beta prior
plot_beta(45, 55)

# Showing prior, likelihood and posterior together
plot_beta_binomial(alpha=45, beta=55, y=30, n=50)

# Summarizing info
summarize_beta_binomial(alpha=45, beta=55, y=30, n=50)

# Simulating the Beta-Binomial
# Beta prior and simulating likelihood
set.seed(84735)
michelle_sim = data.frame(pi = rbeta(10000, 45, 55)) %>% 
  mutate(y = rbinom (10000, size = 50, prob = pi))

# Plotting posterior
ggplot(michelle_sim, aes(x = pi, y = y)) +
  geom_point(aes(color = (y == 30)), size = 0.1)

# Keeping only posterior that matches y == 30
michelle_posterior = michelle_sim %>% filter(y == 30)
ggplot(michelle_posterior, aes(x = pi)) + geom_density()

# Extracting posterior features from sampled posterior
michelle_posterior %>% summarize(mean(pi), sd(pi))
