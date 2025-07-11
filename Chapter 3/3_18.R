# Imports
library('bayesrules')
library('tidyverse')

# Beta-binomial for Patrick
summarize_beta_binomial(3, 3, y=30, n=40)
plot_beta_binomial(3, 3, 30, 40)

# Beta-binomial for Harold
summarize_beta_binomial(3, 3, y=15, n=20)
plot_beta_binomial(3, 3, 15, 20)
