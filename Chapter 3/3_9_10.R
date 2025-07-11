# Imports
library('bayesrules')
library('tidyverse')

# Calculating summary statistics
summarize_beta(8, 2)
plot_beta(8, 2)

summarize_beta(1, 20)
plot_beta(1, 20)

# Plotting priors and posteriors
plot_beta_binomial(8, 2, 12, 50)
plot_beta_binomial(1, 20, 12, 50)

# Summarizing statistics
summarize_beta_binomial(8, 2, 12, 50)
summarize_beta_binomial(1, 20, 12, 50)
                        