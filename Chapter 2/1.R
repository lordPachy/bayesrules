# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)

# Import article data
data(fake_news)

fake_news %>% 
  tabyl(type) %>% 
  adorn_totals("row")

fake_news %>% 
  tabyl(title_has_excl, type) %>% 
  adorn_totals("row")

# Define possible articles
article <- data.frame(type = c("real", "fake"))

# Define the prior model
prior <- c(0.6, 0.4)

# Simulate 3 articles
sample_n(article, size = 3, weight = prior, replace = TRUE)
set.seed(84735)

# Simulate 3 articles
sample_n(article, size = 3, weight = prior, replace = TRUE)
