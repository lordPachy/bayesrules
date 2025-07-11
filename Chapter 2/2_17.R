# Exercise 2.17

# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)

# Simulating mold vs healthy trees
trees <- data.frame(type = c("mold", "healthy"))
prior <- c(0.18, 1-0.18)
trees_sim = sample_n(trees, size=10000, weight=prior, replace=TRUE)

trees_sim %>% tabyl(type) %>% adorn_totals('row')

# Adding mold likelihoods
trees_sim = trees_sim %>% mutate(elm = case_when(type == "mold" ~ 0.15,
                                                        type == "healthy" ~ 0.2))
trees_sim = trees_sim %>% mutate(maple = case_when(type == "mold" ~ 0.8,
                                                 type == "healthy" ~ 0.1)) 
trees_sim = trees_sim %>% mutate(other = case_when(type == "mold" ~ 0.05,
                                                 type == "healthy" ~ 0.7)) 

glimpse(trees_sim)

# Simulating tree type
set.seed(0)
data = c('elm', 'maple', 'other')
trees_sim <- trees_sim %>% group_by(1:n()) %>%
  mutate(usage = sample(data, size = 1, prob = c(elm, maple, other)))

trees_sim %>% tabyl(usage, type) %>% adorn_percentages('row')

# Filtering on maple trees
trees_sim %>% 
  filter(usage == "maple") %>% 
  tabyl(type) %>% 
  adorn_totals("row")

ggplot(trees_sim, aes(x = type)) + 
  geom_bar() + 
  facet_wrap(~ usage)