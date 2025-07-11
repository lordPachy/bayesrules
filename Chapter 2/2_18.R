# Exercise 2.18

# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)

# Simulating prior: each possible value of pi
lactose <- data.frame(pi = c(0.4, 0.5, 0.6, 0.7))
prior <- c(0.1, 0.2, 0.44, 0.26)
lactose_sim = sample_n(lactose, size=10000, weight=prior, replace=TRUE)

lactose_sim %>% tabyl(pi) %>% adorn_totals('row')

# Simulating lactose tests
lactose_sim = lactose_sim %>% mutate(y = rbinom(n = 10000, size = 800, prob=pi))
lactose_sim %>%  head(3)
ggplot(lactose_sim, aes(x=y)) + stat_count(aes(y=..prop..)) + facet_wrap (~pi)

# Calculating posteriors by filtering
rel_experiment = lactose_sim %>% filter(y==470)
rel_experiment %>% tabyl(pi) %>% adorn_totals("row")
ggplot(rel_experiment, aes(x = pi)) + geom_bar()
