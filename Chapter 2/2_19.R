# Exercise 2.19

# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)
set.seed(0)

# Simulating prior: each possible value of pi
cuckoo <- data.frame(pi = c(0.6, 0.65, 0.7, 0.75))
prior <- c(0.3, 0.4, 0.2, 0.1)
cuckoo_sim = sample_n(cuckoo, size=10000, weight=prior, replace=TRUE)

cuckoo_sim %>% tabyl(pi) %>% adorn_totals('row')

# Simulating lactose tests
cuckoo_sim = cuckoo_sim %>% mutate(y = rbinom(n = 10000, size = 15, prob=pi))
cuckoo_sim %>%  head(3)
ggplot(cuckoo_sim, aes(x=y)) + stat_count(aes(y=..prop..)) + facet_wrap (~pi)

# Calculating posteriors by filtering
rel_experiment = cuckoo_sim %>% filter(y==10)
rel_experiment %>% tabyl(pi) %>% adorn_totals("row")
ggplot(rel_experiment, aes(x = pi)) + geom_bar()
