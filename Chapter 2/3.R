# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)

# Import dataset
data(pop_vs_soda)
?pop_vs_soda

# Summarize pop use by region
pop_vs_soda %>% tabyl(pop, region) %>% adorn_percentages("col")

# Chess analysis
chess = data.frame(pi = c(0.2, 0.5, 0.8))
prior = c(0.1, 0.25, 0.65)
set.seed(0)

chess_sim = sample_n(chess, size=10000, weight=prior, replace=TRUE)
# Simulating 10000 match outcomes
chess_sim = chess_sim %>% mutate(y = rbinom(10000, size = 6, prob=pi))
chess_sim %>%  head(3)
chess_sim %>% tabyl(pi) %>% adorn_totals('row')
ggplot(chess_sim, aes(x=y)) + stat_count(aes(y=..prop..)) + facet_wrap (~pi)

# Calculating posterior by filtering on the one-victories
win_one = chess_sim %>% filter(y==1)
win_one %>% tabyl(pi) %>% adorn_totals("row")
ggplot(win_one, aes(x = pi)) + geom_bar()
