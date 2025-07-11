# Imports
library(bayesrules)
library(tidyverse)
library(janitor)
data(bechdel, package = 'bayesrules')

# Take a sample of 20 movies
set.seed(84735)
bechdel_20 = bechdel %>% sample_n(20)

bechdel_20 %>% head(3)

# Percentage of passes
bechdel_20 %>% tabyl(binary) %>% adorn_totals('row')

# Differend data
bechdel %>% filter (year == 1991) %>% tabyl(binary) %>% adorn_totals('row')
