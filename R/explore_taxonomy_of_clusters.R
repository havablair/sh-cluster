library(tidyverse)
library(glue)

d <- read_csv("data/mukey_category_assignments_for_count_tables.csv")


d %>% 
  filter(k_8 == "Cluster_8") %>% 
  count(taxorder)


d %>% 
  filter(k_8 == "Cluster_8") %>% 
  count(taxsuborder)


d %>% 
  filter(k_8 == "Cluster_8") %>% 
  count(taxgrtgroup)

d %>% 
  filter(k_8 == "Cluster_8") %>% 
  count(taxsubgrp)
