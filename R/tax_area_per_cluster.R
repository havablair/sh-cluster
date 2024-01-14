
# setup and data prep -----------------------------------------------------

library(tidyverse)
library(glue)
library(sf)
library(terra)
library(gt)

mu_dat <- read_csv("data/pca_mukey_cluster_assignments_and_soilprops.csv") %>% 
  select(MUKEY = mukey, k_8)

mu_pix <- read.delim("data/gSSURGO_MN/mukey_new_crosswalk.txt", sep = ",") %>% 
  select(MUKEY, MUKEY_New, Count)

mu <- left_join(mu_dat, mu_pix, by = "MUKEY")

incl_mukeys <- mu %>% pull(MUKEY)

# see section 7.7 of documentation
cokey <- read_csv("data/key_cokey_mukey_complete_cases_include.csv")

cotbl <- read_csv("data/component_list.csv") %>% 
  select(cokey, mukey, comppct_r, contains("tax"), contains("crop")) 

comps <- left_join(cokey, cotbl, by = c("cokey", "mukey")) %>% 
  filter(mukey %in% incl_mukeys)


# functions ---------------------------------------------------------------

# calculate representative taxonomy
calc_rep_tax <- function(tax_level, comp_df){
  
  comp_df %>% 
    select(mukey, cokey, comppct_r, {{tax_level}}) %>% 
    distinct() %>% 
    group_by(mukey, {{tax_level}}) %>% 
    summarise(tot_percent = sum(comppct_r), .groups = "drop") %>% 
    group_by(mukey) %>% 
    slice_max(tot_percent) %>% 
    select(-tot_percent)
  
}

rep_sub <- calc_rep_tax(tax_level = taxsuborder,
             comp_df = comps) %>% 
  # drop ties, they represent a small fraction of the overall area
  filter(mukey != 398725,
         mukey != 431547)

rep_order <- calc_rep_tax(tax_level = taxorder, 
                          comp_df = comps) %>% 
  # drop ties, they represent a small fraction of the overall area
  filter(mukey != 398725,
         mukey != 431547)

# ties: mukeys 398725, 431547
sub_ties <- rep_sub %>% group_by(mukey) %>% 
  count() %>% 
  filter(n>1)

# area in ties 
# MUKEY MUKEY_New Count
# 1 398725      1705 30224
# 2 431547      3449 15576
# 
# total area (pixels)
# 1212251517

rep_tax <- full_join(rep_order, rep_sub, by = "mukey")

tax_pix <- left_join(rep_tax, mu_pix, by = c("mukey" = "MUKEY"))
