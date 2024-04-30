
library(tidyverse)
library(glue)


# this has climate clusters by mukey, need to join with the other validation data
clim_mukeys <- read_csv("data/mukey_cluster_assignments_and_props_climate_only.csv") %>% 
  rename_with(.fn = ~str_replace(.x, "k_", "clim_k_"), .cols = contains("k_")) 

ggplot(data = clim_mukeys) + 
  geom_boxplot(aes(x = clim_k_3, y = annprcp_norm)) +
  ggtitle("MAP")

ggplot(data = clim_mukeys) + 
  geom_boxplot(aes(x = clim_k_3, y = anntavg_norm)) +
  ggtitle("MAT")
  


map_lm <- lm(annprcp_norm ~ clim_k_3, data = clim_mukeys)
mat_lm <- lm(anntavg_norm ~ clim_k_3, data = clim_mukeys)

summary(mat_lm)


