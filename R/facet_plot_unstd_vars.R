# goal: for defense presentation, make a facet plot with nice labels
# to show the distribution of the untransformed variables included in the
# cluster analysis

library(tidyverse)
library(glue)

d <- read_csv("data/clean_mu_weighted_soil_props.csv") %>% 
  rename_with(.fn = ~str_replace(.x, "_r_value", ""))


dlong <- d %>% 
  pivot_longer(-mukey,
               names_to = "soil_var")

var_plot <- dlong %>% 
  mutate(soil_var = case_when(
    soil_var == "awc" ~ "Avail. Water Capacity",
    soil_var == "caco3" ~ "Carbonates (%)",
    soil_var == "cec7" ~ "Cation Ex. Capacity (meq/100g)",
    soil_var == "claytotal" ~ "Clay %",
    soil_var == "dbthirdbar" ~ "Bulk Density (g/cm3)",
    soil_var == "ec" ~ "Electrical Conductivity (dS/m)",
    soil_var == "ksat" ~ "Sat. Hydraulic Cond. (um/s)",
    soil_var == "lep" ~ "Linear Extensibility (%)",
    soil_var == "om" ~ "Organic Matter (%)",
    soil_var == "ph1to1h2o" ~ "pH",
    TRUE ~ "fixme"
  )) %>% 
  ggplot() +
  geom_histogram(aes(x = value)) +
  facet_wrap(vars(soil_var), scales = "free") +
  theme_bw() +
  xlab("") +
  theme(strip.text = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

ggsave(filename = "figs/unstd_facet_plot.png", plot = var_plot,
       width = 16, height = 9, units = "in")
