# originally, I made violin plots using all the SSURGO aggregated map unit
# data. But because I want to display the stat pairwise results from 
# the validation data, I think it makes more sense to plot the validation 
# data on its own. Can always consider adding something to the side like weightedmean & SD 
# from the cluster data table as a reference to show how the validation data distribution
# lines up with expected props for each group.



# setup -------------------------------------------------------------------


library(tidyverse)
library(glue)
library(patchwork)
library(ggdist)
library(ggbeeswarm)


valdat <- read_csv("data/validation_data_pca_clusters_and_soil_props.csv") %>% 
  filter(!is.na(k_8), 
         k_8 != 0) %>% 
  select(-c(k_4:k_7, k_9, k_10, k_11), 
         -c(silttotal, sandtotal, lep, awc, cec, ec,
            fragvol_sum, ic_wt_percent, c_est_org)) %>% 
  mutate(k_8 = glue("c{k_8}")) %>% 
  rename(Clay = claytotal, 
         OM = om_loi, 
         CaCO3 = caco3, 
         BD = dbthirdbar, 
         pH = ph1to1h2o) %>% 
  pivot_longer(-c(val_unit_id, k_8), values_to = "value", names_to = "soil_var")



cld_dat <- read_csv("data/cld_display_welch.csv") %>% 
  filter(k_opt == "k_8") %>% 
  mutate( soil_var = case_when(
    str_detect(soil_var, "clay") ~ "Clay",
    str_detect(soil_var, "om") ~ "OM",
    str_detect(soil_var, "caco3") ~ "CaCO3",
    str_detect(soil_var, "db") ~ "BD",
    str_detect(soil_var, "ph") ~ "pH",
  ), 
  group = str_replace(group, "lust_", "")) %>% 
  rename(cluster = group, 
         var_name = soil_var, 
         cluster_opts = k_opt)



plot_colors <- c("#FF5A5F", "#FFB400", 
                          "#007A87", "#8CE071", "#7B0051", 
                          "#00D1C1", "#FFAA91", "#B4A76C", 
                          "#9CA299", "#565A5C", "#00A04B", 
                          "#E54C20")
                          

# we don't have cld values for every cluster, b/c in some cases
# we didn't have any validation points in that cluster (8/histosols)
# and in others we only had one point (not enough for stat test). 
# I'd like these to be labelled with "--" in the plots to indicate
# that they don't have any stats associated with them. 
# to do that, need to make sure we have lines for each cluster/variable
# combination in our cld_pairs dataset.
all_clusts <- glue("c{1:8}") 
all_vars <- c("Clay", "OM", "CaCO3", "BD", "pH")

cld_all_df <- tidyr::crossing(all_clusts, all_vars) %>% 
  rename(cluster = all_clusts,
         var_name = all_vars) %>% 
  mutate(cluster = as.factor(cluster),
         cluster_opts = "k_8")

cld_labels <- left_join(cld_all_df, cld_dat,
                        by = c("cluster", "var_name", "cluster_opts")) %>% 
  replace_na(list(cld = "--", spaced_cld = "--"))

# halfeye single plots ----------------------------------------------------


plot_halfeye_single <- function(plotvar, unit_string, bw = NA,
                                label_y_adj = 1){
  
  dsub <- valdat %>% 
    filter(soil_var == plotvar) %>% 
    drop_na(value) 
  
  # used for cld label y position
  max_y <- dsub %>% pull(value) %>% max()
  
  cld_sub <- cld_labels %>% 
    filter(var_name == plotvar) %>% 
    mutate(y_pos = max_y)
  
  ggplot() +
    stat_dotsinterval(data = dsub, aes(x = k_8,
                                       y = value,
                                       fill = k_8,
                                       slab_color = k_8),
                 #adjust = adj_bw,
                 show.legend = FALSE, 
                 layout = "swarm",
                 overflow = "compress",
                 binwidth = c(bw),
                 shape = 19,
                 slab_shape = 21) + 
    geom_text(data = cld_sub, aes(x = cluster, y = y_pos*label_y_adj, label = cld)) +
    scale_fill_discrete(type = plot_colors) + 
    scale_color_manual(values = plot_colors, aesthetics = "slab_color") +
    xlab("Cluster") +
    ylab(glue("{plotvar} {unit_string}")) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8))
  
  
}

plot_halfeye_single("CaCO3", "(wt %)", bw = 0.45, label_y_adj = 1.05) +
  scale_y_continuous(breaks = seq(0, 25, 5))


plot_halfeye_single("Clay", "(%)") +
  scale_y_continuous(breaks = c(seq(0, 70, 10)))


# save halfeye plots ------------------------------------------------------

clay_he <- plot_halfeye_single("Clay", "(%)", label_y_adj = 1.05, bw = 1.2) +
  scale_y_continuous(breaks = c(seq(0, 70, 10)))

bd_he <- plot_halfeye_single("BD", "(g/cm3)", label_y_adj = 1.05, bw = 0.02) +
  scale_y_continuous(breaks = seq(0, 1.8, 0.2))

om_he_log <- plot_halfeye_single("OM", "(% LOI)", label_y_adj = 1.1) +
  scale_y_log10(breaks = c(1, 2, 3, 4, 5, 10, 20, 50, 100))

ph_he <- plot_halfeye_single("pH", "", label_y_adj = 1.05, bw = 0.075) +
  scale_y_continuous(breaks = seq(5, 8.5, 0.5))

caco3_he <- plot_halfeye_single("CaCO3", "(wt %)", bw = 0.5, label_y_adj = 1.05) +
  scale_y_continuous(breaks = seq(0, 25, 5))


ggsave("figs/clay_dist_halfeye.png", plot = clay_he, width = 4, height = 3, units = "in")
ggsave("figs/bd_dist_halfeye.png", plot = bd_he, width = 4, height = 3, units = "in")
ggsave("figs/omlog_dist_halfeye.png", plot = om_he_log, width = 4, height = 3, units = "in")
ggsave("figs/ph_dist_halfeye.png", plot = ph_he, width = 4, height = 3, units = "in")
ggsave("figs/caco3_dist_halfeye.png", plot = caco3_he, width = 4, height = 3, units = "in")



halfeye_patch <-
  (clay_he + bd_he) / (om_he_log + ph_he) / (caco3_he +
                                               theme(plot.margin = unit(c(0, 30, 0, 0), "pt"))+ plot_spacer())

halfeye_patch

ggsave("figs/dpi300/halfeye_multiplot.tiff", halfeye_patch, height = 8, width = 8, units = "in")
