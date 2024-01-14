# goal: make violin plots to display the distribution of untransformed soil
# properties, showing spatially weighted means


# setup -------------------------------------------------------------------

library(tidyverse)
library(glue)
library(patchwork)
library(ggdist)
library(ggbreak)


mu_info <- read_csv("data/target_mapunit_table.csv")

# mukeys, cluster assignments, and soil properties 
# note these are the untransformed soil prop values
prop <- read_csv("data/pca_mukey_cluster_assignments_and_soilprops.csv")

# weights are based on the total area of a given map unit in our AOI. 
# these are pixel counts. Since pixels are a uniform size, the count
# is directly proportional to area. 1 pixel is 10m x 10m or 100m^2, 
# so we could multiply by 100 to get m^2. Recall that 1ha = 10,000 m^2
mu_wts <- read.delim("data/gSSURGO_MN/mukey_new_crosswalk.txt", sep = ",") %>% 
  select(MUKEY, MUKEY_New, Count)

# load pairwise comparison cld (compact letter display) info 
# for labelling plots
comps <- read_csv("data/pca_pairwise_comparisons.csv")

d_cts <- left_join(prop, mu_wts, by = c("mukey" = "MUKEY")) %>% 
  select(-c(k_2:k_7, k_9:k_20))


plot_colors <- c("#FF5A5F", "#FFB400", 
                 "#007A87", "#8CE071", "#7B0051", 
                 "#00D1C1", "#FFAA91", "#B4A76C", 
                 "#9CA299", "#565A5C", "#00A04B", 
                 "#E54C20")

# pairwise comparisons w/ p-values                 
cld_pairs <- read_csv("data/cld_display.csv") %>%
  filter(cluster_opts == "k_8") %>%
  mutate(
    group = str_replace(group, "clust_", "c"),
    var_names = case_when(
      str_detect(var_names, "clay") ~ "Clay",
      str_detect(var_names, "om") ~ "OM",
      str_detect(var_names, "cec7") ~ "CEC",
      str_detect(var_names, "caco3") ~ "CaCO3",
      str_detect(var_names, "db") ~ "BD",
      str_detect(var_names, "_ec") ~ "EC",
      str_detect(var_names, "ph") ~ "pH",
      str_detect(var_names, "lep") ~ "LEP",
      str_detect(var_names, "ksat") ~ "Ksat",
      str_detect(var_names, "awc") ~ "AWC"
    ),
    group = as.factor(group)
  ) %>% 
  rename(var_name = var_names,
         cluster = group)

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

cld_labels <- left_join(cld_all_df, cld_pairs,
          by = c("cluster", "var_name", "cluster_opts")) %>% 
  replace_na(list(cld = "--", spaced_cld = "--"))
  

# prepare data for plotting -----------------------------------------------

# need to determine the total area (n pixels) in each cluster, so I can
# divide the pixel counts by the total cluster area to get a weight 
# relative to the size of the cluster 

clust_areas <- d_cts %>% 
  group_by(k_8) %>% 
  summarise(clust_pix_count = sum(Count),
            clust_mukey_count = n()) %>% 
  mutate(clust_area_m2 = clust_pix_count * 100,
         clust_area_ha = round(clust_area_m2 / 10000, digits = 0)) 

# this is where I determine the weights 
dwts <- left_join(d_cts, clust_areas, by = "k_8") %>% 
  mutate(weight_mukey_inclust = Count/clust_pix_count, 
         k_8 = as.factor(str_replace(k_8, "Cluster_", "c")))

# check that it worked, the weights should sum to 1 within each cluster 
check_wt <- dwts %>% 
  group_by(k_8) %>% 
  summarise(total_weight = sum(weight_mukey_inclust))

# set up for violin plots --------------------------------------------------

# calculate weighted and unweighted means for each soil prop 
dmeans <- dwts %>% 
  group_by(k_8) %>% 
  summarise(across(.cols = c("claytotal", "om", "cec7", "dbthirdbar", "ec",
                             "ph1to1h2o", "caco3", "lep", "ksat", "awc"),
                   .fns = list(wt_mean = ~weighted.mean(.x, w = weight_mukey_inclust),
                               mean = mean),
                   .names = "{.fn}_{.col}")) %>% 
  pivot_longer(-k_8, names_to = "var_name", values_to = "mean_value") %>% 
  mutate(mean_type = case_when(str_detect(var_name, "wt_mean") ~ "weighted mean",
                               TRUE ~ "unweighted mean")) %>% 
  mutate(var_name = case_when(
    str_detect(var_name, "clay") ~ "Clay",
    str_detect(var_name, "om") ~ "OM",
    str_detect(var_name, "cec7") ~ "CEC",
    str_detect(var_name, "caco3") ~ "CaCO3",
    str_detect(var_name, "db") ~ "BD",
    str_detect(var_name, "_ec") ~ "EC",
    str_detect(var_name, "ph") ~ "pH",
    str_detect(var_name, "lep") ~ "LEP",
    str_detect(var_name, "ksat") ~ "Ksat",
    str_detect(var_name, "awc") ~ "AWC"
  ))

dmeans_wide <- dwts %>% 
  group_by(k_8) %>% 
  summarise(across(.cols = c("claytotal", "om", "cec7", "dbthirdbar", "ec",
                             "ph1to1h2o", "caco3", "lep", "ksat", "awc"),
                   .fns = list(wt_mean = ~weighted.mean(.x, w = weight_mukey_inclust),
                                mean = mean),
                   .names = "{.fn}_{.col}"))

dlong <- dwts %>% 
  select(k_8, weight_mukey_inclust, claytotal, om, cec7, dbthirdbar, ec,
         ph1to1h2o, caco3, lep, ksat, awc) %>% 
  rename(
    Clay = claytotal,
    OM = om,
    CEC = cec7,
    BD = dbthirdbar,
    EC = ec,
    pH = ph1to1h2o,
    CaCO3 = caco3,
    LEP = lep,
    Ksat = ksat, 
    AWC = awc
  ) %>% 
  pivot_longer(-c(k_8, weight_mukey_inclust), names_to = "var_name",
               values_to = "value")

# test plots --------------------------------------------------------------

dwts %>% 
  ggplot(aes(x = k_8, y = claytotal)) +
  stat_eye() +
  theme_bw()

dwts %>% 
  ggplot(aes(x = k_8, y = claytotal)) +
  stat_eye() +
  theme_bw()

dwts %>% 
  ggplot(aes(x = k_8, y = claytotal, group = k_8, weight = Count)) +
  geom_violin()



dwts %>% 
  ggplot(aes(x = k_8, y = claytotal, weight = weight_mukey_inclust)) +
  geom_violin(show.legend = FALSE,
              lwd = 1) +
  geom_point(data = dmeans_wide,
             aes(x = k_8, y = wt_mean_claytotal),
             show.legend = FALSE) +
  scale_color_discrete(type = plot_colors) +
  theme_bw() +
  xlab("Cluster") +
  ylab("Clay (%)") +
  ggtitle("Areally weighted")


plot_not_weighted <- dwts %>%
  ggplot(aes(x = k_8, y = claytotal, color = k_8)) +
  geom_violin(show.legend = FALSE,
              lwd = 1) +
  geom_point(data = dmeans_wide,
             aes(x = k_8, y = mean_claytotal),
             show.legend = FALSE) +
  scale_color_discrete(type = plot_colors) +
  theme_bw() +
  xlab("Cluster") +
  ylab("Clay (%)") +
  ggtitle("Not areally weighted")

weight_comp <- plot_weighted + plot_not_weighted
  
# halfeye single plots ----------------------------------------------------


plot_halfeye_single <- function(plotvar, unit_string, adj_bw = 1,
                                label_y_adj = 1){
  
  dlong_sub <- dlong %>% 
    filter(var_name == plotvar) 
  
  # used for cld label y position
  max_y <- dlong_sub %>% pull(value) %>% max()
  
  cld_sub <- cld_labels %>% 
    filter(var_name == plotvar) %>% 
    mutate(y_pos = max_y)
  
  ggplot() +
    stat_halfeye(data = dlong_sub, aes(x = k_8, y = value, fill = k_8),
                 adjust = adj_bw,
                 show.legend = FALSE) + 
    geom_text(data = cld_sub, aes(x = cluster, y = y_pos*label_y_adj, label = cld)) +
  scale_color_discrete(type = plot_colors) + 
    xlab("Cluster") +
    ylab(glue("{plotvar} {unit_string}")) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8))
  
  
}


# save halfeye plots ------------------------------------------------------

clay_he <- plot_halfeye_single("Clay", "(%)") +
  scale_y_continuous(breaks = c(seq(0, 70, 10)))

bd_he <- plot_halfeye_single("BD", "(g/cm3)") +
  scale_y_continuous(breaks = seq(0, 1.8, 0.2))

om_he_log <- plot_halfeye_single("OM", "(% LOI)") +
  scale_y_log10(breaks = c(1, 2, 3, 4, 5, 10, 20, 50, 100))

ph_he <- plot_halfeye_single("pH", "", label_y_adj = 1.05) +
  scale_y_continuous(breaks = seq(5, 8.5, 0.5))

caco3_he <- plot_halfeye_single("CaCO3", "(wt %)", adj_bw = 2) +
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

ggsave("figs/halfeye_multiplot.png", height = 8, width = 6, units = "in")


# faceted ggplot violins -------------------------------------------------------

plot_violin_facets <- function(vars, wt = TRUE){
  
  dlong_sub <- dlong %>% 
    filter(var_name %in% vars) 
  
  if(isTRUE(wt)){
    
    dmeans_sub <- dmeans %>% 
      filter(var_name %in% vars, 
             mean_type == "weighted mean")
  }else{
    
    dmeans_sub <- dmeans %>% 
      filter(var_name %in% vars, 
             mean_type == "unweighted mean")
    
  }
  
  ggplot() +
    geom_violin(
      data = dlong_sub,
      aes(
        x = k_8,
        y = value,
        color = k_8
      ),
      lwd = 1,
      show.legend = FALSE
    ) +
    geom_point(
      data = dmeans_sub,
      aes(x = k_8, y = mean_value),
      show.legend = FALSE,
      color = "black"
    ) +
    scale_color_discrete(type = plot_colors) +
    theme_bw() +
    facet_wrap(vars(var_name), scales = "free")
  
  
}

plot_violin_facets(vars = c("Clay", "pH", "OM", "CaCO3", "BD"))



# faceted halfeye plots --------------------------------------------------

plot_halfeye_facets <- function(vars, wt = TRUE){
  
  dlong_sub <- dlong %>% 
    filter(var_name %in% vars) 
  
  if(isTRUE(wt)){
    
    dmeans_sub <- dmeans %>% 
      filter(var_name %in% vars, 
             mean_type == "weighted mean")
  }else{
    
    dmeans_sub <- dmeans %>% 
      filter(var_name %in% vars, 
             mean_type == "unweighted mean")
    
  }
  
  ggplot() +
    stat_halfeye(data = dlong_sub,
                 aes(x = k_8, y = value, fill = k_8)) +
    scale_color_discrete(type = plot_colors) +
    theme_bw() +
    facet_wrap(vars(var_name), scales = "free")
  
  
}

plot_halfeye_facets(vars = c("Clay", "pH", "OM", "CaCO3", "BD") )


# single ggplot violin -----------------------------------------------------------



plot_violin_single <- function(plotvar, unit_string, wt = TRUE){
  
  dlong_sub <- dlong %>% 
    filter(var_name == plotvar) 
  
  if(isTRUE(wt)){
    
    dmeans_sub <- dmeans %>% 
      filter(var_name  == plotvar, 
             mean_type == "weighted mean")
  }else{
    
    dmeans_sub <- dmeans %>% 
      filter(var_name  == plotvar, 
             mean_type == "unweighted mean")
    
  }
  
  ggplot() +
    geom_violin(
      data = dlong_sub,
      aes(
        x = k_8,
        y = value,
        color = k_8
      ),
      lwd = 1,
      show.legend = FALSE
    ) +
    geom_point(
      data = dmeans_sub,
      aes(x = k_8, y = mean_value),
      show.legend = FALSE,
      color = "black"
    ) +
    scale_color_discrete(type = plot_colors) + 
    xlab("Cluster") +
    ylab(glue("{plotvar} {unit_string}")) +
    theme_bw() 
  
  
}

clay_viol_wtmean <- plot_violin_single(plotvar = "Clay", unit_string = "(%)") 

ph_viol_wtmean <- plot_violin_single(plotvar = "pH",
                                     unit_string = "")

omlog_viol_wtmean <- plot_violin_single(plotvar = "OM",
                                        unit_string = "(% LOI)") +
  scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 50, 100)) 



# example code from ggdist (geom_slab) documentation --------------------------


# pix_wts <- dwts$Count / 1090532147
# 
# density(x = dwts$claytotal, weights = pix_wts)
# 
# df = expand.grid(
#   mean = 1:3,
#   input = seq(-2, 6, length.out = 100)
# ) %>%
#   mutate(
#     group = letters[4 - mean],
#     density = dnorm(input, mean, 1)
#   )
# 
# # orientation is detected automatically based on
# # use of x or y
# df %>%
#   ggplot(aes(y = group, x = input, thickness = density)) +
#   geom_slab()

