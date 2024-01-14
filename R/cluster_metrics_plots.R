# goal: make a plot or plots to show some cluster evaluation metrics. 
# Can include "elbow" (total WSS) plot, average silhouette, Calinski-Harabsasz,


# setup & load data -------------------------------------------------------

library(tidyverse)
library(glue)
library(fpc)
library(tidyclust)
library(ggthemes)

# our fitted models, created in chap 11
# maybe load these later / individually as we need them, and then rm once done?
load("data/fitted_kmeans_mods.RData")

load("data/fitted_pca_kmeans_mods.RData")

# the pre-processed (transformed, standardized) data
orig_dat <- read_csv("data/data_preprocessed_all_var.csv") %>% 
  select(-mukey)

pca_dat <- read_csv("data/pca_scores.csv") %>% 
  select(PC01, PC02, PC03, PC04, PC05)


# functions ---------------------------------------------------------------

# to extract and modify tidyclust clusters
# into a integer vector, which I will pass to calinhara()
create_clust_vec <- function(fit_obj){
  
  extract_cluster_assignment(fit_obj) %>% 
    pull(.cluster) %>% 
    str_replace(., "Cluster_", "") %>% 
    as.integer()
  
}

calc_clust_metrics <- function(obs_df, mods_df){
  
  # prep for calinhara()
  # wants an observations/variables matrix as first argument
  # (as opposed to a distance matrix)
  obsvar_mx <- as.matrix(obs_df)
  
  # prep for silhouette
  dists <- obsvar_mx %>% 
    dist(method = "euclidean")
  
  
  metrics_df <- mods_df  %>%
    mutate(
      # tot_sse = total sum of squared error
      tot_sse = map_dbl(km_fit, ~ sse_total_vec(.x)),
      # tot_wss = sum of within-cluster sse
      tot_wss = map_dbl(km_fit, ~sse_within_total_vec(.x)),
      # sse ratio = wss / total sse, 
      sse_ratio = map_dbl(km_fit, ~sse_ratio_vec(.x)),
      # int vector of clustering
      clustering_vec = map(km_fit, create_clust_vec),
      # ch_index = Calinski-Harabasz
      ch_index = map_dbl(clustering_vec,
                         ~ fpc::calinhara(x = obsvar_mx,
                                          clustering = .x)),
      avg_sil = map_dbl(km_fit, 
                        ~silhouette_avg_vec(object = .x, 
                                            dists = dists)))
  
  
  metrics_simple <- metrics_df %>% 
    select(n_clust, tot_sse, tot_wss, sse_ratio, ch_index, avg_sil)
  
  return(metrics_simple)
  
}




# calculate metrics -------------------------------------------------------


orig_metrics <- calc_clust_metrics(obs_df = orig_dat,
                                   mods_df = mods)

rm(mods)


pca_metrics <- calc_clust_metrics(obs_df = pca_dat,
                                  mods_df = pca_mods)

rm(pca_mods)

orig_v <- orig_metrics %>% 
  mutate(version = "original")

pca_v <- pca_metrics %>% 
  mutate(version = "pca")

metrics_all <- bind_rows(orig_v, pca_v)


# plot metrics ------------------------------------------------------------

met_long <- metrics_all %>% 
  pivot_longer(cols = -c(n_clust, version),
               names_to = "metric",
               values_to = "value") %>% 
  filter(metric %in% c("tot_wss", "ch_index", "avg_sil", "sse_ratio")) %>% 
  mutate(metric = case_when(
    metric == "tot_wss" ~ "Sum of Within-Cluster SSE",
    metric == "ch_index" ~ "Calinski-Harabasz Index", 
    metric == "avg_sil" ~ "Avg. Silhouette Coefficient",
    metric == "sse_ratio" ~ "Ratio WSS to TSS"
    
  ),
  version = case_when(
    version == "original" ~ "Full",
    version == "pca" ~ "Reduced"
  ))

wss_facet_plot <- met_long %>% 
  filter(metric == "Sum of Within-Cluster SSE") %>% 
  ggplot(aes(x = n_clust, y = value)) + 
  geom_point(aes(color = version)) + 
  geom_line(aes(color = version)) +
  facet_wrap(vars(version)) + 
  scale_x_continuous(breaks = seq(2, 20, 2)) +
  scale_color_manual(values = c("#1b9e77", "#d95f02")) +
  theme_bw() +
  xlab("Number of clusters") +
  ylab("Sum of within-cluster SSE") +
  theme(axis.text.x = element_text(size = 8))

sse_ratio_plot <- met_long %>% 
  filter(metric == "Ratio WSS to TSS") %>% 
  ggplot(aes(x = n_clust, y = value)) + 
  geom_point(aes(color = version)) + 
  geom_line(aes(color = version)) +
  scale_x_continuous(breaks = seq(2, 20, 2)) +
  scale_color_manual(values = c("#1b9e77", "#d95f02")) +
  theme_bw() +
  xlab("Number of clusters") +
  ylab("Ratio WSS to TSS") +
  theme(axis.text.x = element_text(size = 8))




met_ordered <- met_long %>%
  mutate(metric = fct_relevel(met_long$metric, c("Ratio WSS to TSS",
                                              "Calinski-Harabasz Index",
                                             "Avg. Silhouette Coefficient"
                                             ))) %>% 
  filter(metric != "Sum of Within-Cluster SSE")

df_label_pos <- met_ordered %>% 
  group_by(metric) %>% 
  slice_max(value, n = 1) %>% 
  mutate(lab_xpos = 1,
         lab_ypos = value*0.95,
         letter_lab = case_when(
           metric == "Ratio WSS to TSS" ~ "A.",
           metric == "Calinski-Harabasz Index" ~ "B.",
           metric == "Avg. Silhouette Coefficient" ~ "C."
         )) %>% 
  select(-version) %>% 
  filter(metric != "Sum of Within-Cluster SSE") %>% 
  ungroup()
  
  
all3_metrics_plot <- ggplot() +
  geom_line(data = met_ordered, 
            aes(x = n_clust, y = value, group = version, color = version)) + 
  geom_point(data = met_ordered,
             aes(x = n_clust, y = value, group = version, color = version)) + 
   geom_text(data = df_label_pos, aes(x = lab_xpos, y = lab_ypos, label = letter_lab)) +
  scale_x_continuous(breaks = seq(2, 20, 2), limits = c(1, NA)) +
  scale_color_manual(values = c("darkgrey", "black")) +
  facet_wrap(vars(metric), scales = "free", nrow = 1) +
  theme_bw() + 
  xlab("Number of clusters") +
  ylab("Value (all unitless)") +
  theme(panel.grid.major = element_line(color = "lightgrey"),
        legend.position = "bottom",
        axis.text = element_text(size = 8))


  
ggsave(filename = "figs/mod_metrics_facet_plot.png", 
       plot = all3_metrics_plot,
       height = 2.6, 
       width = 6,
       units = "in")  

met_ordered %>% 
  filter(metric != "Sum of Within-Cluster SSE",
         version != "Reduced") %>%
  ggplot(aes(x = n_clust, y = value)) +
  geom_line(aes(color = version)) + 
  geom_point(aes(color = version)) + 
  scale_x_continuous(breaks = seq(2, 20, 2)) +
  scale_color_manual(values = c("darkgrey", "black")) +
  facet_wrap(vars(metric), scales = "free", ncol = 1) +
  theme_tufte() +
  xlab("Number of clusters") +
  ylab("Value (all unitless)") +
  theme(panel.grid.major = element_line(color = "lightgrey"),
        legend.position = "bottom")
  

# testing the {fpc} cluster.stats() ---------------------------------------
# I like this function, it returns a list object with lots of metrics
  
  
  # test_clustering <- metrics_df$clustering_vec[[3]]
  # 
  # fpc_stats <- metrics_df %>% 
  #   select(n_clust, clustering_vec) %>% 
  #   mutate(fpc_stats = map(.x = clustering_vec,
  #                          .f = cluster.stats(clustering = .x),
  #                          d = dists,
  #                          silhouette = FALSE))
  # 
  # test_fpc <- cluster.stats(d = dists,
  #                           clustering = test_clustering,
  #                           silhouette = FALSE)

