library(tidyverse)
library(sf)
library(patchwork)
library(glue)
#library(terra)


# load data ---------------------------------------------------------------



nw <- st_read("_qgis_files/aoi_nw_T158_R50W_Sec22/wss_aoi_2023-04-05_12-04-16/spatial/soilmu_a_aoi.shp")
se <- st_read("_qgis_files/aoi_se_T102_R9W_Sec6/wss_aoi_2023-04-05_12-12-26/spatial/soilmu_a_aoi.shp")
sw <- st_read("_qgis_files/aoi_sw_T115_R34W_Sec28/spatial/soilmu_a_aoi.shp")

mukey_clust <- read_csv("data/pca_mukey_cluster_assignments_and_soilprops.csv") %>% 
  select(mukey, k_8)


mn_border <- st_read("_qgis_files/shp_mn_state_boundary/mn_state_boundary.shp")

inset_mukeys <- c(unique(nw$MUKEY), unique(se$MUKEY), unique(sw$MUKEY))


# create color key dataframe ----------------------------------------------

target_mukeys <- mukey_clust %>% filter(mukey %in% inset_mukeys)

plot_colors <- c("#FF5A5F", "#FFB400", 
                 "#007A87", "#8CE071", "#7B0051", 
                 "#00D1C1", "#FFAA91", "#B4A76C")

cluster_ids <- glue("Cluster_{1:8}")

color_lookup <- data.frame(k_8 = cluster_ids, plot_color = plot_colors)

color_key <- left_join(target_mukeys, color_lookup, by = "k_8") %>% 
  mutate(mukey= as.character(mukey),
         k_8 = str_remove(k_8, "Cluster_")) %>% 
  rename(Cluster = k_8)


col_vec <- setNames(plot_colors, c(as.character(0:8)))



# join in cluster assignments ---------------------------------------------

nw_cols <- left_join(nw, color_key, by = c("MUKEY" = "mukey")) %>% 
  mutate(Cluster = factor(Cluster, levels = as.character(1:8)))
sw_cols <- left_join(sw, color_key, by = c("MUKEY" = "mukey")) %>% 
  mutate(Cluster = factor(Cluster, levels = as.character(1:8)))
se_cols <- left_join(se, color_key, by = c("MUKEY" = "mukey")) %>% 
  mutate(Cluster = ifelse(is.na(Cluster), "3", Cluster),
         Cluster = factor(Cluster, levels = as.character(1:8)))

# add colors to sf objs ----------------------------------------------------------


nw_clust_map <- ggplot() +
  geom_sf(
    data = nw_cols,
    aes(fill = Cluster),
    show.legend = FALSE,
    color = NA
  ) +
  # acrobatics to create a custom legend that includes
  # all the clusters and colors, inspired by 
  # https://aosmith.rbind.io/2020/07/09/ggplot2-override-aes/
  geom_point(x = 0, y = 0, aes(alpha = "1")) +
  geom_point(x = 0, y = 0, aes(alpha = "2")) +
  geom_point(x = 0, y = 0, aes(alpha = "3")) +
  geom_point(x = 0, y = 0, aes(alpha = "4")) +
  geom_point(x = 0, y = 0, aes(alpha = "5")) +
  geom_point(x = 0, y = 0, aes(alpha = "6")) +
  geom_point(x = 0, y = 0, aes(alpha = "7")) +
  geom_point(x = 0, y = 0, aes(alpha = "8")) +
  scale_fill_manual(values = col_vec,
                    drop = FALSE, ) +
  scale_alpha_manual(values = rep(1, 8),
                     name = "Cluster",
                     guide = guide_legend(override.aes = list(
                       fill = plot_colors,
                       shape = c(22),
                       size = 8
                     ))) +
  theme_void() +
  # top, right, bottom, left
  theme(plot.margin = unit(c(5, 0, 0, 0), "pt"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11)) +
  labs(subtitle = "B2")


se_clust_map <- ggplot() +
  geom_sf(data = se_cols, aes(fill = Cluster), color = NA, show.legend = FALSE) +
  scale_fill_manual(values = col_vec, drop = FALSE) +
  theme_void() +
  # top, right, bottom, left
  theme(plot.margin = unit(c(5, 0, 0, 0), "pt")) +
  labs(subtitle = "D2")

sw_clust_map <- ggplot() +
  geom_sf(data = sw_cols, aes(fill = Cluster), color = NA, show.legend = FALSE) +
  scale_fill_manual(values = col_vec, drop = FALSE) +
  theme_void() +
  # top, right, bottom, left
  theme(plot.margin = unit(c(5, 0, 0, 0), "pt")) +
  labs(subtitle = "C2")

nw_line_map <- ggplot() +
  geom_sf(data = nw_cols, color = "black", lwd = 0.3, show.legend = FALSE, fill = "white") +
  theme_void() +
  # top, right, bottom, left
  theme(plot.margin = unit(c(0, 0, 5, 0), "pt")) +
  labs(subtitle = "B1")

se_line_map <- ggplot() +
  geom_sf(data = se_cols, color = "black", lwd = 0.3, show.legend = FALSE, fill = "white") +
  theme_void() +
  # top, right, bottom, left
  theme(plot.margin = unit(c(0, 0, 5, 0), "pt")) +
  labs(subtitle = "D1")

sw_line_map <- ggplot() +
  geom_sf(data = sw_cols, color = "black", lwd = 0.3, show.legend = FALSE, fill = "white") +
  theme_void() +
  # top, right, bottom, left
  theme(plot.margin = unit(c(0, 0, 5, 0), "pt")) +
  labs(subtitle = "C1")


mn_border_map <- ggplot() +
  geom_sf(data = mn_border, fill = "white") + 
  annotate(geom = "text", y = 48, x = -96.1, label = "B", size = 5) +
    annotate(geom = "text", y = 44.8, x = -95.2, label = "C", size = 5) +
    annotate(geom = "text", y = 44, x = -92.5, label = "D", size = 5) +
  annotate(geom = "text", y = 50.15, x = -97, label = "A", size = 5) +
  theme_bw() +
  theme(axis.text = element_text(size = 10)) +
    xlab("") +
    ylab("") +
    coord_sf(ylim = c(43, 49.5), clip = "off") +
    ggtitle("")

# compose multipanel plot -------------------------------------------------

# mn_border_map + ((nw_line_map + sw_line_map + se_line_map)  /
#   ( nw_clust_map + sw_clust_map + se_clust_map ) ) +
#   plot_layout(guides = "collect")


(mn_border_map | ( (nw_line_map + sw_line_map + se_line_map)  /
                   ( nw_clust_map + sw_clust_map + se_clust_map ) ) ) +
  plot_layout(guides = "collect", widths = c(2, 4))


ggsave(
  filename = "figs/dpi300/fig6_inset_map.tiff",
  dpi = 300,
  width = 6.5,
  height = 4,
  units = "in"
)

