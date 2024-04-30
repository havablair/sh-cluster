# this raster of climate clusters was created in the script 
# 'cluster_reclass_rast_clim_only.R' and represents the results of 
# the k-means clustering I did using the MAT and MAP normals I extracted
# for each map unit that is included in this analysis. That process is 
# documented in 35-climate-clusters.qmd.
#
#


# setup -------------------------------------------------------------------



library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(glue)
library(terra)
library(sf)
library(ggplot2)
library(gridExtra)
library(gt)
library(cowplot)
library(gridGraphics) # required for cowplot to handle base R plots
library(magick)


# CRS is NAD 1983
clust <- rast("D:/big-files-backup/ch03-sh-groups/climate_clusters3_try2.tif")
mn <- terra::vect("_qgis_files/shp_mn_state_boundary/mn_state_boundary.shp")

mlras_clipped <- terra::vect("_qgis_files/aoi_shapefiles_from_r/mn_mlras_clipped.shp") 

cig_dat <- read_csv("data/cig_lab_data_all_20230301.csv")
clim_vals <- read_csv("data/mukey_cluster_assignments_and_props_climate_only.csv")

mn_proj <- terra::project(mn, "EPSG:4326")

# calculate MAP, MAT stats for the climate zones --------------------------

map_mat_means <- clim_vals %>% 
  group_by(k_3) %>% 
  summarise(
    mean_map = mean(annprcp_norm),
    min_map = min(annprcp_norm),
    max_map = max(annprcp_norm),
    sd_map = sd(annprcp_norm),
    mean_mat = mean(anntavg_norm),
    min_mat = min(anntavg_norm),
    max_mat = max(anntavg_norm),
    sd_mat = sd(anntavg_norm)
  ) %>% 
  mutate(k_3 = 
           case_when(
             k_3 == "Cluster_1" ~ "Zone 1",
             k_3 == "Cluster_2" ~ "Zone 2",
             k_3 == "Cluster_3" ~ "Zone 3"
           ))

map_mat_means



# projections -------------------------------------------------------------


mn_proj <- terra::project(mn, clust)
mlras_proj <- terra::project(mlras_clipped, clust)


# https://support.esri.com/en-us/knowledge-base/how-to-determine-which-nad1983towgs1984-transformation-000005929
# https://epsg.io/1702
mn_wgs <- terra::project(mn, "EPSG:4326")
# 4269 = NAD83 North America

# this takes a while to run, I ran it on 2024-03-24 and am now just going to
# read that version back in
# clim_wgs <- terra::project(x = clust,
#                            y = "EPSG:4326",
#                            method = "near",
#                            filename = "D:/big-files-backup/ch03-sh-groups/climate_clusters3_wgs84_try2.tif",
#                            overwrite = TRUE)

clim_wgs <- terra::rast("D:/big-files-backup/ch03-sh-groups/climate_clusters3_wgs84_try2.tif")



# validation pts ----------------------------------------------------------

cig_pts <- cig_dat %>% 
  select(lon, lat, sample_id) %>% 
  mutate(sample_id = as.character(sample_id))


pt_matrix <- cig_pts %>% 
  select(lon, lat) %>% 
  as.matrix()

# terra::extract will return "ID", which is a record number (the row number)
# I will use this df as a key to get the sample_ids back
pt_ids <- data.frame(sample_id = cig_pts$sample_id,
                      row_id = 1:nrow(cig_pts))


# reproject points to NAD 1983 -------------------------------------

# turn validation point dataframe into a SpatVector
pt_spat <- vect(x = pt_matrix,
                 type = "points",
                 # EPSG:4326 is WGS84 long/lat
                 crs = "epsg:4326",
                 atts = pt_ids
)


# reproject the CIG and NCSS points to NAD 83 (EPSG 5070)
pt_reproj <- project(x = pt_spat,
                      y = "epsg:5070")


# maps --------------------------------------------------------------------



# make dataframe of colors 

clim_colors <- data.frame(values = c(0:3)) %>% 
  mutate(cols = case_when(
    values == 1 ~ '#a6cee3',
    values == 2 ~'#1f78b4',
    values == 3 ~ '#b2df8a',
    TRUE ~ "#FFFFFF"
  ))

# plot 1: climate zones + MLRA boundaries + validation points

png(file = "figs/climate_zones_3.png",
    width = 5,
    height = 8,
    units = "in",
    res = 102)


plot(x = clust,
     col = clim_colors,
    # legend = FALSE,
     main = "Climate zones",
    # cex.main = cex_val, 
     pax = list(labels = TRUE, tick = FALSE))

lines(mlras_proj, col = "black")

points(pt_reproj, col = "red",
       cex = 1)


dev.off()


# plot 2: only climate zones

# Note that this map plus the GT table I make below are combined into a single figure
# using powerpoint (see "additional_analysis_variation_explained update.pptx")

png(file = "figs/climate_zones_labelled.png",
    width = 5,
    height = 5,
    units = "in",
    res = 102)

plot(mn,
     box = FALSE,
     pax = list(side = c(1:2)))

plot(
  x = clim_wgs,
  col = clim_colors,
  type = "classes",
  legend = TRUE,
  plg = list(
    title = "Climate zone"
  ),
  main = "Climate zones",
  axes = FALSE,
  box = FALSE,
  add = TRUE
)


# text(x = clim_wgs, )

dev.off()



map_mat_tbl <- map_mat_means %>% 
  mutate(across(.cols = contains("map"), ~round(.x, digits = 0)),
         across(.cols = contains("mat"), ~round(.x, digits = 1))) %>% 
  select(k_3, mean_map, sd_map, mean_mat, sd_mat) %>% 
  gt() %>% 
  cols_merge_uncert(
    col_val = mean_map,
    col_uncert = sd_map
  ) %>% 
  cols_merge_uncert(
    col_val = mean_mat,
    col_uncert = sd_mat
  ) %>% 
  cols_label(
    k_3 = "Climate Zone",
    mean_map = "MAP (mm)",
    mean_mat = "MAT (Celsius)"
  ) %>% 
  tab_options(
    table.font.size = pct(130)
  )

  
gtsave(map_mat_tbl, filename = "figs/climzone_mat_map_tbl.png")



# combine map and table with gridExtra ------------------------------------


# function to produce my climate zone map 

make_clim_map <- function(){
  
  plot(mn,
       box = FALSE,
       pax = list(side = c(1:2)),
       cex.axis = 4)
  
  plot(
    x = clim_wgs,
    col = clim_colors,
    type = "classes",
    legend = TRUE,
    plg = list(
      title = "Climate zone",
      cex = 1.2
    ),
    main = "Climate zones",
    axes = FALSE,
    box = FALSE,
    add = TRUE
  )
  
}




tbl_image <- "figs/climzone_mat_map_tbl.png"

# produce multi-panel plot and save

ggdraw() +
  draw_image(
    tbl_image,
    x = 1, width = 0.35,
    hjust = 1.1, halign = 0.8, valign = 0.43
  ) +
  draw_plot(make_clim_map) +
  draw_plot_label(
    c("A", "B"),
    c(0.24,0.595),
    c(0.95, 0.59), 
    size = 12
  )

cowplot::ggsave2(filename = "figs/dpi300/clim_map.tiff",
                 dpi = 300, 
                 width = 6.5,
                 height = 5, 
                 units = "in")

# map using centroid pts --------------------------------------------------

c <- read_csv("data/mukey_cluster_assignments_and_props_climate_only.csv")

clust_long <- c %>% 
  select(-mukey) %>% 
  pivot_longer(cols = contains("k_"),
               names_to = "model_ver",
               values_to = "cluster_assignment") %>% 
  filter(model_ver == "k_3")

climsf <- st_as_sf(clust_long, coords = c("x","y"))

climsf <- st_set_crs(climsf, 4326) 

mn_sf <- st_as_sf(mn_wgs)


ggplot() +
  geom_sf(data = mn_sf) +
  geom_sf(data = climsf, aes(color = cluster_assignment)) +
  ggtitle("Map unit centroids color coded by climate cluster")


# ph map using centroid points --------------------------------------------

d <- read_csv("data/pca_mukey_cluster_assignments_and_soilprops.csv")


centroids <- c %>% select(mukey, x, y, annprcp_norm)

d_id <- left_join(d, centroids, by = "mukey") %>% 
  select(-c(k_2:k_7, k_9:k_20))

propsf <- st_as_sf(d_id, coords = c("x", "y"))
propsf <- st_set_crs(propsf, 4326)

ggplot() +
  geom_sf(data = mn_sf) +
  geom_sf(data = propsf, aes(color = ph1to1h2o)) +
  scale_color_viridis_c()
