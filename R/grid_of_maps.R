# goal: make a figure with a grid of AOI maps, each cluster mapped 
# individually in the AOI to illustrate distributions across AOI 


# setup -------------------------------------------------------------------


library(readr)
library(dplyr)
library(glue)
library(stringr)
library(terra)

cluster_names <- read_csv("data/named_clustering_candidates.csv") %>% 
  filter(version == "pca") %>% 
  mutate(desc_w_count = glue("n={n_members}\n{description}"),
         wrap_names = str_wrap(desc_w_count, 15))

# CRS is NAD 1983
pca8 <- rast("E:/big-files-backup/ch03-sh-groups/clust8_pca.tif")
# pca6 <- rast("E:/big-files-backup/ch03-sh-groups/clust6_pca.tif")
# pca4 <- rast("E:/big-files-backup/ch03-sh-groups/clust4_pca.tif")


mn <- terra::vect("_qgis_files/shp_mn_state_boundary/mn_state_boundary.shp")

mlras_clipped <- terra::vect("_qgis_files/aoi_shapefiles_from_r/mn_mlras_clipped.shp") 

mn_proj <- terra::project(mn, pca8)
mlras_proj <- terra::project(mlras_clipped, pca8)


# https://support.esri.com/en-us/knowledge-base/how-to-determine-which-nad1983towgs1984-transformation-000005929
# https://epsg.io/1702
mn_wgs <- terra::project(mn, "EPSG:4326")
# 4269 = NAD83 North America
pca8_wgs <- terra::project(x = pca8,
                           y = "EPSG:4326", 
                           method = "near",
                           filename = "E:/big-files-backup/ch03-sh-groups/clust8_pca_wgs84.tif")

crs(pca8) == crs(mn_proj)
#crs(pca6) == crs(mn_proj)


# functions ---------------------------------------------------------------


# function to create dataframe passed to "plot" 
# for raster maps that highlight one specific 
# raster value at a time
create_highlight_df <- function(nclust, target_clust){
  
  data.frame(values = c(0:nclust)) %>% 
    mutate(cols = case_when(
      values == target_clust ~ "#FDE725",
      TRUE ~ "#440154"
    ))
  
}

# function to create one map, with target cluster highlighted
plot_cluster_hlight <-
  function(nclust,
           target_clust,
           desc_df = cluster_names,
           sp_rast,
           cex_val=1.5,
           lab_xpos = 6e5) {
    
    hlight_colors <- create_highlight_df(nclust, target_clust)
    
    clust_id <- paste("Cluster_", target_clust, sep = "")
    
    clust_desc <- cluster_names %>%
      filter(k == nclust,
             join_cluster == clust_id) %>%
      pull(wrap_names)
    
    plot(sp_rast,
         col = hlight_colors,
         legend = FALSE,
         main = glue("Cluster {target_clust}"),
         cex.main = cex_val, 
         pax = list(labels = TRUE, tick = FALSE))
    
    # text(x = lab_xpos,
    #      y = 2500000,
    #      labels = clust_desc,
    #      cex_val)
    
  }



# maps --------------------------------------------------------------------


# * pca k_8 ---------------------------------------------------------------


png(file = "figs/pca_mapgrid_k8.png",
    width = 8,
    height = 8,
    units = "in",
    res = 102)

par(mfrow = c(3,3))

plot_cluster_hlight(nclust = 8,
                    target_clust = 1,
                    sp_rast = pca8,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 2,
                    sp_rast = pca8,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 3,
                    sp_rast = pca8,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 4,
                    sp_rast = pca8,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 5,
                    sp_rast = pca8,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 6,
                    sp_rast = pca8,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 7,
                    sp_rast = pca8,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 8,
                    sp_rast = pca8,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

dev.off()


# * pca k_6 ---------------------------------------------------------------

png(file = "figs/pca_mapgrid_k6.png",
    width = 8,
    height = 11,
    units = "in", 
    res = 102)

par(mfrow = c(3,2))

plot_cluster_hlight(nclust = 6,
                    target_clust = 1,
                    sp_rast = pca6,
                    cex_val = 1.2,
                    lab_xpos = 4.5e5)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 6,
                    target_clust = 2,
                    sp_rast = pca6,
                    cex_val = 1.2,
                    lab_xpos = 4.5e5)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 6,
                    target_clust = 3,
                    sp_rast = pca6,
                    cex_val = 1.2,
                    lab_xpos = 4.5e5)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 6,
                    target_clust = 4,
                    sp_rast = pca6,
                    cex_val = 1.2,
                    lab_xpos = 4.5e5)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 6,
                    target_clust = 5,
                    sp_rast = pca6,
                    cex_val = 1.2,
                    lab_xpos = 4.5e5)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 6,
                    target_clust = 6,
                    sp_rast = pca6,
                    cex_val = 1.2,
                    lab_xpos = 4.5e5)

lines(mn_proj, col = "black")

dev.off()


# * pca k_4 ---------------------------------------------------------------

png(file = "figs/pca_mapgrid_k4.png",
    width = 10,
    height = 8,
    units = "in", 
    res = 102)

par(mfrow = c(2,2))

plot_cluster_hlight(nclust = 4,
                    target_clust = 1,
                    sp_rast = pca4,
                    cex_val = 1.2,
                    lab_xpos = 5e5)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 4,
                    target_clust = 2,
                    sp_rast = pca4,
                    cex_val = 1.2,
                    lab_xpos = 5e5)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 4,
                    target_clust = 3,
                    sp_rast = pca4,
                    cex_val = 1.2,
                    lab_xpos = 5e5)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 4,
                    target_clust = 4,
                    sp_rast = pca4,
                    cex_val = 1.2,
                    lab_xpos = 5e5)

lines(mn_proj, col = "black")

dev.off()

