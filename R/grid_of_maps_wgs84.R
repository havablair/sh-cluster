# goal: make a figure with a grid of AOI maps, each cluster mapped 
# individually in the AOI to illustrate distributions across AOI 
# similar to grid_of_maps.R, but on 2023-09-11 finally figured out how to 
# project my pca8 raster from NAD 1983 Albers to WGS 84 because I want my 
# plot labels to be in more recognizable lat/long coordinates and not
# the NAD 83 ones.
#
# Basically what I learned is that when transforming a raster dataset with 
# terra::project, you really need to use a template raster that is in the
# destination CRS rather than simply using an EPSG code like "EPSG:4326". 
# the latter approach works fine for vector transformations, but 
# I haven't been able to get it to work for raster. The terra docs state that
# using a template raster is the "preferred" approach for raster transformations,
# but I've found it to be the ONLY approach that works.


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

mn <- terra::vect("_qgis_files/shp_mn_state_boundary/mn_state_boundary.shp")


# trying to create a template raster to use with terra::project()
# code modified from https://rdrr.io/cran/ebvcube/src/R/ebv_resample.R

#--create dummy terra SpatRast for projection
dummy <- terra::rast()
terra::crs(dummy) <- paste0('EPSG:', "4326") #epsg_desg

#--set extent
#---"if epsg differ, transform src_ext
#---else just assign src extent"
dummy2 <- terra::rast()
terra::crs(dummy2) <- paste0('EPSG:', "5070") # epsg_source = NAD 1983 
terra::ext(dummy2) <-  ext(pca8)
dummy_proj <- terra::project(dummy2, "EPSG:4326") # epsg_dest = 4326
extent_src <-terra::ext(dummy_proj)

terra::ext(dummy) <- extent_src

#--set resolution, might need to change this after looking at plots
terra::res(dummy) <- c(0.02, 0.02) # res destination

# now we can project the cluster raster and MN boundary (vector)
# note this only works with gdal = FALSE, which tells the function to 
# use an internal algorithm rather than GDAL-warp algorithm, which I couldn't
# get to work.
pca8_proj <- terra::project(pca8, y = dummy, align = T, method = "near", gdal=FALSE)
mn_proj <- terra::project(mn, "EPSG:4326")

crs(pca8_proj) == crs(mn_proj)

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
         cex.main = cex_val)
    
    # text(x = lab_xpos,
    #      y = 2500000,
    #      labels = clust_desc,
    #      cex_val)
    
  }



# maps --------------------------------------------------------------------


# * pca k_8 ---------------------------------------------------------------


png(file = "figs/pca_mapgrid_k8_latlon.png",
    width = 8,
    height = 8,
    units = "in",
    res = 102)

par(mfrow = c(3,3))

plot_cluster_hlight(nclust = 8,
                    target_clust = 1,
                    sp_rast = pca8_proj,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 2,
                    sp_rast = pca8_proj,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 3,
                    sp_rast = pca8_proj,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 4,
                    sp_rast = pca8_proj,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 5,
                    sp_rast = pca8_proj,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 6,
                    sp_rast = pca8_proj,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 7,
                    sp_rast = pca8_proj,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

plot_cluster_hlight(nclust = 8,
                    target_clust = 8,
                    sp_rast = pca8_proj,
                    cex_val = 1.2)

lines(mn_proj, col = "black")

dev.off()