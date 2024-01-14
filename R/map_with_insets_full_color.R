# goal: make a colorful map showing all clusters together, add small 
# insets in the NW, SW, and SE to illustrate how the cluster results 
# look / can be used at the field (management) scale

library(readr)
library(dplyr)
library(stringr)
library(glue)
library(terra)

r <- rast("E:/big-files-backup/ch03-sh-groups/clust8_pca.tif")

mn <- terra::vect("_qgis_files/shp_mn_state_boundary/mn_state_boundary.shp")

mn_proj <- terra::project(mn, r)


plot_colors <- c("lightgrey", "#FF5A5F", "#FFB400", 
                          "#007A87", "#8CE071", "#7B0051", "#00D1C1", "#FFAA91", "#B4A76C")

# "#9CA299", "#565A5C", "#00A04B", 
# "#E54C20")

# pull colors for the different clusters
mycols <- data.frame(values = c(0:8), cols = plot_colors)

# cols = hcl.colors(6, palette = "viridis")


# assign the colors to my color table for raster "r"
coltab(r) <- mycols

# make spatvector from matrix of inset points 
se <- c(323660, 2301542)
sw <- c(82574, 2416189)
rr <- c(-82636, 2832148)

insets <- rbind(se, sw, rr)

insets_vec <- vect(insets, crs = "epsg:5070")

png(file = "figs/map_full_color.png",
    width = 10,
    height = 8,
    units = "in", 
    res = 102)

plot(r,
     col = mycols,
     plg=list(legend = c('0', '1', '2', '3', '4', '5', '6', '7', '8')),
     main = "Soil Health Groups")

lines(mn_proj, col = "black")

points(insets_vec, cex = 1.5)

dev.off()


# this is really not working for me. It runs and runs, taking up 2.5+GB memory, 
# which seems like so much for what basically amounts to making grid lines
# that are projected to the right CRS for my MN map?? 

# mn_grat <- graticule(lon = c(43, 44, 45, 46),
#                      lat = c(-90,-91,-92, -93, -94, -96),
#                      crs = "epsg:5070")

plot(mn_proj)
points(insets_vec, cex = 1.5)
lines(mn_grat)
