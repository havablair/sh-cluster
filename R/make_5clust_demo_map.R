# about -------------------------------------------------------------------

# date: 2022-09-29
# purpose: create a small map to use as a feature graphic as part of my 
# fellowship final report to the UMN Informatics Institute / MnDRIVE 
# this will be used in a short feature article about what I accomplished during
# the fellowship period. I am going to use my recent 5 cluster raster for this
# example map 


# setup -------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(glue)
library(terra)

r <- rast("E:/big-files-backup/ch03-sh-groups/clust5_noec.tif")


plot_colors <- c("#636363", "#FF5A5F", "#FFB400", 
                 "#007A87", "#8CE071", "#7B0051")
# "#00D1C1", "#FFAA91", "#B4A76C", 
# "#9CA299", "#565A5C", "#00A04B", 
# "#E54C20")

# pull colors for the different clusters
mycols <- data.frame(values = c(1:6), cols = plot_colors)

# cols = hcl.colors(6, palette = "viridis")


# assign the colors to my color table for raster "r"
coltab(r) <- mycols


plot(r,
     col = mycols,
     plg=list(legend = c('k=0', 'k=1', 'k=2', 'k=3', 'k=4', 'k=5')),
     main = "Soil Health Regions")






