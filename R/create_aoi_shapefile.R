# goal: clip target MLRAs using the Minnesota state boundary to create our 
# area of interest  


# setup -------------------------------------------------------------------


library(tidyverse)
library(sf)

# USA MLRAs
mlras <- st_read("data/mlra/mlra_v42.shp")

# State boundary
mn <- st_read("_qgis_files/shp_mn_state_boundary/mn_state_boundary.shp")

# confirm CRS the same
st_crs(mn) == st_crs(mlras)


# MLRAs to keep -----------------------------------------------------------


# identify all MLRAs that intersect w/ Minnesota
mn_mlras <- st_intersection(mn, mlras)

# check out the result
plot(mn_mlras["MLRARSYM"])

# keep only target MLRAs (agricultural regions of the state)
# clipped to MN boundary

keep_mlrarsym <- c("56",# Red River Valley of the North
                "102A", # Rolling Till Prairie
                "91A", # Central MN Sandy Outwash
                "57", # Northern Minnesota Gray Drift
                "103", # Central IA and MN Till Prairies
                "104", # Eastern IA and MN Till Prairies
                "105") # Northern Mississippi Valley Loess Hills 

mn_targets <- mn_mlras %>% 
  rownames_to_column(var = "rowid") %>% 
  filter(MLRARSYM %in% keep_mlrarsym,
         # drops the northern portion of N MN Gray Drift 
         # which was excluded b/c lack of validation pts 
         rowid != "1.4") 

mn_targets %>% ggplot() + 
  geom_sf(aes(fill = MLRARSYM))

# might want to make a map that shows the full extent of the MLRAs in MN, 
# extending out to neighborhing states. 
mn_mlras_extend <- mlras %>% 
  filter(MLRARSYM %in% keep_mlrarsym) %>% 
  # drops the northern portion of N MN Gray Drift 
  # which was excluded b/c lack of validation pts 
  slice(-2)

ggplot(data = mn_mlras_extend) +
  geom_sf(aes(fill = MLRARSYM)) +
  theme_bw()


# save results ------------------------------------------------------------

ifelse(dir.exists("_qgis_files/aoi_shapefiles_from_r"),
       print("Dir exists"),
       dir.create("_qgis_files/aoi_shapefiles_from_r"))

st_write(obj = mn_targets,
         dsn = "_qgis_files/aoi_shapefiles_from_r/mn_mlras_clipped.shp",
         driver = "ESRI Shapefile",
         # if dataset exists, append=FALSE overwrites it
         append = FALSE)

st_write(obj = mn_mlras_extend,
         dsn = "_qgis_files/aoi_shapefiles_from_r/mn_mlras_extended.shp",
         driver = "ESRI Shapefile",
         # if dataset exists, append=FALSE overwrites it
         append = FALSE)

