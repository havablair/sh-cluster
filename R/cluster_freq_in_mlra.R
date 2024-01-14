# goal: extract raster values (sh groups/classes) in each MLRA/agroecoregion, get 
# pixel counts (to determine total area) and calculate % per cluster for 
# each agroecoregion so I can make a cross-tabulation of clusters vs. agroecoregions


library(tidyverse)
library(glue)
library(sf)
library(terra)
library(gt)

aer <- st_read("_qgis_files/shp_agri_agroecoregions/agroecoregions.shp")
aoi <- st_read("_qgis_files/aoi_shapefiles_from_r/mn_mlras_clipped.shp")
# get the agro-ecoregions in the same CRS as my AOI
aer_reproj <- st_transform(aer, crs = st_crs(aoi))

k8 <- terra::rast("E:/big-files-backup/ch03-sh-groups/clust8_pca.tif")
mlra <- terra::vect("_qgis_files/aoi_shapefiles_from_r/mn_mlras_clipped.shp")
mlra_reproj <- terra::project(mlra, k8)




# calculate group frequency in MLRAs --------------------------------------

# using code example from this link: 
#https://gis.stackexchange.com/questions/451815/how-to-extract-the-frequency-of-raster-values-from-polygons-in-terra
mlra_rast <- rasterize(mlra_reproj, k8, field = "MLRA_ID")

# runs for 40-50 minutes
mlra_xtab <- crosstab(c(k8, mlra_rast)) %>% as.data.frame()

# it's easier to pull the MLRA names and IDs from the sf object 
# instead of the terra object (didn't spend much time looking for alternatives)
mlra_ids_names <- aoi %>% 
  # don't need geometry b/c creating simple crosswalk of MLRA IDs & names
  st_drop_geometry() %>% 
  select(MLRA_ID, MLRA_NAME)%>% 
  mutate(MLRA_ID = as.factor(MLRA_ID)) %>% 
  distinct()

mlra_grp_cts <- left_join(mlra_xtab, mlra_ids_names, by = "MLRA_ID") 

write_csv(mlra_grp_cts, "data/freqs_k8_clusters_in_mlras.csv")


clust_colors <- c("1" = "#FF5A5F", "2" = "#FFB400", "3" = "#007A87" , "4" = "#8CE071",
                  "5" = "#7B0051", "6" = "#00D1C1", "7" = "#FFAA91", "8" = "#B4A76C")

mlra_bars <- mlra_grp_cts %>% 
  filter(Reclass_tif1 != 0) %>% 
  group_by(MLRA_ID) %>% 
  mutate(total_cells = sum(Freq),
         perc_mlra = round((Freq/total_cells)*100, digits = 0),
         MLRA_NAME = str_replace(MLRA_NAME, "Minnesota", "MN"),
         MLRA_NAME = str_replace(MLRA_NAME, "Iowa", "IA"),
         MLRA_NAME = str_replace(MLRA_NAME, "Northern", "N.")) %>% 
  ggplot() +
  geom_col(aes(x = Reclass_tif1, y = perc_mlra, fill = Reclass_tif1),
           show.legend = FALSE) + 
  geom_text(aes(x = Reclass_tif1, y = perc_mlra+9, label = glue("{perc_mlra}%"))) +
  facet_wrap(vars(MLRA_NAME), scales = "free_y", nrow = 3) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
    scale_fill_manual(values = clust_colors) +
  theme_bw() +
  coord_flip() +
    xlab("Group") +
    ylab("Percent area of MLRA")
  
ggsave("figs/barplot_group_freqs_in_mlra.png", height = 5, width = 7, units = "in")  
  
grp_desc <- data.frame(Group = as.character(c(1:8)), 
                       empty = "  ") %>% 
  mutate( Description = case_when(
             Group == "1" ~ "Coarsest, slightly acid, lowest OM",
             Group == "2" ~ "Loamy, neutral, moderate OM",
             Group == "3" ~ "Coarse loamy, low-moderate OM, neutral",
             Group == "4" ~ "Loamy to clayey, slightly alkaline, highest OM, 11% CaCO3",
             Group == "5" ~ "Highest clay, neutral, high OM",
             Group == "6" ~ "Loamy, slightly alkaline, moderate OM, 9% CaCO3, EC 1-2 dS/m",
             Group == "7" ~ "Loamy, slightly alkaline, moderate OM, 8% CaCO3",
             Group == "8" ~ "Histosols & intergrades (organic soils)") ) 

clust_colors <- c("1" = "#FF5A5F", "2" = "#FFB400", "3" = "#007A87" , "4" = "#8CE071",
                  "5" = "#7B0051", "6" = "#00D1C1", "7" = "#FFAA91", "8" = "#B4A76C")

legend_tbl_gt <- grp_desc %>% 
  gt() %>% 
  tab_style(
    style = cell_fill(color = "#FF5A5F"),
    locations = cells_body(
      columns = empty,
      rows = Group == "1"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#FFB400"),
    locations = cells_body(
      columns = empty,
      rows = Group == "2"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#007A87"),
    locations = cells_body(
      columns = empty,
      rows = Group == "3"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#8CE071"),
    locations = cells_body(
      columns = empty,
      rows = Group == "4"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#7B0051"),
    locations = cells_body(
      columns = empty,
      rows = Group == "5"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#00D1C1"),
    locations = cells_body(
      columns = empty,
      rows = Group == "6"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#FFAA91"),
    locations = cells_body(
      columns = empty,
      rows = Group == "7"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#B4A76C"),
    locations = cells_body(
      columns = empty,
      rows = Group == "8"
    )
  ) %>% 
  cols_label(
    empty = "____"
  ) %>% 
  tab_options(table.font.size = pct(75),
              data_row.padding = px(3))
  
gtsave(legend_tbl_gt, filename = "figs/legend_tbl_color.png", vwidth = 450, vheight = 250)  


# stuff I tried / info ----------------------------------------------------


# want to mask and then trim? and then freq?

# not sure this is the way to go, was taking forever to run (and lots of memory)
# some searching online found this: https://cran.r-project.org/web/packages/exactextractr/vignettes/vig2_categorical.html
# that vignette shows exactly what I'm trying to do
# spdl_extr <- terra::extract(x = k8, y = spdl,
#                      method = "simple", 
#                      xy = TRUE, 
#                      ID = TRUE)


# or can I just use terra::freq on appropriate clips/masked rasters for each MLRA/agroecoregion?
# I used terra::freq in "calculate_mukey_spatial_weights.R" with a note that it took 
# several minutes to run on the entire AOI raster. 



# create agro-ecoregion shp clipped to AOI --------------------------------
# only needed to run this once, commenting out b/c 
# we can just load the results, which I saved as "agroecoregions_in_aoi.shp"

# 
# # unite the MLRAs in my AOI into one continuous polygon
# aoi_union <- st_union(aoi)
# 
# ggplot(aoi_union) +
#   geom_sf()
# 
# # keep the intersection of my AOI and the agroecoregions
# aoi_aer <- st_intersection(aer_reproj, aoi_union)
# 
# # save the agroecoregions in my AOI as a shapefile 
# st_write(obj = aoi_aer,
#          dsn = "_qgis_files/shp_agroeco_in_aoi/agroecoregions_in_aoi.shp",
#          driver = "ESRI Shapefile",
#          # if dataset exists, append=FALSE overwrites it
#          append = FALSE)

