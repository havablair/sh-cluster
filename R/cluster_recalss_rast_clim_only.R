
# about -------------------------------------------------------------------

# this is a helper script that takes information about MUKEYs and their
# assigned CLIMATE cluster and creates a reclassed raster file (.tif) that allows us 

# setup -------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(glue)
library(purrr)
library(terra)

# gssurgo raster, clipped to AOI and simple MUKEYs to reduce size
r <- rast("../MapunitRaster_10m_Clip1_and_Reclass/Reclass_tif1.tif")

# allows us to translate our "new" (shorter) MUKEYs to the
# originals that match up with the rest of the db
aoi_mu <- read.delim("data/gSSURGO_MN/mukey_new_crosswalk.txt", sep = ",") %>% 
  select(MUKEY, MUKEY_New, Count)



# cluster assignments data ---------------------------------------------------
# note this only has n=6872 rows (less than aoi_mu above) because some 
# map units were excluded based on data availability (see 07-map-unit-agg.qmd) 
# or because they are not relevant to the analysis (landfill, dump, sand pit...
# see 10-additional-data-prep.qmd)
c <- read_csv("data/mukey_cluster_assignments_and_props_climate_only.csv") 

clim_only <- left_join(c, aoi_mu, by = c("mukey_short" = "MUKEY_New")) %>% 
  select(-c(Count)) %>% 
  select(mukey, everything())

# function ----------------------------------------------------------------

# k_col expected format is k_n (unquoted), where n is the number of clusters
create_cluster_raster <- function(start_rast,
                                  clust_assign_df,
                                  k_col, 
                                  outfile_basename){
  
  clustkey <- clust_assign_df %>%
    select(mukey, mukey_short, {{k_col}}) %>% 
    rename(clustid = {{k_col}}) %>%
    mutate(clustid = as.numeric(str_replace(clustid, "Cluster_", ""))) %>%
    # the full join below generates NAs in the clustid column for every 
    # MUKEY that was excluded from our analysis. This is 
    # because aoi_mu contains the full set of MUKEYs but 
    # the clust_assign_df contains only MUKEYs which were
    # included in the clustering. This is the desired behavior.
    full_join(., aoi_mu, by = c("mukey" = "MUKEY")) %>%
    mutate(clustid = case_when(
      is.na(clustid) ~ 0, # 0 means excluded during our process
      TRUE ~ clustid
    )) %>%
    select(is = MUKEY_New,
           becomes = clustid)

  clustkey_mx <- clustkey %>%
    as.matrix()
  
  savepath <- glue("D:/big-files-backup/ch03-sh-groups/{outfile_basename}.tif")

  r_reclass <- terra::classify(x = start_rast,
                        rcl = clustkey_mx,
                        filename = savepath,
                        datatype = "INT2U",
                        overwrite = TRUE)
  
}

#4:37
create_cluster_raster(
  start_rast = r,
  clust_assign_df = clim_only,
  k_col = k_3,
  outfile_basename = "climate_clusters3_try2"
)

create_cluster_raster(
  start_rast = r,
  clust_assign_df = clim_only,
  k_col = k_4,
  outfile_basename = "climate_clusters4"
)





# data checks -------------------------------------------------------------


# 19 tifs, for 2-20 clusters
my_tifs <- list.files("E://big-files-backup/ch03-sh-groups/", pattern = "_allvar.tif",
           full.names = TRUE)

# what is the max number of categories I should have in each tif? 
n_clust <- basename(my_tifs) %>% str_extract(pattern = "[:digit:]{1,}")

# function to return nice df of expected categories (nclust) and 
# the min (should always be 0, for "not included") and max (should 
# always be equal to nclust)
check_minmax <- function(tif_path, nclust){
  
  temp_rast <- rast(tif_path)
  
  rast_minmax <- minmax(temp_rast)
  
  info <- data.frame(ncat = nclust,
                     min_cat = rast_minmax['min',],
                     max_cat = rast_minmax['max',])
  
  return(info)
  
  
}

# all OK? 
map2_dfr(my_tifs, n_clust, check_minmax) %>% 
  mutate(category_match = case_when(
    ncat == max_cat ~ "OK",
    TRUE ~ "Wrong number, re-write raster"
  ))

# double check that CRS information is included with .tif files? 

k9 <- rast("E://big-files-backup/ch03-sh-groups/clust9_allvar.tif")

terra::crs(k9, describe = TRUE)

terra::crs(r, describe = TRUE)

