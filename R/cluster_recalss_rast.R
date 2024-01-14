
# about -------------------------------------------------------------------

# this is a helper script that takes information about MUKEYs and their
# assigned cluster and creates a reclassed raster file (.tif) that allows us 
# to explore spatial patterns in the clusters. 
# the MUKEY/cluster assignments are generated in chaps 11, 13, 15 with kmeans


# done (all var tifs) ---------------------------------------------------------

# 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15-20

# setup -------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(glue)
library(purrr)
library(terra)

# gssurgo raster, clipped to AOI and simple MUKEYs to reduce size
r <- rast("data/gSSURGO_MN/MapunitRaster_10m_Clip1_and_Reclass/MapunitRaster_10m_Clip1_and_Reclass/Reclass_tif1.tif")

# allows us to translate our "new" (shorter) MUKEYs to the
# originals that match up with the rest of the db
aoi_mu <- read.delim("data/gSSURGO_MN/mukey_new_crosswalk.txt", sep = ",") %>% 
  select(MUKEY, MUKEY_New, Count)



# cluster assignments data ---------------------------------------------------

# all variables
clustv1 <- read_csv("data/mukey_cluster_assignments_and_soilprops.csv")

# dropping ec and carbonates
clustv2 <- read_csv("data/mukey_cluster_assignments_and_soilprops_no_ec_carbs.csv")

# dropping only ec
noec <- read_csv("data/mukey_cluster_assignments_and_soilprops_no_ec.csv")

# PCA pre-processing before k-means
pca <- read_csv("data/pca_mukey_cluster_assignments_and_soilprops.csv")

# function ----------------------------------------------------------------

# k_col expected format is k_n (unquoted), where n is the number of clusters
create_cluster_raster <- function(start_rast,
                                  clust_assign_df,
                                  k_col, 
                                  outfile_basename){
  
  clustkey <- clust_assign_df %>%
    select(mukey, {{k_col}}) %>% 
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
    select(MUKEY_New, clustid)
  
  clustkey_mx <- clustkey %>% 
    as.matrix()
  
  savepath <- glue("E:/big-files-backup/ch03-sh-groups/{outfile_basename}.tif")
  
  r_reclass <- classify(x = start_rast,
                        rcl = clustkey_mx,
                        filename = savepath,
                        datatype = "INT2U",
                        overwrite = TRUE)
  
}

create_cluster_raster(
  start_rast = r,
  clust_assign_df = pca,
  k_col = k_5,
  outfile_basename = "clust5_pca"
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

