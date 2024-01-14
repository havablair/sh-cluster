# goal: extract the raster values (cluster ids) for each raster from k-20
# for the CIG and NCSS validation points. Recall that the NRCS-SHI validation
# points were done by JB at NRCS for the original model version (data privacy)
# but in the final section here I do it by joining on MUKEY


# setup -------------------------------------------------------------------

library(tidyverse)
library(glue)
library(terra)

# points to validate
ncss_dat <- read_csv("data/validation_data/NCSS-KSSL/validation_ncss_kssl_0-20cm_aggr.csv")

cig_dat <- read_csv("../CIG/cig-main/cig_lab_data_all_20230301.csv")

# ended up making these matrices so I can add ids as "attributes" (atts) 
# in the vect function 
ncss_pts <- ncss_dat %>% 
  select(longitude_decimal_degrees, latitude_decimal_degrees, pedon_key) %>% 
  rename(lon = longitude_decimal_degrees,
         lat = latitude_decimal_degrees,
         sample_id = pedon_key)

cig_pts <- cig_dat %>% 
  select(lon, lat, sample_id) %>% 
  mutate(sample_id = as.character(sample_id))

# all validation pts together
val_pts <- bind_rows(cig_pts, ncss_pts)

val_matrix <- val_pts %>% 
  select(lon, lat) %>% 
  as.matrix()

# terra::extract will return "ID", which is a record number (the row number)
# I will use this df as a key to get the sample_ids back
val_ids <- data.frame(sample_id = val_pts$sample_id,
                      row_id = 1:nrow(val_pts))


# reproject points to NAD 1983 -------------------------------------

# turn validation point dataframe into a SpatVector
val_spat <- vect(x = val_matrix,
                 type = "points",
                 # EPSG:4326 is WGS84 long/lat
                 crs = "epsg:4326",
                 atts = val_ids
)


# reproject the CIG and NCSS points to NAD 83 (EPSG 5070)
val_reproj <- project(x = val_spat,
                      y = "epsg:5070")

# view spatvector properties
val_reproj

# function to extract raster values from 1 .tif ---------------------------

extract_rast_values <- function(k, tif_path, pts_spat_vec){
  
  tif <- rast(tif_path)
  
  extracted_vals_df <- terra::extract(
    x = tif,
    y = pts_spat_vec,
    method = "simple",
    xy = TRUE,
    # note this returns row ID
    ID = TRUE
  ) 
  
  colnames(extracted_vals_df) <- c("row_id", glue("{k}"), "x", "y")
  
  rm(tif)
  
  return(extracted_vals_df)
  
  
}



# extract (sample) raster values-------------------------------------------------


#*  original version: iterate over all .tifs -------------------------------------

# first create a dataframe to hold the arguments we will pass 
# to the extract_rast_values function
tif_paths <- list.files(path = "E:/big-files-backup/ch03-sh-groups/",
                        pattern = "allvar",
                        full.names = TRUE)

k_vals <- map_chr(tif_paths, basename) %>% 
  str_extract(string = .,
              pattern = "[:digit:]+")

k_colnames <- glue("k_{k_vals}")

args_df <- data.frame(
  path = tif_paths,
  k_col = k_colnames
)

# iterate over the dataframe 
results_df <- args_df %>% 
  mutate(extr_vals = map2(.x = k_col,
                          .y = path,
                          .f = extract_rast_values,
                          pts_spat_vec = val_reproj))

# reduce() is an operation that combines the elements of a vector into 
# a single value using a specified function (in this case, left join)
# this allows us to work through our list of dataframes, left joining
# them all together in a very concise way 

results_wide <- purrr::reduce(
  .x = results_df$extr_vals,
  .f = left_join,
  by = c("row_id", "x", "y")
) %>% 
  left_join(x = ., y = val_ids, by = "row_id") %>% 
  select(sample_id, row_id, x, y, everything()) %>% 
  mutate(crs = "NAD_1983_Albers")

write_csv(results_wide,
          file = "data/validation_data/cig_ncss-kssl_allvar_validation_20221230.csv")


# * pca version: iterate over all .tifs -----------------------------------
# note that we used the cluster metrics (C-H index, WSS, silhouette) to narrow
# down the number of candidates (values of k), so there aren't 19 versions here.

pca_tif_paths <- list.files(path = "E:/big-files-backup/ch03-sh-groups/",
                        pattern = "pca",
                        full.names = TRUE)

pca_k_vals <- map_chr(pca_tif_paths, basename) %>% 
  str_extract(string = .,
              pattern = "[:digit:]+")

pca_k_colnames <- glue("k_{pca_k_vals}")

pca_args_df <- data.frame(
  path = pca_tif_paths,
  k_col = pca_k_colnames
)

# iterate over the dataframe 
pca_results_df <- pca_args_df %>% 
  mutate(extr_vals = map2(.x = k_col,
                          .y = path,
                          .f = extract_rast_values,
                          pts_spat_vec = val_reproj))

# reduce() is an operation that combines the elements of a vector into 
# a single value using a specified function (in this case, left join)
# this allows us to work through our list of dataframes, left joining
# them all together in a very concise way 

pca_results_wide <- reduce(
  .x = pca_results_df$extr_vals,
  .f = left_join,
  by = c("row_id", "x", "y")
) %>% 
  left_join(x = ., y = val_ids, by = "row_id") %>% 
  select(sample_id, row_id, x, y, everything()) %>% 
  mutate(crs = "NAD_1983_Albers")

write_csv(pca_results_wide,
          file = "data/validation_data/cig_ncss-kssl_pca_validation_20230118.csv")



# * NRCS-SHI validation for PCA version -----------------------------------

# recall that cluster membership for CIG, NCSS points is determined using 
# extract method above, but SHI is done by MUKEY for the PCA
# model version b/c locations are protected (had help from JB for sampling
# the original version cluster membership)

shi_mukeys <- read_csv("data/validation_data/shi_site_allvar_mukeys_20230118.csv") %>% 
  select(unique_id, MUKEY)

pca_clust <- read_csv("data/pca_mukey_cluster_assignments_and_soilprops.csv")

# function to identify clusters for each val point in a single model scenario
# k=6 for example
id_val_point_clust <- function(k_col){
  
  target_clustering <- pca_clust %>% 
    select(mukey, all_of(k_col))
  
  result <- left_join(shi_mukeys, target_clustering, by = c("MUKEY" = "mukey"))
  
  return(result)
  
}

# create a vector representing all the model scenarios (k = 2-20)
n_clusts <- c(2:20)

clust_cols <- paste("k_", n_clusts, sep = "")

# iterate over the list, then do a series of left joins with "reduce" to get one
# dataframe

val_dat_list <- map(clust_cols, id_val_point_clust)

val_dat_df <- reduce(val_dat_list, .f = left_join, by = c("unique_id", "MUKEY"))


# actually I DON"T want to add the soil props back in here, I think
# it's confusing because these are the SSURGO soil props (and for the 
# validation points we want the EXTERNAL data from each project, not this
# SSURGO data associated with the assigned MUKEY)
# # bring back the soil props, associate them w/ validation pts
# soil_props <- pca_clust %>% 
#   select(-contains("k_"))
# 
# val_dat_props_df <- left_join(val_dat_df, soil_props, by = c("MUKEY" = "mukey"))

write_csv(val_dat_df, "data/validation_data/NRCS-SHI/shi_site_pca_validation_points.csv")


# * extract MUKEYs for CIG points -----------------------------------------

# might use these to make comparisons between taxonomy classification and 
# clustering classification in terms of SH indicator variability explained
# demonstrated with ANOVAs and % sum of squares calculations in 
# "compare_variation_demo.qmd"

# started this originally and then came back, realizing that I need to do 
# more wrangling to get the components right. Originally I was just grabbing
# the "top" (greatest comp_pct) component for each MUKEY, but I think 
# what I want to do is a weighted avg / vote so that a taxonomic classification
# assigned to a given MUKEY takes into account all of the components
# ultimately contributed data in our analysis. 


# ** use CIG locations to extract MUKEYs from gSSURGO raster -----------------

mukey_crosswalk <- read.delim(file = "data/gSSURGO_MN/mukey_new_crosswalk.txt", sep = ",")

# recall that these have the "new" short MUKEYS we created to deal with 
# file size issues.
# below we will join in the real SSURGO MUKEYs using the crosswalk
cig_ncss_mukeys <- extract_rast_values(k = "mukey",
                    tif_path = "data/gSSURGO_MN/MapunitRaster_10m_Clip1_and_Reclass/MapunitRaster_10m_Clip1_and_Reclass/Reclass_tif1.tif",
                    pts_spat_vec = val_reproj)

cig_ncss_mukeys_ids <- left_join(cig_ncss_mukeys, val_ids, by = "row_id") %>% 
  rename(MUKEY_New = mukey)

cig_ncss <- left_join(cig_ncss_mukeys_ids, mukey_crosswalk, by = "MUKEY_New") %>% 
  select(-c(MUKEY_New, OID_, Count, Value))

write_csv(cig_ncss, "cig_ncss_validation_pt_mukey_sample_id.csv")


# ** subset component table to only include components for CIG mukeys --------


# ALL the components 
comps <- read_csv("data/component_list.csv")

# only the components we included when calculating 
# weighted averages for each MUKEY (based on comp pct). These 
# components all have complete data 
comp_key <- read_csv("data/key_cokey_mukey_complete_cases_include.csv")

# only want the complete/included cokeys
comps_incl <- comps %>% 
  filter(cokey %in% comp_key$cokey)

# keep the unique MUKEYs in the CIG validation dataset 
# 38 total
unique_mukeys <- cig_ncss %>%
  # keep only the CIG data
  filter(sample_id %in% as.character(c(1:488))) %>% 
  pull(MUKEY) %>% unique()

# data for the components which contributed data to 
# MUKEYs associated with CIG validation points
cig_comps <- comps_incl %>% filter(mukey %in% unique_mukeys) %>% 
  left_join(., cig_ncss, by = c("mukey" = "MUKEY"))

# now need to create key associating sample ids with column indicating
# independent validation units (summarize the CIG data to the "mapunit"
# level)

cig_key <- cig_dat %>%
  select(sample_id, site, treatment, position) %>% 
  mutate(val_unit_id = glue("{site}-{treatment}-{position}"),
         sample_id = as.character(sample_id)) 

cig_comps_labelled <- left_join(cig_comps, cig_key, by = "sample_id")


# 9 val_unit_ids with ambiguous soil mapunit assignments
# let's do a vote count to determine which cluster each belongs to. 
# (recall that there are 6 GPS points associated with each CIG 
# mapunit, if these points didn't all fall in the same MUKEY per SSURGO
# boundaries this is why we would have more than one)
mult_groups <- cig_comps_labelled %>% 
  group_by(val_unit_id) %>% 
  summarise(smu = str_c(unique(mukey), collapse = ",")) %>% 
  filter(str_detect(smu, ",")) %>% 
  pull(val_unit_id) %>% 
  unique()

# this does a vote count for all val_unit_ids, and selects the 
# mukeys with the most votes from among the repeated
# measures (there were 9 of 82 areas in the CIG dataset that had multiple
# assignments based on the location of the replicates)
mukey_votes_long <- cig_comps_labelled %>% 
  select(val_unit_id, sample_id, mukey) %>% 
  group_by(val_unit_id) %>% 
  count(mukey) %>% 
  group_by(val_unit_id) %>% 
  mutate(max_vote = max(n)) %>% 
  filter(n == max_vote, 
         # had to look at the map of our sampling points
         # to break a tie in RR1-CV-B
         !(val_unit_id == "RR1-CV-B" & mukey == 2798997),
         # had an add'l treatment in this field, dropping it 
        # for consistency with the other sites (which only had CV-SH-UD)
         !(val_unit_id == "RR1-CC-B"))

write_csv(mukey_votes_long, "data/cig_mukey_votes_for_validation.csv")


cig_comp_tbl_final <- cig_comps_labelled %>% 
  # this doesn't drop any rows, which makes sense b/c CIG sampling
  # areas on the border of two mapunits, likely the second mapunit
  # was part of the other (upper or lower) sampling 
  # area in that field, and thus was included in the dataset anyway
  # BUT it's not very helpful, b/c it means that we still have multiple 
  # mukeys associated with some val_unit_ids.
  # so I ended up saving a separate dataset above "cig_mukey_votes..."
  filter(mukey %in% mukey_votes_long$mukey) 

write_csv(cig_comp_tbl_final, "data/cig_incl_components_table.csv")


# some examples of the table summaries I want to add to one of my chapters for
# reference

cig_comps %>% count(taxorder)

cig_comps %>% count(taxsuborder)



