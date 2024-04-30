# in response to reviewer comments on this manuscript, I've been comparing 
# different grouping methods (suborder + particle size class, MLRA + suborder,
# etc.) One grouping that the reviewer suggested was order + texture + climate
# I did this and for the CIG validation points it results in 8 classes represented
# for the CIG points. This makes for a nice comparison with my kmeans soil + climate clusters, 
# which also have 8 classes represented among the CIG points. 
#
# In thinking more about this option, I was curious how many different 
# order + particle size class + climate combinations existed across my
# entire area of interest. That is what I am figuring out here. 

library(tidyverse)
library(sf)
library(terra)

# climate zone assignments for all mukeys in the AOI
clim_mukeys <- read_csv("data/mukey_cluster_assignments_and_props_climate_only.csv") %>% 
  rename_with(.fn = ~str_replace(.x, "k_", "clim_k_"), .cols = contains("k_")) %>% 
  select(mukey, clim_k_3) %>% 
  mutate(clim_k_3 = case_when(
    clim_k_3 == "Cluster_1" ~ "Northwest",
    clim_k_3 == "Cluster_2" ~ "Central",
    clim_k_3 == "Cluster_3" ~ "Southeast"
  ))


key <- read.delim("data/gSSURGO_MN/mukey_new_crosswalk.txt", sep = ",") %>% 
  select(MUKEY, MUKEY_New, Count)

comps <- read_csv("data/key_cokey_mukey_complete_cases_include.csv")

mn_gdb <- "data/gSSURGO_MN/gSSURGO_MN.gdb"
  
# read only component table, as dataframe
mn_comp <- sf::st_read(dsn = mn_gdb, layer = "component")

target_comp <- mn_comp %>% 
  dplyr::filter(cokey %in% comps$cokey)



# function to look at all components in a given map unit
# and determine representative taxonomic classification or characterstic
# based on comppct_r
identify_rep_group_from_comp <- function(var_name, comp_df){
  
  comp_df %>% 
    select(mukey, cokey, comppct_r, {{var_name}}) %>% 
    distinct() %>% 
    group_by(mukey, {{var_name}}) %>% 
    summarise(tot_percent = sum(comppct_r), .groups = "drop") %>% 
    group_by(mukey) %>% 
    slice_max(tot_percent) 
  # %>% 
  #   select(-tot_percent)
  
}



# order-particle size - climate -------------------------------------------


# identify representative particle size for each mukey
mukey_dominant_partsize <- identify_rep_group_from_comp(
  var_name = taxpartsize,
  comp_df = target_comp
  ) %>% 
  mutate(mukey = as.integer(mukey))

partsize_ties <- mukey_dominant_partsize %>% count(mukey) %>% filter(n>1)

mukey_dominant_partsize %>% filter(mukey %in% partsize_ties$mukey)


# identify representative order for each mukey
mukey_dominant_order <- identify_rep_group_from_comp(
  var_name = taxorder, comp_df = target_comp
) %>% 
  mutate(mukey = as.integer(mukey))

order_ties <- mukey_dominant_order %>% count(mukey) %>% filter(n>1)

mukey_dominant_order %>% filter(mukey %in% order_ties$mukey)


# resolve ties (3)
# strategy is to select the single component with the highest comppct_r
target_comp %>% 
  filter(mukey %in% c(order_ties$mukey, partsize_ties$mukey)) %>% 
  select(mukey, cokey, compname, compkind, comppct_r, taxorder, taxsuborder, taxpartsize)

# for mukey 1653641, select cokey 21794147 (particle size = sandy)
# for mukey 398725, select cokey 21754843 (order = Inceptisols)
# for mukey 431547, select cokey 21787061 (order = Alfisols)

mukey_dom_order_no_ties <- mukey_dominant_order %>% 
  filter(
         !(mukey == 398725 & taxorder == "Mollisols"), 
         !(mukey == 431547 & taxorder == "Mollisols"), 
         ) %>% 
  select(-tot_percent)

mukey_dom_partsize_no_ties <- mukey_dominant_partsize %>% 
  filter(
    !(mukey == 1653641 & taxpartsize == "fine-loamy")
  ) %>% 
  select(-tot_percent)


clim_order <- left_join(clim_mukeys, mukey_dom_order_no_ties, by = 'mukey')

clim_order_partsize <- left_join(clim_order, mukey_dom_partsize_no_ties, by = 'mukey')


unique_classes_clim_order_partsize <- clim_order_partsize %>% 
  select(-mukey) %>% 
  distinct()


clim_order_partsize %>% 
  select(taxorder, taxpartsize) %>% 
  distinct()


# suborder ----------------------------------------------------------------

# identify representative suborder for each mukey
mukey_dominant_suborder <- identify_rep_group_from_comp(
  var_name = taxsuborder, comp_df = target_comp
) %>% 
  mutate(mukey = as.integer(mukey))

suborder_ties <- mukey_dominant_suborder %>% count(mukey) %>% filter(n>1)

mukey_dominant_suborder %>% filter(mukey %in% suborder_ties$mukey)


# resolve ties (2)
# strategy is to select the single component with the highest comppct_r
target_comp %>% 
  filter(mukey %in% c(suborder_ties$mukey)) %>% 
  select(mukey, cokey, compname, compkind, comppct_r, taxorder, taxsuborder, taxpartsize) 

# for mukey 1653641, select cokey 21794147 (particle size = sandy)
# for mukey 398725, select cokey 21754843 (order = Inceptisols)
# for mukey 431547, select cokey 21787061 (order = Alfisols)

mukey_dom_suborder_no_ties <- mukey_dominant_suborder %>% 
  filter(
    !(mukey == 398725 & taxsuborder == "Udolls"), 
    !(mukey == 431547 & taxsuborder == "Udolls"), 
  ) %>% 
  select(-tot_percent)


suborder <- left_join(clim_mukeys, mukey_dom_suborder_no_ties, by = "mukey")


suborder %>% 
  count(taxsuborder)


# mlra-suborder ------------------------------------------------------------

centroid_df <- read_csv('data/mapunit_centroids_nad1983_epsg5070.csv') %>% 
  rename(lon = mean_lon,
         lat = mean_lat) %>% 
  select(-n) %>%
  # because my short Mukeys are essentially row numbers (1-7859)
  # adding an ID (row number) column here AND arranging by mukey_short
  # is redundant. Having a separate ID column is important because
  # it's conceivable that you might NOT have mukeys that line up exactly 
  # with row numbers, and then you wouldn't have a way to join the row ids
  # returned by terra::extract back with the appropriate mukeys in 
  # this dataframe. I made this mistake the first time around (not sorting
  #by mukey_short OR creating ID column) and got some weird climate results
  arrange(mukey_short) %>% 
  mutate(ID = row_number())

# NAD 1983 / EPSG 5070
centroids_nad83 <- vect(centroid_df, geom = c("lon", "lat"), crs = 'epsg:5070')
centroids <- project(centroids_nad83, 'epsg:4269')

# EPSG 4269 / NAD 83
mn <- st_read('data/cb_2018_us_state_500k/cb_2018_us_state_500k.shp') %>% 
  filter(NAME == "Minnesota")
mnspat <- vect(mn)

mlras_extended <- terra::vect("_qgis_files/aoi_shapefiles_from_r/mn_mlras_extended.shp")

# all mukeys with associated mlra, but some are na (b/c points are slightly
# outside mlra polygon borders, zoom in on plot below to see)
mlra_mukey <- extract(x = mlras_extended, 
                      y = centroids)

plot(mlras_extended)
points(centroids)

# grab just the na ones
na_mlra <- mlra_mukey %>% filter(is.na(MLRA_ID))

# create df & vect of na points, will use to find nearest MLRA polygon
na_points_df <- centroid_df %>% 
  filter(ID %in% na_mlra$id.y) %>% 
  mutate(na_row_id = row_number())

na_points_key <- na_points_df %>% 
  select(mukey_short, na_row_id)

na_pts_nad83 <- vect(na_points_df, geom = c("lon", "lat"), crs = 'epsg:5070')
na_pts <- project(na_pts_nad83, 'epsg:4269')

# nearest returns the ids (row ids) for both input points (mukey centroids)
# and input polygons (mlras), need to get these for the mlras so I can
# join in the MLRA names
na_nearest_mlra <- nearest(x = na_pts, y = mlras_extended)
na_nearest_df <- terra::values(na_nearest_mlra)

mlra_ids <- terra::values(mlras_extended) %>% 
  rowid_to_column(var = "rowid")

na_pts_mlras <- left_join(na_nearest_df, mlra_ids, by = c("to_id" = "rowid"))

na_pts_mlras_ids <- left_join(
  na_pts_mlras,
  na_points_key,
  by = c("from_id" = "na_row_id")
  ) %>% 
  select(mukey_short, contains("MLRA"), contains("LRR"))

mlra_mukey_no_na <- mlra_mukey %>% 
  filter(!is.na(MLRA_ID)) %>% 
  rename(mukey_short = id.y)


all_mukeys_mlras <- bind_rows(mlra_mukey_no_na, na_pts_mlras_ids) %>% 
  left_join(x = ., y = key, by = c("mukey_short" = "MUKEY_New")) %>% 
  filter(MUKEY %in% clim_mukeys$mukey) %>% 
  select(-Count) %>% 
  left_join(x = ., y = suborder, by = c("MUKEY" = "mukey"))


all_mukeys_mlras %>% 
  count(MLRA_NAME, taxsuborder)

# suborder-particle size --------------------------------------------------


# identify representative suborder for each mukey
# already did this above, can use suborder df
suborder


# identify representative particle size for each mukey 
# already did this above can use
mukey_dom_partsize_no_ties


suborder_partsize_incl <- left_join(clim_mukeys, suborder, by = 'mukey') %>% 
  select(-contains("clim")) %>% 
  left_join(x = ., y = mukey_dom_partsize_no_ties, by = "mukey")


suborder_partsize_incl %>% 
  count(taxsuborder, taxpartsize)

# kmsoil-clim clusters ----------------------------------------------------

clim_mukeys <- read_csv("data/mukey_cluster_assignments_and_props_climate_only.csv") %>% 
  rename_with(.fn = ~str_replace(.x, "k_", "clim_k_"), .cols = contains("k_")) %>% 
  select(mukey, clim_k_3) %>% 
  mutate(clim_k_3 = case_when(
    clim_k_3 == "Cluster_1" ~ "Northwest",
    clim_k_3 == "Cluster_2" ~ "Central",
    clim_k_3 == "Cluster_3" ~ "Southeast"
  )) 

kmsoil_mukeys <- read_csv("data/pca_mukey_cluster_assignments_and_soilprops.csv") %>% 
  select(mukey, k_8)


kmsoil_clim_clusters <- left_join(kmsoil_mukeys, clim_mukeys, by = "mukey")


kmsoil_clim_clusters %>% 
  count(k_8, clim_k_3)

xtabs(~k_8 + clim_k_3, data = kmsoil_clim_clusters)


# great group -------------------------------------------------------------

# identify representative great group for each mukey
mukey_dominant_grtgrp <- identify_rep_group_from_comp(
  var_name = taxgrtgroup,
  comp_df = target_comp
) %>% 
  mutate(mukey = as.integer(mukey))

grtgrp_ties <- mukey_dominant_grtgrp %>% count(mukey) %>% filter(n>1)

mukey_dominant_grtgrp %>% filter(mukey %in% grtgrp_ties$mukey)

target_comp %>% 
  filter(mukey %in% c(grtgrp_ties$mukey)) %>% 
  select(mukey, cokey, compname, compkind, comppct_r, taxgrtgroup) 


mukey_dom_grtgrp_no_ties <- mukey_dominant_grtgrp %>% 
  filter(
    !(mukey == 398725 & taxgrtgroup == "Hapludolls") 
  ) %>% 
  select(-tot_percent)


all_mukeys_grt_groups <- left_join(clim_mukeys, mukey_dom_grtgrp_no_ties, by = "mukey") %>% 
  select(-clim_k_3)



# additional taxonomic info -----------------------------------------------

# identify representative subgroup for each mukey

mukey_dominant_subgrp <- identify_rep_group_from_comp(
  var_name = taxsubgrp, comp_df = target_comp
) %>% 
  mutate(mukey = as.integer(mukey))

subgrp_ties <- mukey_dominant_subgrp %>% count(mukey) %>% filter(n>1)

mukey_dominant_subgrp %>% filter(mukey %in% subgrp_ties$mukey)


# resolve ties (3)
# strategy is to select the single component with the highest comppct_r
target_comp %>% 
  filter(mukey %in% subgrp_ties$mukey) %>% 
  select(mukey, cokey, compname, compkind, comppct_r, taxorder, taxsuborder, taxpartsize, taxsubgrp)

# for mukey 1653641, select cokey 21794147 (particle size = sandy)

mukey_dom_subgrp_no_ties <- mukey_dominant_subgrp %>% 
  filter(
    # perfect tie, dropping the one with compkind == Taxadjunct ("Grayling")
    !(mukey == 2732651 & taxsubgrp == "Typic Udipsamments"), 
  ) %>% 
  select(-tot_percent)


# create one dataframe ----------------------------------------------------


temp1 <- left_join(clim_order_partsize, kmsoil_mukeys, by = "mukey") %>% 
  select(-taxpartsize)

temp2 <- left_join(temp1, suborder_partsize_incl, by = "mukey")

mlras_only <- all_mukeys_mlras %>% 
  select(mukey = MUKEY, MLRA_NAME)

temp3 <- left_join(temp2, mlras_only, by = "mukey")

temp4 <- left_join(temp3, mukey_dom_subgrp_no_ties, by = "mukey")

final <- left_join(temp4, all_mukeys_grt_groups, by = "mukey")


write_csv(final, "data/mukey_category_assignments_for_count_tables.csv")


final <- read_csv("data/mukey_category_assignments_for_count_tables.csv")

final %>% 
  count(k_8, clim_k_3)

xtabs(~clim_k_3 + k_8, data = final)

