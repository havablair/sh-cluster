# goal: make a small heatmap of Minnesota, with counties highlighted
# based on the number of validation points (val_unit_ids) in each one. 
library(readxl)
library(tidyverse)
library(glue)
library(sf)
library(patchwork)



# SHI counties ------------------------------------------------------------

val <- read_csv("data/validation_data_pca_clusters_and_soil_props.csv")

shi <- read_excel("data/validation_data/NRCS-SHI/nrcs_shi_site_level_datasheet.xlsx") %>% 
  select(unique_id, PedonID) %>% 
  mutate(fips_code = str_extract(PedonID, "(?<=MN)[:digit:]{3}"))

# need for identifying counties of SHI points
fips_mn <- tigris::fips_codes %>% 
  filter(state == "MN")

shi_incl <- val %>% filter(str_detect(val_unit_id, "CC"),
                           !str_detect(val_unit_id, "RR1")) %>% 
  drop_na(k_8)

shi_counties <- left_join(shi, fips_mn, by = c("fips_code" = "county_code")) %>% 
  select(val_unit_id = unique_id,
         county) %>% 
  mutate(county = str_replace(county, " County", ""),
         # drop leading zeros so the join with sh_incl works
         val_unit_id = str_replace(val_unit_id, "^0+", "")) %>% 
  mutate(val_unit_id = ifelse(val_unit_id == "25026NCC", "26NCC", val_unit_id)) %>% 
  filter(val_unit_id %in% shi_incl$val_unit_id)

# CIG counties ------------------------------------------------------------

cig_dat <- read_csv("../CIG/cig-main/cig_lab_data_all_20230301.csv")
mn_counties <- st_read("_qgis_files/shp_bdry_counties_in_minnesota/mn_county_boundaries.shp") %>% 
  mutate(state = "MN")

cig_pts <- cig_dat %>% 
  select(sample_id, site, treatment, position, replicate, lon, lat) %>% 
  mutate(val_unit_id = glue("{site}-{treatment}-{position}")) %>% 
  filter(replicate == 1,
         sample_id %in% c(1:243))

cig_sf <- st_as_sf(cig_pts, 
                   coords = c("lon", "lat"),
                   crs = 4326)

cig_trans <- st_transform(cig_sf, 
                          crs = st_crs(mn_counties))

# make sure it looks right
ggplot() +
  geom_sf(data = mn_counties) +
  geom_sf(data = cig_trans) 

# on 2023-12-10, create and save a simple BW map to use in an updated version
# of the inset figure that complies with Crop Science figure color rules 
# (is interpretable in grayscale)

bw_mn_counties <- ggplot() +
  geom_sf(data = mn_counties, fill = "white") +
  theme_bw()

# helpful blog post about point in polygon operations
# https://mattherman.info/blog/point-in-poly/
cig_counties <- st_join(cig_trans, mn_counties, join = st_within) %>% 
  select(val_unit_id, CTY_NAME) %>% 
  st_drop_geometry() %>% 
  rename(county = CTY_NAME)


# NCSS counties -----------------------------------------------------------

# need this val data to help identify only the "include" ncss points based on location
# included points will have values for the cluster (k_) columns
ncss_incl <- read_csv("data/validation_data_pca_clusters_and_soil_props.csv") %>% 
  # keep NCSS ids only
  filter(str_detect(val_unit_id, "[:digit:]{4}")) %>% 
  drop_na(k_8)


ncss_dat <- read_csv("data/validation_data/NCSS-KSSL/validation_ncss_kssl_0-20cm_aggr.csv")

ncss_temp <- ncss_dat %>% 
  select(val_unit_id = pedon_key, mn_fips = county) %>% 
  filter(val_unit_id %in% ncss_incl$val_unit_id) %>% 
  mutate(fips_code = str_extract(mn_fips, "[:digit:]{3}"))

ncss_counties <- left_join(ncss_temp, fips_mn, c("fips_code" = "county_code")) %>% 
  select(val_unit_id, county) %>% 
  mutate(county = str_replace(county, " County", ""))
  

# join county data & plot -------------------------------------------------

all_counties_in_val <- bind_rows(cig_counties, ncss_counties, shi_counties) %>% 
  mutate(county = case_when(
    # had to go back to Field Data Summary (Master) to fill in these missing
    # counties for NRCS SHI data
    (is.na(county) & str_detect(val_unit_id, "041")) ~ "Pipestone", 
    (is.na(county) & str_detect(val_unit_id, "075")) ~ "Stevens", 
    (is.na(county) & str_detect(val_unit_id, "084")) ~ "Dakota", 
    TRUE ~ county
  ))


county_counts <- all_counties_in_val %>% 
  count(county) 

mn_county_names <- mn_counties %>% 
  select(CTY_NAME) %>% 
  st_drop_geometry() 

county_summary <- left_join(mn_county_names, county_counts, by = c("CTY_NAME" = "county")) 

mn_counties_n <- left_join(mn_counties, county_summary, by = "CTY_NAME") 

# should have 26 counties with n>0
mn_counties_n %>% filter(n>0) %>% nrow()

aoi_mn <- st_read("_qgis_files/aoi_shapefiles_from_r/mn_mlras_clipped.shp")
aoi_mn_trans <- st_transform(aoi_mn, st_crs(mn_counties_n))
aoi_union <- st_union(aoi_mn_trans)

mn_boundary <- st_read("_qgis_files/shp_mn_state_boundary/mn_state_boundary.shp")
mn_bdry_trans <- st_transform(mn_boundary, st_crs(mn_counties_n))

counties_in_aoi <- st_intersection(mn_counties_n, aoi_union)



val_point_heatmap <- ggplot() + 
  geom_sf(data = mn_counties, fill = "grey60") +
  geom_sf(data = counties_in_aoi, aes(fill = n)) +
  scale_fill_viridis_c(na.value = "white") + 
  theme_bw() +
  ggtitle("Distribution of validation points")

ggsave(filename = "figs/valpoint_heatmap.png", plot = val_point_heatmap, width = 5, height = 4, units = "in")


# check CIG MLRAs ---------------------------------------------------------

# on 2023-03-22, learned that all my Stearns county CIG points don't share
# an MLRA. Need to determine which MLRA they belong to (looks like they are split across 3)
#  and run the variation explained calculations again

# plot the points
ggplot() +
  geom_sf(data = aoi_mn_trans, aes(fill = MLRA_NAME)) +
  geom_sf(data = cig_trans) 

# helpful blog post about point in polygon operations
# https://mattherman.info/blog/point-in-poly/

cig_mlras <- st_join(cig_trans, aoi_mn_trans, join = st_within) %>% 
  select(val_unit_id, MLRA_NAME) %>% 
  st_drop_geometry() 

write_csv(cig_mlras, "data/cig_mlra_by_val_unit_id.csv")
