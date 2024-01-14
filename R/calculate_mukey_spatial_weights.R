

# about ------------------------------------------------------------------- I
# need to determine the pixel counts of all the MUKEYs in my AOI raster, and
# drop the ones we aren't interested in. I can do pixel counts for each MUKEY
# based on the existing Reclass_tif1.tif and mulitply number pixels x pixel
# area. These values will be used for spatial weights.


# setup -------------------------------------------------------------------


library(dplyr)
library(readr)
library(terra)

# can pull **included** MUKEYs from these clustering results 
incl_mukeys <- read_csv("data/mukey_cluster_assignments_and_soilprops.csv") %>% 
  select(mukey) %>% 
  mutate(include = TRUE)

# crosswalk between gSSURGO mukeys and my shorter ones
# curious if "count" will match
cwalk <- read.delim("data/gSSURGO_MN/mukey_new_crosswalk.txt", sep = ",") %>% 
  select(MUKEY, MUKEY_New, Count)

mkey <- left_join(cwalk, incl_mukeys, by = c("MUKEY" = "mukey")) %>% 
  mutate(include = case_when(
    is.na(include) ~ FALSE,
    TRUE ~ TRUE
  )) %>% 
  rename(arc_count = Count)

# gssurgo raster, clipped to AOI and simple MUKEYs to reduce size
r <- rast("data/gSSURGO_MN/MapunitRaster_10m_Clip1_and_Reclass/MapunitRaster_10m_Clip1_and_Reclass/Reclass_tif1.tif")

# takes a couple minutes to run
freq_tbl <-  terra::freq(r)

freq_tbl2 <- left_join(freq_tbl, mkey, by = c("value"="MUKEY_New"))

# check if the original counts from the Arc attribute table I saved
# are the same as what I get with terra::freq()  (they should be)
freq_tbl2 %>% 
  mutate(is_same = count == arc_count) %>% 
  filter(is_same == FALSE)

# Answer, yes, all the same. Can use the cell counts from the
# mukey_new_crosswalk.txt for calculating the area for each mapunit, and using
# as spatial weight for reporting spatially weighted means for the different
# clusters/soil health regions
