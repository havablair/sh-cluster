
# goal: prepare data for pairwise comparisons of validation points (PCA model
# version). We need the soil property data to appear alongside the cluster
# assignments for each value of k. Also need to make sure that for the CIG and
# NRCS SHI data, we only have one validation point per independent experimental
# unit (so average repeated measures from the same "plot" to get representative
# soil property data, and make sure that is associated with just one cluster
# assignment)



# setup -------------------------------------------------------------------

library(tidyverse)
library(glue)

#* soil var data --------------------------------------------------------

# keep only vars relevant to cluster analysis
ncss_dat <- read_csv("data/validation_data/NCSS-KSSL/validation_ncss_kssl_0-20cm_aggr.csv") %>% 
  select(pedon_key,
         claytotal,
         silttotal,
         sandtotal,
         dbthirdbar,
         lep,
         awc = water_ret_diff,
         c_est_org,
         fragvol_sum,
         ec,
         cec,
         ph1to1h2o,
         caco3) %>% 
  # multiply by Van Bemmelen factor to estimate organic matter % 
  # per KSSL Lab Methods Manual 4H2 Section 1 "Application"
  mutate(om_loi = c_est_org * 1.724) %>% 
  rename(val_unit_id = pedon_key)

# pulling in CIG carbonate data separately for now b/c it's not in the main 
# dataset (only kept % inorganic C in that set, but plan to add CCE 2022-12-30)
cig_carbonates <- read_csv("../CIG/cig-cn/data/agvise_calcium_carbonate_equiv_data.csv") %>% 
  select(`Sample ID`, `CCE% D1`) %>% 
  rename(sample_id = `Sample ID`,
         cce_wt_percent = `CCE% D1`)

cig_dat <- read_csv("../CIG/cig-main/cig_lab_data_all_20221129.csv") %>% 
  select(sample_id,
         site,
         treatment,
         position,
         region,
         sand_perc,
         clay4hr_perc,
         silt4hr_perc,
         mean_db_g_cm3_cyl,
         pH,
         OM,
         org_c_wt_percent,
         ic_wt_percent) %>% 
  left_join(x = ., y = cig_carbonates, by = "sample_id")

shi_raw <- read_csv("data/validation_data/NRCS-SHI/merged data 1.26.21 adj.csv") %>% 
  select(farm.id,
         treatment,
         rep,
         om.loi,
         ph,
         bd.shallow,
         per.c.shallow,
         clay.shallow, 
         silt.shallow, 
         sand.shallow
  )

# one texture val used for each pair of fields, make sure this value is 
# represented for both treatments
shi_texture <- shi_raw %>% 
  select(farm.id, clay.shallow, silt.shallow, sand.shallow) %>% 
  filter(!is.na(clay.shallow))

shi_dat <- shi_raw %>% 
  select(-c(clay.shallow, silt.shallow, sand.shallow)) %>% 
  left_join(., shi_texture, by = "farm.id")

# * validation data -------------------------------------------------------

shi_val <- read_csv("data/validation_data/NRCS-SHI/shi_site_pca_validation_points.csv") %>% 
  # doing this string replacement twice b/c farm 8 has two leading zeroes
  mutate(unique_id = str_replace(unique_id, "^0", ""),
         unique_id = str_replace(unique_id, "^0", "")) %>% 
  # fix weird labelling for site that was used twice as control
  # (both 25 NCC and 26 NCC refer to the same physical spot)
  # retain 26NCC b/c this is the label that appears in the validation dataset
  mutate(unique_id = case_when(
    unique_id == "25026NCC" ~ "26NCC", 
    TRUE ~ unique_id
  )) %>% 
  select(val_unit_id = unique_id,
         k_4, k_5, k_6, k_7, k_8, k_9, k_10, k_11)

cig_kssl_val <- read_csv("data/validation_data/cig_ncss-kssl_pca_validation_20230118.csv")

# * create key to translate between sample ID and val_unit_id -------------

cig_key <- cig_dat %>%
  select(sample_id, site, treatment, position) %>% 
  mutate(val_unit_id = glue("{site}-{treatment}-{position}"),
         sample_id = as.character(sample_id)) 

# calculate average soil props for independent soil units -----------------
# "independent soil units" here means that I'm dealing with repeated measures
# that we have in the CIG and SHI datasets, averaging across them so 
# for any given mapunit in the dataset, we have one set of independent soil
# property values 

# NCSS KSSL already OK (each pedon independent)
# CIG need to aggregate to site-trt-position level
# SHI need to aggregate to farm.id-treatment level

# * KSSL ------------------------------------------------------------------

ncss_indep <- ncss_dat

# * CIG -------------------------------------------------------------------

cig_indep <- cig_dat %>% 
  group_by(site, treatment, position) %>% 
  summarise(across(.cols = c(sand_perc,
                             clay4hr_perc,
                             silt4hr_perc,
                             mean_db_g_cm3_cyl,
                             pH,
                             OM,
                             org_c_wt_percent,
                             ic_wt_percent,
                             cce_wt_percent),
                   .fns = ~mean(.x, na.rm = TRUE)),
            .groups = "drop") %>% 
  rename(sandtotal = sand_perc,
         claytotal = clay4hr_perc,
         silttotal = silt4hr_perc,
         dbthirdbar = mean_db_g_cm3_cyl,
         ph1to1h2o = pH,
         om_loi = OM,
         c_est_org = org_c_wt_percent,
         caco3 = cce_wt_percent) %>% 
  mutate(caco3 = case_when(
    is.na(caco3) ~ 0,
    TRUE ~ caco3
  ),
  ic_wt_percent = case_when(
    is.na(ic_wt_percent) ~ 0,
    TRUE ~ ic_wt_percent
  ),
  val_unit_id = glue("{site}-{treatment}-{position}")) %>% 
  select(-c(site, treatment, position))


# * NRCS SHI --------------------------------------------------------------

shi_indep <- shi_dat %>% 
  group_by(farm.id, treatment) %>% 
  summarise(across(.cols = c(om.loi,
                             ph,
                             bd.shallow,
                             per.c.shallow,
                             clay.shallow,
                             silt.shallow,
                             sand.shallow),
                   .fns = ~mean(.x, na.rm = TRUE)),
            .groups = "drop") %>% 
  rename(
    om_loi = om.loi,
    ph1to1h2o = ph,
    dbthirdbar = bd.shallow,
    claytotal = clay.shallow,
    silttotal = silt.shallow,
    sandtotal = sand.shallow,
    c_est_org = per.c.shallow
  ) %>% 
  mutate(
    val_unit_id = glue("{farm.id}{treatment}")
  ) %>% 
  select(-c(farm.id, treatment))


#* join soil prop dfs together--------------------------------------------------

soil_props_df <- bind_rows(ncss_indep, cig_indep, shi_indep)

# how many missing values do we have for each soil property? 

soil_props_df %>% 
  summarise(across(where(is.numeric), ~sum(is.na(.x)))) %>% 
  pivot_longer(cols = everything(),
               names_to = "var", 
               values_to = "n_na") %>% 
  arrange(n_na) %>% 
  mutate(percent_na = (n_na / 175) * 100)


# combine validation datasets ---------------------------------------------

# * consolidate CIG validation points to 1 per val_unit_id ------------------

# first figure out if any sampling areas where assigned 
# different cluster values (so repeated measures crossed mapunit lines)

cig_val <- cig_kssl_val %>% 
  filter(sample_id %in% as.character(c(1:488))) %>% 
  left_join(., cig_key, by = "sample_id") %>% 
  select(sample_id, val_unit_id, x, y, everything())

# 8 val_unit_ids with ambiguous cluster assignments
# let's do a vote count to determine which cluster each belongs to. 
mult_groups <- cig_val %>% 
  group_by(val_unit_id) %>% 
  summarise(across(contains("k_"),
                   .fns = ~str_c(unique(.x), collapse = ","))) %>% 
  pivot_longer(cols = contains("k_"),
               names_to = "k",
               values_to = "cluster_assignment") %>% 
  filter(str_detect(cluster_assignment, ",")) %>% 
  pull(val_unit_id) %>% 
  unique()

# this does a vote count for all val_unit_ids, and selects the 
# cluster assignment with the most votes from among the repeated
# measures (there were only 8 of 82 areas in the CIG dataset that had multiple
# assignments depending on the location of the replicates)
cluster_votes_long <- cig_val %>% 
  pivot_longer(cols = contains("k_"),
               names_to = "k",
               values_to = "cluster_assignment") %>% 
  select(val_unit_id, sample_id, k, cluster_assignment) %>% 
  group_by(val_unit_id, k) %>% 
  count(cluster_assignment) %>% 
  group_by(val_unit_id, k) %>% 
  mutate(max_vote = max(n)) %>% 
  filter(n == max_vote)

# get back to wide format, one row per independent validation unit
# should have 82 rows for CIG

cig_val_wide <- cluster_votes_long %>% 
  select(-c(n, max_vote)) %>% 
  pivot_wider(names_from = "k",
              values_from = "cluster_assignment")

# * prep shi and kssl validation data -------------------------------------

# because we consolidated the cig validation points above, now just 
# need kssl points from the original combined dataset
kssl_val_wide <- cig_kssl_val %>% 
  filter(!(sample_id %in% as.character(1:488))) %>% 
  select(val_unit_id = sample_id,
         contains("k_"))

# function to rename cols in shi_val so they match ncss and cig datasets
rename_shi_col <- function(col_string) {
  
  kval <- str_extract(col_string, pattern = "[:digit:]+")
  
  new_name <- glue("k_{kval}")
  
  return(new_name)
  
}

shi_val_wide <- shi_val %>% 
  rename_with(.data = .,
              .fn = rename_shi_col,
              .cols = contains("clust")) %>% 
  mutate(across(.cols = contains("k_"),
                .fns = ~str_replace(.x, "Cluster_", "")),
         across(.cols = contains("k_"),
                .fns = as.numeric))

val_data_all <- bind_rows(kssl_val_wide, cig_val_wide, shi_val_wide) 


# combine soil prop and val data ------------------------------------------

val_soil_prop_df <- left_join(val_data_all, soil_props_df, by = "val_unit_id")


# save data for pairwise comparisons --------------------------------------

write_csv(val_soil_prop_df, "data/validation_data_pca_clusters_and_soil_props.csv")

