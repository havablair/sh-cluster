# goal: pull together NCSS lab data for validation (post-2000 pedons only)
# see 02-data-sources.qmd for more info on where I downloaded this NCSS data
# (Kellogg Lab data). 

library(tidyverse)
library(janitor)


# unzip (only do this once) -----------------------------------------------


# zip_paths <- list.files("data/validation_data/NCSS-KSSL/",
#                         pattern = ".zip",
#                         full.names = TRUE)
# 
# exdir_paths <- zip_paths %>% str_replace(pattern = ".zip", "")
# 
# map2(.x = zip_paths,
#      .y = exdir_paths,
#      .f = ~unzip(zipfile = .x, exdir = .y))
# 
# 
# unzip("data/validation_data/NCSS-KSSL/NCSS_Lab_Data_Mart_09142018.zip",
#       exdir = "data/validation_data/NCSS-KSSL/NCSS_Lab_Data_Mart_09142018")
# 
# unzip("data/validation_data/NCSS-KSSL/NCSS_Soil_Characterization_Database_09_14_2018.gdb.zip")


# site and pedon info-----------------------------------------------------------
# see notes in 02-data_sources.qmd > NCSS (Kellogg Lab) Data 

# grab one site spreadsheet and one pedon sheet 
# (they are all the same, included w/ every download)
site <- read_csv("data/validation_data/NCSS-KSSL/bd_moisture_water_content/site.csv")
pedon <-
  read_csv("data/validation_data/NCSS-KSSL/bd_moisture_water_content/pedon.csv") %>%
  select(
    user_site_id,
    pedon_key,
    user_pedon_id,
    sampled_taxon_name,
    sampled_class_name,
    correlated_taxon_name,
    correlated_class_name
  )

# how many sites have lat/lon? 
site %>% 
  filter(!is.na(latitude_decimal_degrees)) %>% 
  nrow()

# filter to only sites w/ location data
# 55 total
site_loc <- site %>% 
  filter(!is.na(latitude_decimal_degrees))

# join site and pedon data 
# 56 rows here indicates that 1 of the sites has 2 pedons
site_ped <- left_join(pedon, site_loc, by = "user_site_id") %>% 
  filter(!is.na(latitude_decimal_degrees))

# which site has multiple pedons?
# S2005MN069052
# the site location is mapped as Enstrom loamy fine sand (one of the pedons)
# other pedon is Northcote, but I don't see any of that nearby.
# might need to drop that Northcote since these points share a GPS location
site_ped %>% group_by(user_site_id) %>% count() %>% filter(n>1)

# this dataset has the target pedons, duplicates removed
site_ped_nodup <- site_ped %>% 
  filter(pedon_key != "06N0416")


# combine lab data  -------------------------------------------------------

# use this vector to filter out data for target pedons
pedons <- site_ped_nodup$pedon_key

## bulk density 

db <- read_csv("data/validation_data/NCSS-KSSL/bd_moisture_water_content/Bulk_Density_and_Moisture.csv") %>% 
  filter(pedon_key %in% pedons) %>% 
  select(pedon_key,
         layer_key,
         layer_sequence,
         hzn_top,
         hzn_bot,
         hzn_desgn,
         texture_description,
         # bulk density
         `Db1/3_4A1d_Caj_g/cc_0_CMS_0_0`,
         `Db13b_3B1b_Caj_0_SSL_0_0`,
         `Db13b_DbWR1_Caj_g/cc_0_SSL_0_0`, 
         # LEP (COLE),
         `COLEws_d-1...17`,
         `COLEws_d-1...18`,
         # water retention difference (AWC)
         `wrdws3_d-1_S...35`,
         `wrdws3_d-1_S...36`
         )

# clay, silt, sand, rock frags >2mm (weight %)
psda <- read_csv("data/validation_data/NCSS-KSSL/psda_rock_water_disp/PSDA_and_Rock_Fragments.csv") %>% 
  filter(pedon_key %in% pedons) %>% 
  select(pedon_key,
         layer_key,
         # texcl: 2 cols w/ same info, taking first one
         `Textur_d-1_S...11`,
         `ClyT_p_3A1_Sjj_% wt_0_CMS_0_0`,
         `Clay_3A1a1a_Sjj_39_SSL_0_0`,
         `Clay_PSDAr1_Sjj_% wt_39_SSL_0_0`,
         # grabbing silt & sand totals too
         `Silt_d-1_S...16`,
         `Silt_d-1_S...17`,
         `Sand_d-1_S...18`,
         `Sand_d-1_S...19`,
         )

 # carbon and extractions

carbon <- read_csv("data/validation_data/NCSS-KSSL/carbon_and_extractions/Carbon_and_Extractions.csv") %>% 
  filter(pedon_key %in% pedons) %>% 
  select(pedon_key,
         layer_key,
         # total carbon (incl. carbonates - exclude for now)
         # `C_tot_6A2e_Sjj_% wt_126_CMS_0_0`,
         # `C_tot_6A2f_Sjj_% wt_126_CMS_0_0`,
         # `Ctot_4H2a_Sjf_8_SSL_0_0`,
         # `Ctot_TotNCS_Sjf_% wt_8_SSL_0_0`,
         # est. organic carbon 
         `corgest_d-0_S`,
         `eoc_d-1_S...24`,
         `eoc_d-1_S...25`,
  )

# organic (name of spreadsheet, but the variable we want is vol % frags)

volfrag <- read_csv("data/validation_data/NCSS-KSSL/cec_bases_salt_organic/Organic.csv") %>% 
  filter(pedon_key %in% pedons) %>% 
  select(pedon_key,
         layer_key,
         # vol percent 2-75mm frags at 1/3bar
         `vp752_d-1_S...26`,
         `vp752_d-1_S...27`
  )

# EC (salts spreadsheet)
ec <- read_csv("data/validation_data/NCSS-KSSL/cec_bases_salt_organic/Salt.csv") %>% 
  filter(pedon_key %in% pedons) %>% 
  select(pedon_key,
         layer_key,
         # EC
         `EC_4F2b1_Sjj_54_SSL_0_0`,
         `EC_sx_8A3a_Sjj_dS/m_122_CMS_0_0`)


# cec and bases 

cec <- read_csv("data/validation_data/NCSS-KSSL/cec_bases_salt_organic/CEC_and_Bases.csv") %>% 
  filter(pedon_key %in% pedons) %>% 
  select(pedon_key,
         layer_key,
         # CEC at pH 7
         `CECnh4_5A8b_Sjj_cmol(+)/kg_121_CMS_0_0`,
         `CEC_4B1a1b_Sjj_47_SSL_0_0`,
         `CEC_CEC1_Sjj_cmol(+)/kg_47_SSL_0_0`,
         `CEC_4B1a1a_Sjj_47_SSL_0_0`,
         `CEC_CECd_Sjj_cmol(+)/kg_47_SSL_0_0`
         
  )

# pH and carbonates 

ph_carb <- read_csv("data/validation_data/NCSS-KSSL/ph_carbonates/pH_and_Carbonates.csv") %>% 
  filter(pedon_key %in% pedons) %>% 
  select(pedon_key,
         layer_key,
         # pH 1:1 h2o
         `pH_h2o_8C1f_Sjj_(NA)_123_CMS_0_0`,
         `pHh2o_4C1a2a1_Sjj_73_SSL_0_0`,
         `pHh2o_pHRou1_Sjj_(NA)_73_SSL_0_0`,
         # carbonates in <2mm fraction
         `CaCO3_4E1a1a1a1_Sjf_40_SSL_0_0`,
         `CaCO3_6E1g_Sjj_% wt_118_CMS_0_0`,
         `CaCO3_6E1h_Sjj_% wt_118_CMS_0_0`,
         `CaCO3_Carb1_Sjf_% wt_40_SSL_0_0`)
 
# join lab datasets together   

dat1 <- left_join(psda, db, by = c("pedon_key", "layer_key"))

dat2 <- left_join(dat1, carbon, by = c("pedon_key", "layer_key"))

dat3 <- left_join(dat2, volfrag, by = c("pedon_key", "layer_key"))   

dat4 <- left_join(dat3, ec, by = c("pedon_key", "layer_key"))

dat5 <- left_join(dat4, cec, by = c("pedon_key", "layer_key"))

dat_all <- left_join(dat5, ph_carb, by = c("pedon_key", "layer_key"))


# clean up combined data  -------------------------------------------------


# are there any empty columns?
dat_all %>% 
  janitor::remove_empty(which = "cols",
               quiet = FALSE)

# some of the columns that should contain numbers are "character" type
# mostly becuase some 0s or NAs appear to have been coded with "--"
dat_num <- dat_all %>% 
  mutate(across(.cols = -c(pedon_key, layer_key, `Textur_d-1_S...11`,
                           hzn_desgn, texture_description),
                .fns = as.numeric
                ))

# collapse down columns that have the same soil property
# but measured with different methods 
dat_coalesce <- dat_num %>% 
  mutate(
    claytotal = coalesce(`ClyT_p_3A1_Sjj_% wt_0_CMS_0_0`,
                         `Clay_3A1a1a_Sjj_39_SSL_0_0`,
                         `Clay_PSDAr1_Sjj_% wt_39_SSL_0_0`),
    silttotal = coalesce(`Silt_d-1_S...16`,
                         `Silt_d-1_S...17`),
    sandtotal = coalesce(`Sand_d-1_S...18`,
                         `Sand_d-1_S...19`),
    dbthirdbar = coalesce(`Db1/3_4A1d_Caj_g/cc_0_CMS_0_0`,
                          `Db13b_3B1b_Caj_0_SSL_0_0`,
                          `Db13b_DbWR1_Caj_g/cc_0_SSL_0_0`),
    lep = coalesce(`COLEws_d-1...17`,
                   `COLEws_d-1...18`),
    water_ret_diff = coalesce(`wrdws3_d-1_S...35`,
                              `wrdws3_d-1_S...36`),
    c_est_org = coalesce(`corgest_d-0_S`,
                         `eoc_d-1_S...24`,
                         `eoc_d-1_S...25`),
    fragvol_sum = coalesce(`vp752_d-1_S...26`,
                           `vp752_d-1_S...27`),
    ec = coalesce(`EC_4F2b1_Sjj_54_SSL_0_0`,
                  `EC_sx_8A3a_Sjj_dS/m_122_CMS_0_0`),
    cec = coalesce(`CECnh4_5A8b_Sjj_cmol(+)/kg_121_CMS_0_0`,
                   `CEC_4B1a1b_Sjj_47_SSL_0_0`,
                   `CEC_CEC1_Sjj_cmol(+)/kg_47_SSL_0_0`,
                   `CEC_4B1a1a_Sjj_47_SSL_0_0`,
                   `CEC_CECd_Sjj_cmol(+)/kg_47_SSL_0_0`),
    ph1to1h2o = coalesce(`pH_h2o_8C1f_Sjj_(NA)_123_CMS_0_0`,
                         `pHh2o_4C1a2a1_Sjj_73_SSL_0_0`,
                         `pHh2o_pHRou1_Sjj_(NA)_73_SSL_0_0`),
    caco3 = coalesce(`CaCO3_4E1a1a1a1_Sjf_40_SSL_0_0`,
                     `CaCO3_6E1g_Sjj_% wt_118_CMS_0_0`,
                     `CaCO3_6E1h_Sjj_% wt_118_CMS_0_0`,
                     `CaCO3_Carb1_Sjf_% wt_40_SSL_0_0`)
    
    ) %>% 
  select(pedon_key,
         layer_key,
         layer_sequence,
         hzn_top,
         hzn_bot,
         hzn_desgn,
         texture_description,
         claytotal,
         silttotal,
         sandtotal,
         dbthirdbar,
         lep, 
         water_ret_diff,
         c_est_org,
         fragvol_sum,
         ec,
         cec,
         ph1to1h2o,
         caco3) %>% 
  # this ensures that horizons are sorted in order by profile for aqp
  arrange(pedon_key, layer_key)



datc_clean <- dat_coalesce %>% 
  filter(!is.na(hzn_top),
         !is.na(hzn_bot),
         # had a number of empty hzns appended to some profiles
         # this drops ~50 rows
         !is.na(hzn_desgn), 
         !(pedon_key == "99P0513" & layer_sequence %in% c(15:17)),
         !(pedon_key == "99P0514" & layer_sequence %in% c(11:13)),
         !(pedon_key == "99P0515" & layer_sequence %in% c(13:15)),
         !(pedon_key == "99P0516" & layer_sequence %in% c(11:13)))

# explainer for pedon_key filtering above: it appears that for pedon_key =
# "99P0513", "99P0514", "99P0515", "99P0516", there were additional samples
# collected at fixed depth intervals, and entered in the database but these do
# not have accompanying data. These overlap with the genetic horizon data, and
# are causing errors downstream when I try to use aqp::slab to calculate my
# depth-weighted means of soil properties for 0-20cm.


# save horizon data------------------------------------------------------------

write_csv(dat_coalesce, "data/validation_data/NCSS-KSSL/validation_ncss_kssl_horizon_data_clean.csv")
    


# calc 0-20cm avgs --------------------------------------------------------

# better name in this context
profiles <- dat_coalesce

# create aqp object

aqp::depths(profiles) <- pedon_key ~ hzn_top + hzn_bot

aqp::hzdesgnname(profiles) <- "hzn_desgn"

class(profiles)

# note after dropping pedons w/ NA horizon data,
# we now have 49 profiles
print(profiles)

# custom slab function I can use w/ map to calc weighted means 
# for the soil properties of interest 

slab_fun <- function(var, spc_obj){
  
  slab_formula <- as.formula(paste("pedon_key ~ ", var))
  
  slab_df <- aqp::slab(object = spc_obj, 
                  fm = slab_formula,
                  slab.structure = c(0,20),
                  slab.fun = mean,
                  # keeping this "false" for now b/c I have so much
                  # missing data, want to know how many "complete" 
                  # profiles I have for each soil property
                  na.rm = FALSE,
                  strict = TRUE) 
  
  return(slab_df)
  
}

soil_vars <- c("claytotal", "silttotal", "sandtotal",
               "dbthirdbar", "lep", "water_ret_diff",
               "c_est_org", "fragvol_sum",
               "ec", "cec", "ph1to1h2o", "caco3")

# # dataframe for results to land in
r <- data.frame(var_name = soil_vars)

# map over my list of vars, using custom slab function
# result is a list col with soil prop data nested 
rnest <- r %>% 
  dplyr::mutate(aggr_data = map(.x = var_name,
                                .f = slab_fun, 
                                spc_obj = profiles))

# pivot wider so soil props are in separate columns 
dat_20cm <- bind_rows(rnest$aggr_data) %>% 
  select(-contributing_fraction) %>% 
  pivot_wider(names_from = variable,
              values_from = value)

# add in the site and pedon data (which contains the lat/long)

ncss_final <- left_join(dat_20cm, site_ped_nodup, by = "pedon_key")


# save averaged (0-20cm) ncss validation data ----------------------------------

write_csv(ncss_final, "data/validation_data/NCSS-KSSL/validation_ncss_kssl_0-20cm_aggr.csv")

