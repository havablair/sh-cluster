# goal: make a small table to show cluster area in hectares, % AOI occupied
# by each cluster, number of unique MUKEYs in each cluster

# setup -------------------------------------------------------------------

library(tidyverse)
library(glue)
library(gt)


mu_info <- read_csv("data/target_mapunit_table.csv")

# mukeys, cluster assignments, and soil properties 
# note these are the untransformed soil prop values
prop <- read_csv("data/pca_mukey_cluster_assignments_and_soilprops.csv")

# weights are based on the total area of a given map unit in our AOI. 
# these are pixel counts. Since pixels are a uniform size, the count
# is directly proportional to area. 1 pixel is 10m x 10m or 100m^2, 
# so we could multiply by 100 to get m^2. Recall that 1ha = 10,000 m^2
mu_wts <- read.delim("data/gSSURGO_MN/mukey_new_crosswalk.txt", sep = ",") %>% 
  select(MUKEY, MUKEY_New, Count)

val_pts <- read_csv("data/validation_data_pca_clusters_and_soil_props.csv")

comps <- read_csv("data/component_list.csv")

d_wts <- left_join(prop, mu_wts, by = c("mukey" = "MUKEY"))

mukeys_cts <- d_wts %>% select(mukey, Count, k_8)


# generate table of top series, taxonomic groups (based on area)  --------

# need to pull in comp table here b/c that's where the 
# taxonomic level info is located
comps_incl <- comps %>% filter(mukey %in% mukeys_cts$mukey)

comps_wts <- left_join(comps_incl, mukeys_cts, by = "mukey") %>% 
  # convert percent to proportion, multiply by number of mukey 
  # pixels ("Count"). Gives us approx area per component
  # across the AOI. Will group by cluster to identify the 
  # top 4-5 components by area to include in my results/discussion
  mutate(comp_count = Count*(comppct_r/100))

comp_8 <- comps_wts %>%
  group_by(k_8) %>%
  slice_max(order_by = comp_count,
            n = 8)

# identify the columns that are entirely NA
# (so we can drop them)
cols_all_na <- comp_8 %>% 
  ungroup() %>% 
  summarise(across(.cols = everything(), .fns = ~sum(is.na(.x)))) %>% 
  pivot_longer(cols = everything(), names_to = "col_name", values_to = "n_na") %>% 
  filter(n_na == 40) %>% 
  pull(col_name)

# drop the all-NA cols
comp_top8 <- comp_8 %>% 
  select(-all_of(cols_all_na)) %>% 
  select(k_8, mukey, cokey, mukey_count = Count, comp_count, compname, everything())

top8_summary_tbl <- comp_top8 %>% 
group_by(k_8) %>% 
  summarise(across(.cols = c(compname, taxorder, taxsubgrp, drainagecl, geomdesc),
                   .fns = ~str_c(unique(.x), collapse = ", ")))

write_csv(top8_summary_tbl, "data/top8_comps_perclust_by_area.csv")


# cluster summary table ---------------------------------------------------

clust_areas <- d_wts %>% 
  group_by(k_8) %>% 
  summarise(clust_pix_count = sum(Count),
            clust_mukey_count = n()) %>% 
  mutate(clust_area_m2 = clust_pix_count * 100,
         clust_area_ha = round(clust_area_m2 / 10000, digits = 0),
         aoi_area_ha = sum(clust_area_ha),
         clust_percent_area = round((clust_area_ha/aoi_area_ha)*100, digits = 1)) 

val_pt_count <- val_pts %>% 
  select(val_unit_id, k_8) %>% 
  drop_na() %>% 
  count(k_8) %>% 
  filter(k_8 != 0) %>% 
  add_row(k_8 = 8, n = 0L) %>% 
  mutate(k_8 = glue("Cluster_{k_8}")) %>% 
  rename(n_val = n)

clust_summary <- left_join(clust_areas, val_pt_count, by = "k_8")

# might consider updating these descriptions after doing the spatially
# weighted violin plots in "make_violin_plots_soil_props.R" Trying to figure
# out if it makes sense to reference specific ranges (like for CaCO3, EC)
# for context in the descriptions. 
clust_area_tbl <- clust_summary %>% 
  select(k_8, clust_mukey_count, clust_area_ha, clust_percent_area, n_val) %>% 
  rename(Cluster = k_8,
         "Mapunits (n)" = clust_mukey_count,
         "Total Area (ha)" = clust_area_ha,
         "% of AOI (area)" = clust_percent_area,
         "Validation points (n)" = n_val
         ) %>% 
  mutate(Cluster = str_replace(Cluster, "Cluster_", ""),
         Description = case_when(
           Cluster == "1" ~ "Coarsest, slightly acid, lowest OM",
           Cluster == "2" ~ "Loamy, neutral, moderate OM",
           Cluster == "3" ~ "Coarse loamy, low-moderate OM, neutral",
           Cluster == "4" ~ "Loamy to clayey, slightly alkaline, highest OM, 11% CaCO3",
           Cluster == "5" ~ "Highest clay, neutral, high OM",
           Cluster == "6" ~ "Loamy, slightly alkaline, moderate OM, 9% CaCO3, EC 1-2 dS/m",
           Cluster == "7" ~ "Loamy, slightly alkaline, moderate OM, 8% CaCO3",
           Cluster == "8" ~ "Histosols & intergrades (organic soils)"
         )) %>% 
  select(Cluster, Description, everything()) 

# Define a named list of aggregation
# functions and summary row labels
fns_labels <- 
  list(
    Total = ~sum(., na.rm = TRUE)
  )
  
clust_tbl_formatted <- clust_area_tbl %>% 
  gt() %>% 
  tab_header(
    title = md("**Cluster Summary**")
  ) %>% 
  cols_align(
    align = "center",
    columns = Cluster
  ) %>% 
  fmt_number(
    columns = c("Total Area (ha)", "Mapunits (n)"),
    use_seps = TRUE,
    sep_mark = ",",
    decimals = 0
  ) %>% 
  fmt_percent(
    columns = "% of AOI (area)", 
    decimals = 1, 
    scale_values = FALSE
  ) %>% 
  grand_summary_rows(
    columns = c("Mapunits (n)", "Total Area (ha)", "% of AOI (area)", "Validation points (n)"),
    fns = fns_labels,
    fmt = ~fmt_number(., decimals = 0)
  ) 


gtsave(clust_tbl_formatted, "figs/cluster_summary_tbl.png", zoom = 1.5)

# note: when saving as word table, had to do some hand-editing because 
# the (empty?) stub seems to be throwing things off, the column 
# labels were shifted over 
gtsave(clust_tbl_formatted, "figs/cluster_summary_tbl.docx")


# weighted soil props table -----------------------------------------------

wtd_mean_tbl <- d_wts %>% 
  select(mukey:awc, k_8, Count) %>% 
  group_by(k_8) %>% 
  summarise(across(.cols = c(claytotal, om, cec7, dbthirdbar, ec, 
                             ph1to1h2o, caco3, lep, ksat, awc),
                   .fns = list(mean = mean, 
                               sd = sd))) %>% 
  rename_with(.cols = contains("_mean"), .fn = ~str_replace(.x, "_mean", "")) %>% 
  mutate(k_8 = str_replace(k_8, "Cluster_", "")) %>% 
  select(k_8, contains("clay"), contains("ph"), everything()) %>% 
  gt(rowname_col = "k_8") %>% 
  tab_stubhead(label = html("<b>Cluster</b><br>")) %>% 
  fmt_number(columns = c(contains("claytotal"), contains("cec7"), contains("lep"),
                         contains("ksat")),
             decimals = 0) %>% 
  fmt_number(columns = c(contains("om"), contains("dbthirdbar"),
                         contains("ph1to1h2o"), contains("ec"), contains("caco3")),
             decimals = 1) %>% 
  fmt_number(columns = contains("awc"),
             decimals  = 2) %>% 
  cols_merge_uncert(col_val = claytotal,
                    col_uncert = claytotal_sd) %>% 
  cols_merge_uncert(col_val = ph1to1h2o,
                    col_uncert = ph1to1h2o_sd) %>% 
  cols_merge_uncert(col_val = om,
                    col_uncert = om_sd) %>% 
  cols_merge_uncert(col_val = cec7,
                    col_uncert = cec7_sd) %>% 
  cols_merge_uncert(col_val = dbthirdbar,
                    col_uncert = dbthirdbar_sd) %>% 
  cols_merge_uncert(col_val = ec,
                    col_uncert = ec_sd) %>% 
  cols_merge_uncert(col_val = caco3,
                    col_uncert = caco3_sd) %>% 
  cols_merge_uncert(col_val = lep,
                    col_uncert = lep_sd) %>%
  cols_merge_uncert(col_val = ksat,
                    col_uncert = ksat_sd) %>% 
  cols_merge_uncert(col_val = awc,
                    col_uncert = awc_sd) %>% 
  cols_label(claytotal = html("<b>Clay</b><br>(%)"),
             om = html("<b>OM</b><br>(%)"), 
             cec7 = html("<b>CEC</b><br>(meq/100g)"),
             dbthirdbar = html("<b>BD</b><br>(g/cm^3)"),
             ec = html("<b>EC</b><br>(dS/m)"), 
             ph1to1h2o = html("<b>pH</b><br>(1:1 H2O)"), 
             caco3 = html("<b>CaCO3</b><br>(%)"),
             lep = html("<b>LEP</b><br>(%)"), 
             ksat = html("<b>Ksat</b><br>(um/s)"), 
             awc = html("<b>AWC</b><br>(vol. fraction)")) %>% 
  cols_align(align = "center") 

gtsave(wtd_mean_tbl, "figs/cluster_props_weighted_means_tbl.png")


# weighted means tbl for word ---------------------------------------------

# the only thing that's different between this one and the one above is 
# the elimination of the HTML formatting I added to the column labels
# so that it exports nicely to word doc for publication.

word_wtd_mean_tbl <- d_wts %>% 
  select(mukey:awc, k_8, Count) %>% 
  group_by(k_8) %>% 
  summarise(across(.cols = c(claytotal, om, cec7, dbthirdbar, ec, 
                             ph1to1h2o, caco3, lep, ksat, awc),
                   .fns = list(mean = mean, 
                               sd = sd))) %>% 
  rename_with(.cols = contains("_mean"), .fn = ~str_replace(.x, "_mean", "")) %>% 
  mutate(k_8 = str_replace(k_8, "Cluster_", "")) %>% 
  select(k_8, contains("clay"), contains("ph"), everything()) %>% 
  gt(rowname_col = "k_8") %>% 
  tab_stubhead(label = html("Cluster")) %>% 
  fmt_number(columns = c(contains("claytotal"), contains("cec7"), contains("lep"),
                         contains("ksat")),
             decimals = 0) %>% 
  fmt_number(columns = c(contains("om"), contains("dbthirdbar"),
                         contains("ph1to1h2o"), contains("ec"), contains("caco3")),
             decimals = 1) %>% 
  fmt_number(columns = contains("awc"),
             decimals  = 2) %>% 
  cols_merge_uncert(col_val = claytotal,
                    col_uncert = claytotal_sd) %>% 
  cols_merge_uncert(col_val = ph1to1h2o,
                    col_uncert = ph1to1h2o_sd) %>% 
  cols_merge_uncert(col_val = om,
                    col_uncert = om_sd) %>% 
  cols_merge_uncert(col_val = cec7,
                    col_uncert = cec7_sd) %>% 
  cols_merge_uncert(col_val = dbthirdbar,
                    col_uncert = dbthirdbar_sd) %>% 
  cols_merge_uncert(col_val = ec,
                    col_uncert = ec_sd) %>% 
  cols_merge_uncert(col_val = caco3,
                    col_uncert = caco3_sd) %>% 
  cols_merge_uncert(col_val = lep,
                    col_uncert = lep_sd) %>%
  cols_merge_uncert(col_val = ksat,
                    col_uncert = ksat_sd) %>% 
  cols_merge_uncert(col_val = awc,
                    col_uncert = awc_sd) %>% 
  cols_label(claytotal = html("Clay (%)"),
             om = html("OM (%)"), 
             cec7 = html("CEC (meq/100g)"),
             dbthirdbar = html("BD (g/cm^3)"),
             ec = html("EC (dS/m)"), 
             ph1to1h2o = html("pH (1:1 H2O)"), 
             caco3 = html("CaCO3 (%)"),
             lep = html("LEP (%)"), 
             ksat = html("Ksat (um/s)"), 
             awc = html("AWC (vol. fraction)")) %>% 
  cols_align(align = "center") 

gtsave(word_wtd_mean_tbl, "figs/cluster_props_weighted_means_tbl.docx")



# curious about 20 MUKEYs with largest area / most pixels in AOI ----------


top20 <- d_wts %>% filter(Count > 2000000) %>% 
  arrange(desc(Count)) %>% 
  slice_head(n = 20)


top20_info <- mu_info %>% 
  filter(mukey %in% top20$mukey) 
