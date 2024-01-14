library(tidyverse)
library(glue)
library(gt)
library(patchwork)

orig_comps <- read_csv("data/original_pairwise_comparisons.csv")

pca_comps <- read_csv("data/pca_pairwise_results_all.csv") %>% 
  rename(var_name = var_names) %>% 
  mutate(var_name = case_when(
    str_detect(var_name, "clay") ~ "Clay",
    str_detect(var_name, "om") ~ "OM",
    str_detect(var_name, "cec7") ~ "CEC",
    str_detect(var_name, "caco3") ~ "CaCO3",
    str_detect(var_name, "db") ~ "BD",
    str_detect(var_name, "_ec") ~ "EC",
    str_detect(var_name, "ph") ~ "pH",
    str_detect(var_name, "lep") ~ "LEP",
    str_detect(var_name, "ksat") ~ "Ksat",
    str_detect(var_name, "awc") ~ "AWC"
  ))

# when calculating the number of "possible" pairwise contrasts for the 
# validation dataset, need to account for the fact that we don't have 
# validation points in every cluster. For cases when there are 0 or 1 
# validation points per cluster, we can't do a pairwise t-test. As a result
# there are fewer possible comparisons by virtue of data availability. 
# adjusting for that here, based on information from the "validation
# point count" bar plot in chapter 25 PCA cluster pairwise comparison
pca_summary <- read_csv("data/pca_pairwise_comparisons_summary.csv") %>% 
  mutate(num_valid_regions = case_when(
    num_regions %in% c(6, 7, 8) ~ num_regions-1,
    num_regions %in% c(9, 10) ~ num_regions-2,
    num_regions %in% c(11) ~ num_regions-3,
    TRUE ~ num_regions
  ),
  poss_valid_comps = (num_valid_regions * (num_valid_regions - 1)) / 2,
  perc_valid_comps = round((n_sig_comps/poss_valid_comps)*100, digits = 0))

valid_comps_summary <- pca_summary %>%
  filter(cluster_opts %in% c("k_4", "k_6", "k_8")) %>%
  select(cluster_opts, var_names, perc_valid_comps)

valid_comps_summary %>% ggplot(aes(x = cluster_opts,
                                   y = perc_valid_comps,
                                   color = var_names, 
                                   group = var_names)) +
  geom_point() +
  geom_line() +
  theme_minimal()


# orig_summary <- orig_comps %>% 
#   filter(cluster_opts %in% c("k_6", "k_9", "k_11")) %>% 
#   group_by(cluster_opts) %>% 
#   summarise(across(.cols = c(n_sig_pair_diffs, possible_comps),
#                    .fns = c("sum" = sum),
#                    .names = "{.fn}_{.col}")) %>% 
#   mutate(percent_sig = (sum_n_sig_pair_diffs/sum_possible_comps)*100,
#          version = "orig")
# 
# pca_summary <- pca_comps %>% 
#   filter(cluster_opts %in% c("k_6", "k_8", "k_11")) %>% 
#   group_by(cluster_opts) %>% 
#   summarise(across(.cols = c(n_sig_pair_diffs, possible_comps),
#                    .fns = c("sum" = sum),
#                    .names = "{.fn}_{.col}")) %>% 
#   mutate(percent_sig = (sum_n_sig_pair_diffs/sum_possible_comps)*100,
#          version = "pca")



possible_per_var <- pca_summary %>% select(cluster_opts, possible_comps, num_regions) %>% 
  distinct() %>% 
  filter(num_regions %in% c(4:9))


possible_edit <- pca_summary %>% 
  select(cluster_opts, poss_valid_comps, num_regions) %>%
  filter(num_regions %in% c(4:9))


# compare PCA k_6 and k_8 pairwise comps ----------------------------------

# comps_tbl <- pca_comps %>% 
#   group_by(num_regions) %>% 
#   summarise(across(.cols = c(n_sig_pair_diffs, possible_comps),
#                    .fns = sum)) %>% 
#   rename(sig_comps = n_sig_pair_diffs) %>% 
#   mutate(percent_sig = round((sig_comps/possible_comps)*100, digits = 0),
#          alpha_5perc = round(possible_comps*0.05)) 
# 
# comps_tbl
# 
# k8_pairs <- pca_comps %>% filter(cluster_opts == "k_8") %>% 
#   mutate(perc_sig = round((n_sig_pair_diffs/possible_comps)*100, digits = 0)) %>% 
#   select(-c(cluster_opts, num_regions, possible_comps)) %>% 
#   rename(n_sig_diffs = n_sig_pair_diffs) %>% 
#   mutate(vers = "k8")
# 
# k6_pairs <- pca_comps %>% filter(cluster_opts == "k_6") %>% 
#   mutate(perc_sig = round((n_sig_pair_diffs/possible_comps)*100, digits = 0)) %>% 
#   select(-c(cluster_opts, num_regions, possible_comps)) %>% 
#   rename(n_sig_diffs = n_sig_pair_diffs) %>% 
#   mutate(vers = "k6")
# 
# k6_k8_pairs <- bind_rows(k6_pairs, k8_pairs)


# plot --------------------------------------------------------------------



pairwise_possible_plot <- pca_summary %>% 
  mutate(var_names = case_when(
    var_names == "claytotal" ~ "Clay",
    var_names == "caco3" ~ "CaCO3",
    var_names == "dbthirdbar" ~ "Bulk Density",
    var_names == "om_loi" ~ "Organic Matter",
    var_names == "ph1to1h2o" ~ "pH"
  )) %>% 
  filter(num_regions %in% c(4:9)) %>% 
  ggplot() +
  geom_point(aes(x = num_regions,
                 y = n_sig_comps,
                 color = var_names),
             show.legend = FALSE) +
  geom_line(aes(x = num_regions,
                y = n_sig_comps,
                color = var_names),
            show.legend = FALSE) + 
  geom_point(data = possible_per_var,
             aes(x = num_regions, y = possible_comps),
             show.legend = FALSE) +
  geom_line(data = possible_per_var,
            aes(x = num_regions, y = possible_comps,
                lty = "possible comparisons\n (1 variable)"),
            show.legend = FALSE) + 
  geom_text(aes(x = 8.5, y = 18, label = "Clay"), color = "#00BF7D", size = 3) +
  geom_text(aes(x = 8.5, y = 12, label = "pH"), color = "#E76BF3", size = 3) +
  geom_text(aes(x = 8.5, y = 7, label = "CaCO3"), color = "#A3A500", size = 3) +
  geom_text(aes(x = 7.5, y = 4.8, label = "Bulk density"), color = "#F8766D",
            angle = 8, size = 3) +
  geom_text(aes(x = 8.5, y = 4, label = "OM"), color = "#00B0F6", size = 3) +
  geom_text(aes(x = 8.1, y = 35, label = "n possible"), color = "black", size = 3) +
 # theme_minimal() +
  xlab("Clusters (n)") +
  ylab("Significant contrasts (n)") + 
 # ggtitle("Pairwise comparisons by soil property") +
  scale_x_continuous(breaks = c(4:9)) +
  scale_linetype('') +
  scale_color_discrete('Soil property') +
  theme_bw() + 
  theme(panel.grid.major = element_line(color = "gray75", linewidth = 0.3),
        panel.grid.minor = element_line(color = "gray85"))



ggsave("figs/pairwise_comps_by_soil_var.png",
       pairwise_possible_plot,
       width = 5, height = 5, units = "in")

tot_comps <- pca_summary %>%
  filter(num_regions %in% c(4:9)) %>% 
  group_by(cluster_opts, num_regions) %>% 
  summarise(tot_sig_comps = sum(n_sig_comps),
            .groups = "drop") %>% 
  ggplot(aes(x = num_regions, y = tot_sig_comps)) + 
  geom_point() + 
  geom_line() +
 # theme_minimal() +
  xlab("Clusters (n)") +
  ylab("Significant contrasts (n)") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray75", linewidth = 0.3),
        panel.grid.minor = element_line(color = "gray85"))

pairwise_possible_plot + tot_comps

ggsave("figs/combo_pairwise_plot_sep23.png", width = 6, height = 3)


possible_valid_plot <- pca_summary %>% 
  mutate(var_names = case_when(
    var_names == "claytotal" ~ "Clay",
    var_names == "caco3" ~ "CaCO3",
    var_names == "dbthirdbar" ~ "Bulk Density",
    var_names == "om_loi" ~ "Organic Matter",
    var_names == "ph1to1h2o" ~ "pH"
  )) %>% 
  filter(num_regions %in% c(4:9)) %>% 
  ggplot() +
  geom_point(aes(x = num_regions,
                 y = n_sig_comps,
                 color = var_names)) +
  geom_line(aes(x = num_regions,
                y = n_sig_comps,
                color = var_names)) + 
  geom_point(data = possible_edit,
             aes(x = num_regions, y = poss_valid_comps)) +
  geom_line(data = possible_edit,
            aes(x = num_regions, y = poss_valid_comps,
                lty = "possible comparisons\n (1 variable)")) +
  theme_minimal() +
  xlab("Number of Clusters/Regions") +
  ylab("Sig. pairwise contrasts (validation data)") + 
  ggtitle("Pairwise comparisons by soil property") +
  scale_x_continuous(breaks = c(4:9)) +
  scale_linetype('') +
  scale_color_discrete('Soil property') +
  labs(subtitle = "Possible contrasts calculated based on available validation data")

ggsave("figs/valid_comps_by_soil_var.png",
       possible_valid_plot,
       width = 5, height = 5, units = "in")




# summary table formatting ------------------------------------------------

pairwise_summary_tbl <- pca_summary %>% 
  filter(num_regions %in% c(4:9)) %>% 
  group_by(num_regions) %>% 
  summarise(across(.cols = contains("comps"), .fns = sum)) %>% 
  mutate(perc_sig = round((n_sig_comps/possible_comps)*100, digits = 0)) %>% 
  select(-c(possible_comps)) %>% 
  select(num_regions, n_sig_comps, perc_sig) %>% 
  gt(rowname_col = "num_regions") %>% 
  tab_stubhead(label = md("*k* regions")) %>% 
  tab_spanner(label = "Significant pairwise contrasts", columns = c("n_sig_comps", "perc_sig")) %>% 
  cols_label(
    n_sig_comps = md("total (*n*)"),
    perc_sig = "% of possible"
  ) %>% 
  tab_header("Validation data")

gt::gtsave(pairwise_summary_tbl, "figs/pairwise_summary_tbl.png")

pairwise_summary_tbl_poss <- pca_summary %>% 
  filter(num_regions %in% c(4:9)) %>% 
  select(-c(possible_comps, alpha_comps, num_valid_regions, perc_valid_comps)) %>% 
  group_by(num_regions) %>% 
  summarise(across(.cols = contains("comps"), .fns = sum)) %>% 
  mutate(perc_sig = round((n_sig_comps/poss_valid_comps)*100, digits = 0)) %>% 
  select(-c(poss_valid_comps)) %>% 
  select(num_regions, n_sig_comps, perc_sig) %>% 
  gt(rowname_col = "num_regions") %>% 
  tab_stubhead(label = md("*k* regions")) %>% 
  tab_spanner(label = "Significant pairwise contrasts", columns = c("n_sig_comps", "perc_sig")) %>% 
  cols_label(
    n_sig_comps = md("total (*n*)"),
    perc_sig = "% of possible"
  ) %>% 
  tab_header("Validation data") %>% 
  tab_footnote(
    footnote = html("Possible comparisons calculated using number of<br>clusters with available validation data"),
    locations = cells_column_labels(
      perc_sig
    )
  )

gt::gtsave(pairwise_summary_tbl_poss, "figs/pairwise_summary_tbl_poss.png")


# wide soil property table ------------------------------------------------

wide_soil_prop_contrasts <- pca_summary %>%
  filter(num_regions %in% c(4, 6, 8)) %>% 
  select(var_names, cluster_opts, n_sig_comps, possible_comps) %>%
  mutate(perc_sig_comps = round((n_sig_comps/possible_comps)*100, digits = 0)) %>% 
  pivot_wider(names_from = "var_names",
              values_from = contains("comps")) %>%
  select(possible = possible_comps_claytotal, cluster_opts, contains("n_sig_"), contains("perc")) %>%
  rename_with(.cols = contains("n_sig"),
              .fn = ~ str_replace(.x, "n_sig_comps_", "n_")) %>%
  rename_with(.cols = contains("perc"),
              .fn = ~ str_replace(.x, "perc_sig_comps_", "perc")) %>% 
  gt(rowname_col = "cluster_opts") %>%
  tab_spanner(label = "n significant contrasts", columns = contains("n_"))

gtsave(wide_soil_prop_contrasts, "figs/wide_soil_prop_contrasts.png")
  