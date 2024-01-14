## goal is to make an "alluvial plot" or a "parallel set diagram" to represent
## how cluster membership as k (number clusters) increases. 

# helpful definitions, distinctions, examples at this blog post by the
# {ggaluvial} author: https://corybrunson.github.io/2019/09/13/flow-taxonomy/

# intro to {ggaluvial}, which is the package I'd like to try first 

# page for {ggforce}, which includes functions for parallel set diagrams 
# something to try if {ggalluvial} doesn't work out. 
# https://ggforce.data-imaginist.com/reference/geom_parallel_sets.html


# setup -------------------------------------------------------------------



library(tidyverse)
library(glue)
library(ggalluvial)


d_orig <- read_csv("data/mukey_cluster_assignments_and_soilprops.csv") %>% 
  select(mukey, k_6, k_7, k_8, k_11)

d_pca <- read_csv("data/pca_mukey_cluster_assignments_and_soilprops.csv") %>% 
  select(mukey, k_4, k_6, k_7, k_8, k_11) %>% 
  rename(pk_4 = k_4,
         pk_6 = k_6,
         pk_7 = k_7,
         pk_8 = k_8,
         pk_11 = k_11)

d <- left_join(d_orig, d_pca, by = "mukey")

cluster_names <- read_csv("data/named_clustering_candidates.csv") %>% 
  mutate(label_names = glue("{cluster_id} {description} n={n_members}"),
         wrap_names = str_wrap(label_names, 25))


# data wrangling ----------------------------------------------------------

freqs_k6 <- d %>% 
  select(mukey, k_6, pk_6) %>% 
  count(k_6, pk_6, .drop = FALSE) 

freqs_k8 <- d %>% 
  select(mukey, k_8, pk_8) %>% 
  count(k_8, pk_8, .drop = FALSE) 

freqs_k11 <- d %>% 
  select(mukey, k_11, pk_11) %>% 
  count(k_11, pk_11, .drop = FALSE) 

freqs_pca6_8 <- d %>% 
  select(mukey, pk_6, pk_8) %>% 
  count(pk_6, pk_8, .drop = FALSE)

freqs_pca468 <- d %>% 
  select(mukey, pk_4, pk_6, pk_8) %>% 
  count(pk_4, pk_6, pk_8, .drop = FALSE)

# need to do 2 joins to add descriptive names for 
# the pk_6 and pk_8 clusters 

pk6_names <- cluster_names %>% 
  filter(version == "pca",
         k == 6) %>% 
  select(join_cluster, wrap_names)

pk8_names <- cluster_names %>% 
  filter(version == "pca", 
         k == 8) %>% 
  select(join_cluster, wrap_names)

pk4_names <- cluster_names %>% 
  filter(version == "pca", 
         k == 4) %>% 
  select(join_cluster, wrap_names)

temp_names1 <- left_join(freqs_pca468, pk6_names, by = c("pk_6" = "join_cluster")) %>% 
  rename(k6_desc = wrap_names)

pca68_freqs_named <- left_join(temp_names1, pk8_names, 
                               by = c("pk_8" = "join_cluster")) %>% 
  rename(k8_desc = wrap_names)

pca468_freqs_named <- left_join(pca68_freqs_named, pk4_names, 
                                by = c("pk_4" = "join_cluster")) %>% 
  rename(k4_desc = wrap_names)


# Comparing k_6 alluvial plot -----------------------------------------------------

alluv_k6 <-
  ggplot(data = freqs_k6, aes(
    y = n,
    axis1 = k_6,
    axis2 = pk_6,
  )) +
  geom_alluvium(aes(fill = k_6), show.legend = FALSE) +
  geom_stratum(width = 1 / 3,
               fill = "black",
               color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(
    limits = c("original", "pca"),
    expand = c(.05, .05),
    position = "top"
  ) +
  scale_fill_discrete(type = c(
    "#FF5A5F",
             "#FFB400",
             "#007A87",
             "#8CE071",
             "#7B0051",
             "#00D1C1"
  )) +
  ggtitle("Soil Health Regions") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16))

ggsave("_refs/alluvial_compare_k6.pdf", plot = alluv_k6,  width = 8, height = 8, units = "in")



# Comparing k_8 alluvial plot ---------------------------------------------

alluv_k8 <-
  ggplot(data = freqs_k8, aes(
    y = n,
    axis1 = k_8,
    axis2 = pk_8,
  )) +
  geom_alluvium(aes(fill = k_8), show.legend = FALSE) +
  geom_stratum(width = 1 / 3,
               fill = "black",
               color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(
    limits = c("original", "pca"),
    expand = c(.05, .05),
    position = "top"
  ) +
  scale_fill_discrete(type = c(
    "#FF5A5F",
             "#FFB400",
             "#007A87",
             "#8CE071",
             "#7B0051",
             "#00D1C1",
             "#FFAA91",
             "#B4A76C"
  )) +
  ggtitle("Soil Health Regions") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16))

ggsave("_refs/alluvial_compare_k8.pdf", plot = alluv_k8,  width = 8, height = 8, units = "in")


# Comparing k_11 alluvial plot --------------------------------------------

alluv_k11 <-
  ggplot(data = freqs_k11, aes(
    y = n,
    axis1 = k_11,
    axis2 = pk_11,
  )) +
  geom_alluvium(aes(fill = k_11), show.legend = FALSE) +
  geom_stratum(width = 1 / 3,
               fill = "black",
               color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(
    limits = c("original", "pca"),
    expand = c(.05, .05),
    position = "top"
  ) +
  scale_fill_discrete(type = c(
    "#FF5A5F",
             "#FFB400",
             "#007A87",
             "#8CE071",
             "#7B0051",
             "#00D1C1",
             "#FFAA91",
             "#B4A76C",
             "#9CA299",
             "#565A5C",
             "#00A04B"
             
  )) +
  ggtitle("Soil Health Regions") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16))

ggsave("_refs/alluvial_compare_k11.pdf", plot = alluv_k11,  width = 8, height = 13, units = "in")



# Comparing pca k_6 vs. pca k_8 alluvial plot -----------------------------

alluv_pca68 <-
  ggplot(data = pca68_freqs_named, aes(
    y = n,
    axis1 = k6_desc,
    axis2 = k8_desc,
  )) +
  geom_alluvium(aes(fill = pk_6), show.legend = FALSE) +
  geom_stratum(width = 1 / 3,
               fill = "black",
               color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(
    limits = c("PCA 6", "PCA 8"),
    expand = c(.05, .05),
    position = "top"
  ) +
  scale_fill_discrete(type = c(
    "#FF5A5F",
             "#FFB400",
             "#007A87",
             "#8CE071",
             "#7B0051",
             "#00D1C1"
  )) +
  ggtitle("Soil Health Regions") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16))

ggsave("_refs/alluvial_compare_pca6-8.pdf", plot = alluv_pca68,  width = 8, height = 11, units = "in")

ggsave("_refs/png_alluvial_compare_pca6-8.png", plot = alluv_pca68,  width = 8, height = 11, units = "in")

# Comparing pca k_4, k_6, k_8 alluvial plot -----------------------------

alluv_pca468 <-
  ggplot(data = pca468_freqs_named, aes(
    y = n,
    axis1 = k4_desc,
    axis2 = k6_desc,
    axis3 = k8_desc,
  )) +
  geom_alluvium(aes(fill = pk_4), show.legend = FALSE) +
  geom_stratum(width = 1 / 3,
               fill = "black",
               color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(
    limits = c("PCA 4", "PCA 6", "PCA 8"),
    expand = c(.05, .05),
    position = "top"
  ) +
  scale_fill_discrete(type = c(
    "#FF5A5F",
             "#FFB400",
             "#007A87",
             "#8CE071"
  )) +
  ggtitle("Soil Health Regions") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16))

ggsave("_refs/alluvial_compare_pca4-6-8.pdf", plot = alluv_pca468,  width = 8, height = 11, units = "in")

ggsave("_refs/png_alluvial_compare_pca4-6-8.png", plot = alluv_pca468,  width = 8, height = 11, units = "in")
