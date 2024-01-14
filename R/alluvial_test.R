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


d <- read_csv("data/mukey_cluster_assignments_and_soilprops.csv") %>% 
  select(mukey, k_6, k_7, k_8, k_9, k_10, k_11)

cluster_names <- read_csv("data/named_clustering_candidates.csv") %>% 
  mutate(label_names = glue("{cluster_id} {description}"),
         wrap_names = str_wrap(label_names, 25))


# data wrangling ----------------------------------------------------------


freqs <- d %>% 
  select(mukey, k_6, k_7, k_8, k_9, k_10, k_11) %>% 
  count(k_6, k_7, k_8, k_9, k_10, k_11, .drop = FALSE) 

# need separate dfs for each clustering so my left_join scheme below
# has the desired behavior 
k6_names <- cluster_names %>% 
  filter(k == 6) %>% 
  select(join_cluster, wrap_names)

k7_names <- cluster_names %>% 
  filter(k == 7) %>% 
  select(join_cluster, wrap_names)

k8_names <- cluster_names %>% 
  filter(k == 8) %>% 
  select(join_cluster, wrap_names)

k9_names <- cluster_names %>% 
  filter(k == 9) %>% 
  select(join_cluster, wrap_names)

k10_names <- cluster_names %>% 
  filter(k == 10) %>% 
  select(join_cluster, wrap_names)

k11_names <- cluster_names %>% 
  filter(k == 11) %>% 
  select(join_cluster, wrap_names)

# progressively adding the long descriptive labels
freqs6 <- left_join(freqs, k6_names, by = c("k_6" = "join_cluster")) %>% 
  rename(k6_desc = wrap_names)

freqs7 <- left_join(freqs6, k7_names, by = c("k_7" = "join_cluster")) %>% 
  rename(k7_desc = wrap_names)

freqs8 <- left_join(freqs7, k8_names, by = c("k_8" = "join_cluster")) %>% 
  rename(k8_desc = wrap_names)

freqs9 <- left_join(freqs8, k9_names, by = c("k_9" = "join_cluster")) %>% 
  rename(k9_desc = wrap_names)

freqs10 <- left_join(freqs9, k10_names, by = c("k_10" = "join_cluster")) %>% 
  rename(k10_desc = wrap_names)

freqs11 <- left_join(freqs10, k11_names, by = c("k_11" = "join_cluster")) %>% 
  rename(k11_desc = wrap_names)

# k=6-11 alluvial plot ----------------------------------------------------

alluv6_11 <-
  ggplot(data = freqs11, aes(
    y = n,
    axis1 = k6_desc,
    axis2 = k7_desc,
    axis3 = k8_desc, 
    axis4 = k9_desc,
    axis5 = k10_desc,
    axis6 = k11_desc
  )) +
  geom_alluvium(aes(fill = k6_desc), show.legend = FALSE) +
  geom_stratum(width = 1 / 3,
               fill = "black",
               color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  scale_x_discrete(
    limits = c("k=6", "k=7", "k=8", "k=9", "k=10", "k=11"),
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

ggsave("_refs/alluvial_k6-11.pdf", width = 20, height = 18, units = "in")



# k=6-8 alluvial plot -----------------------------------------------------

alluv6_8 <-
  ggplot(data = freqs11, aes(
    y = n,
    axis1 = k6_desc,
    axis2 = k7_desc,
    axis3 = k8_desc
  )) +
  geom_alluvium(aes(fill = k6_desc), show.legend = FALSE) +
  geom_stratum(width = 1 / 3,
               fill = "black",
               color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(
    limits = c("k=6", "k=7", "k=8"),
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

ggsave("_refs/alluvial_k6-8.pdf", width = 11.5, height = 8, units = "in")

# k=8-11 alluvial plot ----------------------------------------------------


alluv8_11 <-
  ggplot(data = freqs11, aes(
    y = n,
    axis1 = k8_desc,
    axis2 = k9_desc,
    axis3 = k10_desc, 
    axis4 = k11_desc
  )) +
  geom_alluvium(aes(fill = k8_desc), show.legend = FALSE) +
  geom_stratum(width = 1 / 4,
               fill = "black",
               color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  scale_x_discrete(
    limits = c("k=8", "k=9", "k=10", "k=11"),
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

ggsave("_refs/alluvial_k8-11.pdf", width = 20, height = 18, units = "in")

