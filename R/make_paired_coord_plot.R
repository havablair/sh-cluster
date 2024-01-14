library(tidyverse)
library(GGally) # for ggparcoor

# our fitted models, created in chap 11
load("data/fitted_kmeans_mods.RData")

# extract centroids
cent_df <- mods %>% 
  mutate(centroids = map(km_fit, ~extract_centroids(.x))) %>% 
  select(n_clust, centroids) %>% 
  unnest(centroids) 

colnames(cent_df) <- c("n_clust", ".cluster", "Clay", "OM",
                       "CEC", "BD", "EC", "pH", "CaCO3",
                       "LEP", "Ksat", "AWC")

# I liked the default colors from ggradar, so I pulled them
# to use in the rest of the plots too 
plot_colors <- c("#FF5A5F", "#FFB400", 
                 "#007A87", "#8CE071", "#7B0051", 
                 "#00D1C1", "#FFAA91", "#B4A76C", 
                 "#9CA299", "#565A5C", "#00A04B", 
                 "#E54C20")

plot_linetypes <- c()

# might come back later and try to adjust linetypes to make this
# plot more readable, but not sure it's a good candidate anyway
# for publication, might be able to find better ways to show this info 

# scale_type should be either: 
## - "uniminmax" : scale so min 0, max 1
## - "globalminmax" : no scaling, (but consider that my data is already
##    standardized)

parcoor_plot <- function(nclust, scale_type){
  
  cent_df %>% 
    filter(n_clust == nclust) %>% 
    ggparcoord(columns = c(3:12),
               groupColumn = '.cluster',
               scale = scale_type,
               showPoints = TRUE) +
            #mapping = ggplot2::scale_linetype_manual()) +
    theme_bw() + 
    scale_color_discrete(type = plot_colors) +
    ggtitle(glue("{nclust} Clusters")) + 
    #guides(color = 'none') +
    geom_vline(xintercept = 0, color = "black") 

  
}


