# goal: make a map for the manuscript to display the AOI and extent of the MLRAs


# setup -------------------------------------------------------------------


library(tidyverse)
library(sf)
library(ggpattern)

mlras_clipped <- st_read("_qgis_files/aoi_shapefiles_from_r/mn_mlras_clipped.shp") %>% 
 # mutate(MLRA_NAME = str_wrap(MLRA_NAME, width = 20)) %>% 
  mutate(MLRA_NAME = case_when(
    str_detect(MLRA_NAME, "Red River") ~ "g. Red River Valley\nof the North",
    str_detect(MLRA_NAME, "Rolling Till") ~ "h. Rolling Till Prairie",
    str_detect(MLRA_NAME, "Central Minnesota") ~ "b. Central MN\nSandy Outwash",
    str_detect(MLRA_NAME, "Northern Minnesota") ~ "d. Northern MN\nGray Drift",
    str_detect(MLRA_NAME, "Central Iowa") ~ "a. Central IA and\nMN Till Prairies",
    str_detect(MLRA_NAME, "Mississippi") ~ "e. Northern Mississippi\nValley Loess Hills",
    str_detect(MLRA_NAME, "Eastern Iowa") ~ "c. Eastern IA and\nMN Till Prairies"
  ))

mlras_extended <- st_read("_qgis_files/aoi_shapefiles_from_r/mn_mlras_extended.shp")

# State boundary
mn <- st_read("_qgis_files/shp_mn_state_boundary/mn_state_boundary.shp")

map_colors <- c(
    "g. Red River Valley\nof the North" = '#e41a1c',            
    "h. Rolling Till Prairie" = '#377eb8',                    
    "b. Central MN\nSandy Outwash" = '#4daf4a',         
    "d. Northern MN\nGray Drift" = '#984ea3',           
    "h. Rolling Till Prairie" = '#ff7f00',                     
    "d. Northern MN\nGray Drift" = '#ffff33',           
    "a. Central IA and\nMN Till Prairies" = '#a65628',
    "e. Northern Mississippi\nValley Loess Hills"  = '#f781bf',
    "c. Eastern IA and\nMN Till Prairies" = '#fdbf6f' ,
    "f. Not included" = 'lightgrey'
  ) 
  

label_text <- c("a", "c", "e", "d", "b", "h", "b", "h", "g", "f", "f")


label_x <- c(-94.5, # C IA MN Till
             -92.75, # E IA and MN Till
             -92, # N Miss Loess
             -94.6, # N MN Gray Drift
             -94.7, #C MN Sandy Outwash
             -95.8, # Rolling Till
             -95.8, # C MN Sandy 2
             -96.1, # Rolling till 2
             -96.5, # RRV
             -96.15, # Not incl 1
             -93.5 # Not incl 2
              )

label_y <- c(44.5,
             44, 
             44,
             45.9,
             46.5,
             45.8,
             46.6,
             44.3, 
             48,
             43.7, 
             47.5
             )

label_df <- data.frame(labelx = label_x, 
                       labely = label_y, 
                       labeltxt = label_text)

mlra_mn_map <- ggplot() +
  geom_sf(data = mn, aes(fill = "f. Not included")) +
  geom_sf(data = mlras_clipped,
          (aes(fill = MLRA_NAME))) +
  theme_bw() +
  scale_fill_manual(values = map_colors, name = "MLRA Name") +
  theme(legend.text = element_text(size = 8),
        legend.spacing.y = unit(0.3, 'cm')) +
  guides(fill = guide_legend(byrow = TRUE)) +
  annotate(geom = "text", x = label_x, y = label_y, label = label_text) +
  xlab("") +
  ylab("")

ggsave(filename = "figs/mlra_mn_map.png", plot = mlra_mn_map, width = 5, height = 4, units = "in")



# try pattern & BW plot --------------------------------------------------------

bw_map_colors <- c(
  "Red River Valley of\nthe North" = '#ffffff',            
  "Rolling Till Prairie" = '#000000',                    
  "Central Minnesota\nSandy Outwash" = '#d9d9d9',         
  "Northern Minnesota\nGray Drift" = '#bdbdbd',           
  "Rolling Till Prairie" = '#969696',                     
  "Northern Minnesota\nGray Drift" = '#737373',           
  "Central Iowa and\nMinnesota Till\nPrairies" = '#525252',
  "Northern Mississippi\nValley Loess Hills"  = '#f0f0f0',
  "Eastern Iowa and\nMinnesota Till\nPrairies" = '#252525' 
)

ggplot() +
  # geom_sf_pattern(data = mn,
  #                 aes(fill = "Not included"),
  #                 pattern_alpha = 0.5) +
  geom_sf(data = mlras_clipped,
                  aes(fill = MLRA_NAME)) +
  scale_fill_manual(values = bw_map_colors, name = "MLRA Name")+
  theme_bw() +
  theme(legend.text = element_text(size = 8),
        legend.spacing.y = unit(0.3, 'cm')) +
  guides(fill = guide_legend(byrow = TRUE))


# on 2023-12-10 make a simple MN boundary map -----------------------------

mn_border_simple <- ggplot() +
  geom_sf(data = mn, fill = "white", alpha = 0.1) +
  theme_bw() +
  theme(axis.text = element_text(size = 7))

ggsave(plot = mn_border_simple, filename = "figs/bw_mn_border_map.png",
       height = 3, width = 2, units = "in")
