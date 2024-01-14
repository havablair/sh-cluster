# make a special color-coded table to serve as map legend for the figure 
# that displays full MN cluster results in color, plus three insets

library(tidyverse)
library(gt)


grp_desc <- data.frame(Cluster = as.character(c(1:8)), 
                       empty = "  ") %>% 
  mutate( Description = case_when(
    Cluster == "1" ~ "Coarsest, slightly acid, lowest OM",
    Cluster == "2" ~ "Loamy, neutral, moderate OM",
    Cluster == "3" ~ "Coarse loamy, low-moderate OM, neutral",
    Cluster == "4" ~ "Loamy to clayey, slightly alkaline, highest OM, 11% CaCO3",
    Cluster == "5" ~ "Highest clay, neutral, high OM",
    Cluster == "6" ~ "Loamy, slightly alkaline, moderate OM, 9% CaCO3, EC 1-2 dS/m",
    Cluster == "7" ~ "Loamy, slightly alkaline, moderate OM, 8% CaCO3",
    Cluster == "8" ~ "Histosols & intergrades (organic soils)") ) 

clust_colors <- c("1" = "#FF5A5F", "2" = "#FFB400", "3" = "#007A87" , "4" = "#8CE071",
                  "5" = "#7B0051", "6" = "#00D1C1", "7" = "#FFAA91", "8" = "#B4A76C")

legend_tbl_gt <- grp_desc %>% 
  gt() %>% 
  tab_style(
    style = cell_fill(color = "#FF5A5F"),
    locations = cells_body(
      columns = empty,
      rows = Cluster == "1"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#FFB400"),
    locations = cells_body(
      columns = empty,
      rows = Cluster == "2"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#007A87"),
    locations = cells_body(
      columns = empty,
      rows = Cluster == "3"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#8CE071"),
    locations = cells_body(
      columns = empty,
      rows = Cluster == "4"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#7B0051"),
    locations = cells_body(
      columns = empty,
      rows = Cluster == "5"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#00D1C1"),
    locations = cells_body(
      columns = empty,
      rows = Cluster == "6"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#FFAA91"),
    locations = cells_body(
      columns = empty,
      rows = Cluster == "7"
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = "#B4A76C"),
    locations = cells_body(
      columns = empty,
      rows = Cluster == "8"
    )
  ) %>% 
  cols_label(
    empty = "____"
  ) %>% 
  tab_options(table.font.size = pct(130),
              data_row.padding = px(3),
              # transparent background color
              table.background.color = "#FFFFFF00")

gtsave(legend_tbl_gt, filename = "figs/legend_tbl_color_wider2.png", vwidth = 600, vheight = 170, expand = 15)  

