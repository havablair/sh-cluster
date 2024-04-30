# goal: create scree plot to illustrate variance explained by different PCs


# setup -------------------------------------------------------------------

library(tidyverse)
library(ggrepel)

var_dat <- read_csv("data/pca_variance.csv")

wide_var <- var_dat %>% 
  mutate(terms = str_replace_all(terms, " ", "_")) %>% 
  pivot_wider(names_from = "terms", values_from = "value") %>% 
  select(-id)


# recall that there are multiple "terms" in this variance dataset:
# variance, cumulative variance, percent variance, cumulative percent variance

scree_plot <- wide_var %>% 
  ggplot(aes(x = component, y = variance)) + 
  geom_point(size = 2.5) +
  geom_line() +
  geom_text(aes(x = component+0.3,
                 y = variance + 0.4,
                 label = glue("{round(cumulative_percent_variance, digits = 0)}%")),
             size = 3) + 
  geom_label(aes(x = 4, y = 5, label = "Text labels show\ncumulative % variance"),
             size = 2.5) +
    scale_x_continuous(breaks = c(1:10)) +
  theme_bw() +
  xlab("Principal Component") +
  ylab("Eigenvalue")

ggsave(filename = "figs/dpi300/scree_plot.tiff", plot = scree_plot, width = 4, height = 3, units = "in", dpi = 300)
