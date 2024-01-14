library(readr)
library(dplyr)
library(corrplot)
library(corrr)
library(gt)
library(GGally)


# load data ---------------------------------------------------------------

# this is the pre-processed data (transformed, standardized)
# this datasets is generated in chap 11 section "Pre-process data (recipe)"
d <- read_csv("./data/data_preprocessed_all_var.csv")

forcor <- d %>% 
  select(-mukey)

colnames(forcor) <- c("Clay", "OM", "CEC", "BD", "EC", "pH", "CaCO3",
                      "LEP", "Ksat", "AWC")

# with corrr (tidymodels pkg) -------------------------------------------------

# defaults to pearson , only uses pairwise complete obs

tm_cor <- corrr::correlate(forcor)

# rearranges to put highly correlated variables closer together
# and changes shape to show only lower triangle
cor_prepped <- rearrange(tm_cor, absolute = FALSE) %>% 
  shave()

# rounds and aligns decimals for pretty printing
cor_aligned <- fashion(cor_prepped)

# gt_cor_tbl <- 
  
  cor_aligned %>%
  gt() %>%
  opt_stylize(style = 5, color = "gray") 
  

gtsave(gt_cor_tbl, filename = "figs/pearsons_corr_matrix.png", expand = 15)



# pairs plot with GGally --------------------------------------------------

# takes a minute or so to run
#ggpairs(forcor)

# trying different plots --------------------------------------------------
# testing two different functions for making a correlation matrix plots:
# autoplot() and rplot()
test_autoplot <- autoplot(tm_cor)

test_rplot <- corrr::rplot(cor_prepped, print_cor = TRUE)


# with corrplot -----------------------------------------------------------

mx <- as.matrix(forcor)

x_cor <- cor(mx, use = "pairwise.complete.obs")

# default behavior is to omit nas
# check that here
getOption("na.action")

# returns list w/ p values, lowCI, uppCI
test_results <- cor.mtest(mx, conf.level = 0.95)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

png("./figs/corr_plot_all.png", width = 960, height = 960, units = "px")

corrplot(x_cor, method="color", col=col(200),
                          type="upper", order="hclust",
                          addCoef.col = "black", # Add coefficient of correlation
                          tl.cex = 2, # text label (var name) size
                          tl.col="black", tl.srt=45, #Text label color and rotation
                          cl.cex = 2, # color legend text size
                          # # Combine with significance
                          # p.mat = test_results$p, sig.level = 0.05, insig = "blank",
                          # hide correlation coefficient on the principal diagonal
                          diag=FALSE,
                          mar = c(0,0,1,0), number.cex = 2, number.digits = 2)



dev.off()
