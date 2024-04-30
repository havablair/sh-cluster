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
dclim <- read_csv('./data/data_preprocessed_all_var_and_climate_20240303.csv')

forcor <- d %>% 
  select(-mukey) %>% 
  rename(
    Clay = claytotal,
    OM = om,
    CEC = cec7,
    BD = dbthirdbar,
    EC = ec, 
    pH = ph1to1h2o,
    CaCO3 = caco3,
    LEP = lep,
    Ksat = ksat,
    AWC = awc
  )

forcor_clim <- dclim %>% 
  select(-c(mukey, k_6)) %>% 
  rename(
    Clay = claytotal,
    Sand = sandtotal,
    OM = om,
    CEC = cec7,
    BD = dbthirdbar,
    EC = ec, 
    pH = ph1to1h2o,
    CaCO3 = caco3,
    LEP = lep,
    Ksat = ksat,
    AWC = awc,
    MAP = annprcp_norm,
    MAT = anntavg_norm
  ) %>% 
  select(-c(MAP, MAT))



# with corrr (tidymodels pkg) -------------------------------------------------

# defaults to pearson , only uses pairwise complete obs

tm_cor <- corrr::correlate(forcor)

# rearranges to put highly correlated variables closer together
# and changes shape to show only lower triangle
cor_prepped <- rearrange(tm_cor, absolute = FALSE) %>% 
  shave()

# rounds and aligns decimals for pretty printing
cor_aligned <- fashion(cor_prepped)

gt_cor_tbl <- cor_aligned %>%
  gt() %>%
  opt_stylize(style = 5, color = "gray") 

gt_cor_tbl
  

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

tiff(
  "./figs/dpi300/corr_plot_all_300dpi.tiff",
  width = 180,
  height = 180,
  units = "mm",
  res = 300,
  pointsize = 7
)

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



# with corrplot + climate vars --------------------------------------------

# with corrplot -----------------------------------------------------------

mx_clim <- as.matrix(forcor_clim)

x_cor_clim <- cor(mx_clim, use = "pairwise.complete.obs")

# default behavior is to omit nas
# check that here
getOption("na.action")

# returns list w/ p values, lowCI, uppCI
test_results_clim <- cor.mtest(mx_clim, conf.level = 0.95)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

png("./figs/corr_plot_all_clim.png", width = 960, height = 960, units = "px")

corrplot(x_cor_clim, method="color", col=col(200),
         type="upper", order="hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.cex = 2, # text label (var name) size
         tl.col="black", tl.srt=45, #Text label color and rotation
         cl.cex = 2, # color legend text size
         # # Combine with significance
         # p.mat = test_results_clim$p, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,
         mar = c(0,0,1,0), number.cex = 2, number.digits = 2)



dev.off()


# drop some vars ----------------------------------------------------------

subset_df <- forcor_clim %>% 
  select(-c(
    OM, # impacted by management, "semi-dynamic" 
    pH, # impacted by management (but I think still retains the signal of soil forming factors)
    Ksat, # now that we are including sand might be redundant, and we know this is really hard to measure (and might be partially from pedotransfer functions?)
    CEC # strong correlation w/ clay & sand, also b/c using ammonium acetate method buffered at pH 7 we are measuring different degrees of pH dependent charge across our wide range of soil pHs
  ))

mx_sub <- as.matrix(subset_df)

x_cor_sub <- cor(mx_sub, use = "pairwise.complete.obs")

# default behavior is to omit nas
# check that here
getOption("na.action")

# returns list w/ p values, lowCI, uppCI
test_results_sub <- cor.mtest(mx_sub, conf.level = 0.95)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

png("./figs/corr_plot_all_subset.png", width = 960, height = 960, units = "px")

corrplot(x_cor_sub, method="color", col=col(200),
         type="upper", order="hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.cex = 2, # text label (var name) size
         tl.col="black", tl.srt=45, #Text label color and rotation
         cl.cex = 2, # color legend text size
         # # Combine with significance
         # p.mat = test_results_clim$p, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,
         mar = c(0,0,1,0), number.cex = 2, number.digits = 2)



dev.off()

