library(recipes)


# works: stackoverflow example --------------------------------------------

rec <- recipe( ~ ., data = USArrests) %>%
  step_normalize(all_numeric()) %>%
  step_pca(all_numeric(), num_comp = 3)

prep(rec) %>% tidy(number = 2, type = "coef")


# my data -----------------------------------------------------------------

d <- readr::read_csv("./data/clean_mu_weighted_soil_props.csv") %>% 
  select(-contains("comp_pct"))

old_names <- colnames(d)

new_names <- stringr::str_replace_all(old_names, "_r_value", "")

colnames(d) <- new_names


rec_spec <-   recipe(~., data = d) %>% 
  update_role(mukey, new_role = "ID") %>% 
 # update_role(k_6, new_role = "cluster_k6") %>% 
  # note this is log10 (the default is ln)
  step_log(om, cec7, ksat, awc, lep, base = 10) %>% 
  step_mutate(dbthirdbar = dbthirdbar^3) %>% 
  step_sqrt(claytotal, caco3) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = 4)

rec_spec

prep(rec_spec) %>% tidy(number = 5, type = "coef")

