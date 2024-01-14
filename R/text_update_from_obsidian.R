library(tidyverse)
library(glue)
library(fs)


todays_date <- lubridate::today() %>% str_replace_all("-", "")


path_in_vault <- list.files(path = "../../../Desktop/Files/Hava Vault/001 Dissertation/ch03 - mn regional soil health synthesis/",
                            full.names = TRUE,
                            pattern = "01 draft ch3")

base_name_qmd <- basename(path_in_vault) %>% str_replace_all("md", "qmd") %>% str_replace_all(" ", "_")

path_to_rproj <- glue("drafts/{base_name_qmd}")



file_copy(path = path_in_vault, new_path = path_to_rproj, overwrite = TRUE)


# what happens when we go from qmd -> obsidian md? ------------------------

target_file <- "24-compare-kmeans.qmd" 

target_md <- target_file %>% str_replace(".qmd", ".md")

path_to_obsidian <- glue("../../../Desktop/Files/Hava Vault/001 Dissertation/ch03 - mn regional soil health synthesis/{target_md}")

file_copy(path = target_file, new_path = path_to_obsidian, overwrite = TRUE)
