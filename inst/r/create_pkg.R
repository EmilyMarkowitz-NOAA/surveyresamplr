##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Resample_survey_data: Multiple species, multiple years
## Authors:       Derek Bolser, Office of Science and Technology 
##                (derek.bolser@noaa.gov)
##                Em Markowitz, Alaska Fisheries Science Center 
##                (emily.markowitz@noaa.gov)
##                Elizabeth Perl, ECS Federal contracted to Office of Science 
##                and Technology (elizabeth.gugliotti@noaa.gov)
## Description:   Resample_survey_data: Multiple species, multiple years.
## Date:          March 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PKG <- c(
  "devtools",
  "remotes",
  "here",

  # other tidyverse
  "flextable",
  "plyr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "tibble",
  "janitor",
  "readr",

  # Survey data pull Specific packages
  # devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes=TRUE)
  "akgfmaps",
  # devtools::install_github("afsc-gap-products/coldpool")
  "coldpool",
  # "gapctd", # install_github("afsc-gap-products/gapctd")
  # "gapindex", # devtools::install_github("afsc-gap-products/gapindex")
  "nwfscSurvey",
  "surveyjoin",

  "sp",
  "RODBC",

  "roxygen2",
  "usethis",

  "pkgdown",

  # Spatial mapping
  "sf",
  "ggspatial",
  "FishStatsUtils",
  "fontawesome",

  # API pulls
  "jsonlite",
  "httr"
)

source("./inst/r/pkg_install.R")
base::lapply(unique(PKG), pkg_install)

# Data for package -------------------------------------------------------------

source(here::here("inst", "r", "data_documentation.R"))
source(here::here("inst", "r", "data_dl_nw.R"))
source(here::here("inst", "r", "data_dl_ne.R"))
source(here::here("inst", "r", "data_dl_ak.R"))

# README -----------------------------------------------------------------------

# Update README.Rmd file with new date version number!!!
rmarkdown::render(
  here::here("inst", "r", "README.Rmd"),
  output_dir = "./",
  output_file = "README.md"
)

# Update DESCRIPTION -----------------------------------------------------------
date0 <- "0.0.1"
aaa <- readLines(con = "DESCRIPTION")
aaa[grepl(pattern = "Version: ", x = aaa)] <- paste0("Version: ", date0)
write.table(
  x = aaa,
  file = "DESCRIPTION",
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE
)

# Document and create Package --------------------------------------------------

.rs.restartR()

PKG <- c("devtools", "here", "usethis", "roxygen2", "RODBC")
source("./inst/r/pkg_install.R")
base::lapply(unique(PKG), pkg_install)

devtools::document()
install("../surveyresamplr")
devtools::check()

## Create Documentation GitHub-Pages -------------------------------------------

# .rs.restartR()

PKG <- c(
  "fontawesome", # devtools::install_github("rstudio/fontawesome", force = T)
  "here",
  "usethis",
  "pkgdown"
)
source("./inst/r/pkg_install.R")
base::lapply(unique(PKG), pkg_install)

# devtools::install_github("r-lib/pkgdown")
# pkgdown::build_favicons()
# devtools::build_vignettes()
# usethis::use_pkgdown(config_file = "./pkgdown/_pkgdown.yml")
# usethis::use_vignette("my-vignette")
# pkgdown::clean_site()
pkgdown::build_site(pkg = here::here())
# usethis::use_github_action("pkgdown")

# Save Package tar.gz
# date0 <- "0.0.1"
# devtools::build(path = here::here(paste0("surveyresamplr_",date0,".tar.gz")))
