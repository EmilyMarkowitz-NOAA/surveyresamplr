##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Resample_survey_data: Multiple species, multiple years
## Authors:       Derek Bolser, Office of Science and Technology
##                (derek.bolser@noaa.gov)
## Description:   Resample_survey_data: Multiple species, multiple years.
## Date:          March 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load support files -----------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page.
PKG <- c(
  "here",
  "devtools",
  "downloader",
  "require",
  "nwfscSurvey",

  # other tidyverse
  "plyr",
  "dplyr",
  "magrittr",
  "tidyr",
  "readr"
)

# Use pkg_install() function found in functions.R file to load packages
source("./inst/r/pkg_install.R")
base::lapply(unique(PKG), pkg_install)

# Get cpue data ----------------------------------------------------------------

#pull BT data for all spp
noaa_nwfsc_catch <- nwfscSurvey::pull_catch(
  survey = "NWFSC.Combo",
  common_name = c(
    "Pacific spiny dogfish",
    "longnose skate",
    "arrowtooth flounder",
    "petrale sole",
    "rex sole",
    "Dover sole",
    "sablefish",
    "shortspine thornyhead",
    "Pacific ocean perch",
    "darkblotched rockfish",
    "widow rockfish",
    "yellowtail rockfish",
    "bocaccio",
    "canary rockfish",
    "lingcod"
  )
) |>
  unique() |>
  dplyr::select(
    trawlid = Trawl_id,
    common_name = Common_name,
    longitude_dd = Longitude_dd,
    latitude_dd = Latitude_dd,
    year = Year,
    pass = Pass,
    area_swept_ha = Area_swept_ha,
    total_catch_numbers,
    total_catch_wt_kg,
    depth_m = Depth_m
  ) |>
  dplyr::mutate(srvy = "CA")

# nwfscSurvey::get_expanded comps requires the following in the catch data:
# year, trawl_id, depth_m, latitude_dd, area_swept_ha, total_catch_numbers

# table(catch$common_name)

# Save catch data --------------------------------------------------------------

dat <- noaa_nwfsc_catch
obj_name <- "noaa_nwfsc_catch"
title <- "Combined NWFSC catch, haul, and species data"
author <- paste0(
  "Northwest Fisheries Science Center, compiled by Emily ",
  "Markowitz (Emily.Markowitz AT noaa.gov) and Derek Bolser ",
  "(Derek.Bolser AT noaa.gov)"
)
source <- "Northwest Fisheries Science Center. "
details <- "ENTER."
description <- "ENTER."

save(noaa_nwfsc_catch, file = here::here("data", paste0(obj_name, ".rdata")))
data_documentation(dat, title, obj_name, author, source, details, description)

# Pull biological data ---------------------------------------------------------

noaa_nwfsc_bio <- nwfscSurvey::pull_bio(
  survey = "NWFSC.Combo",
  common_name = c(
    "Pacific spiny dogfish",
    "longnose skate",
    "arrowtooth flounder",
    "petrale sole",
    "rex sole",
    "Dover sole",
    "sablefish",
    "shortspine thornyhead",
    "Pacific ocean perch",
    "darkblotched rockfish",
    "widow rockfish",
    "yellowtail rockfish",
    "bocaccio",
    "canary rockfish",
    "lingcod"
  )
) |>
  dplyr::select(
    trawlid = Trawl_id,
    common_name = Common_name,
    longitude_dd = Longitude_dd,
    latitude_dd = Latitude_dd,
    year = Year,
    pass = Pass,
    sex = Sex,
    length_cm = Length_cm,
    age = Age,
    depth_m = Depth_m,
    project = Project
  ) |>
  dplyr::mutate(srvy = "CA")

# nwfscSurvey::get_expanded_comps requires sex, year, trawl_id, depth_m,
# latitude_dd, common_name, project, age, and length

# Save biological data ---------------------------------------------------------

dat <- noaa_nwfsc_bio
obj_name <- "noaa_nwfsc_bio"
title <- paste0(
  "Combined NWFSC biological (length and age composition), haul,",
  " and species data"
)
author <- paste0(
  "Northwest Fisheries Science Center, compiled by Emily ",
  "Markowitz (Emily.Markowitz AT noaa.gov), Elizabeth Perl ",
  "(Elizabeth.Gugliotti AT noaa.gov), and Derek Bolser ",
  "(Derek.Bolser AT noaa.gov)"
)
source <- "Northwest Fisheries Science Center. "
details <- "ENTER."
description <- "ENTER."

save(noaa_nwfsc_bio, file = here::here("data", paste0(obj_name, ".rdata")))
data_documentation(dat, title, obj_name, author, source, details, description)

# Get grid ---------------------------------------------------------------------

path <- here::here(
  "inst",
  "exdata",
  "grids",
  "orig",
  "california_current_grid.rda"
)
url <- paste0(
  "https://raw.githubusercontent.com/pfmc-assessments/indexwc/main",
  "/data/california_current_grid.rda"
)
# read in from indexwc

downloader::download(
  url = url,
  destfile = path
)

load(path, verbose = TRUE)

pred_grid_depth <- california_current_grid |> # rename x and y cols
  dplyr::select(
    longitude_dd = longitude,
    latitude_dd = latitude,
    pass = pass_scaled,
    depth_m = depth,
    area_km2 = area_km2_WCGBTS
  ) |>
  dplyr::mutate(srvy = "CA")

names(pred_grid_depth) <- tolower(names(pred_grid_depth))

obj_name <- paste0("noaa_nwfsc_pred_grid_depth")
assign(x = obj_name, value = pred_grid_depth)
dat <- pred_grid_depth
title <- paste0("Prediction grid for NWFSC survey")
author <- paste0(
  "Northwest Fisheries Science Center, compiled by Emily ",
  "Markowitz (Emily.Markowitz AT noaa.gov) and Derek Bolser ",
  "(Derek.Bolser AT noaa.gov)"
)
source <- paste0("Northwest Fisheries Science Center; ", url, ". ")
details <- "ENTER."
description <- "ENTER."

save(
  noaa_nwfsc_pred_grid_depth,
  file = here::here("data", paste0(obj_name, ".rdata"))
)
data_documentation(dat, title, obj_name, author, source, details, description)
