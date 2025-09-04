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

#get rid of memory limits
options(future.globals.maxSize = 1 * 1024^4) # Allow up to 1 TB for globals

# Set directories --------------------------------------------------------------
library(here)

wd <- paste0(here::here(), "/")
dir_out <- paste0(wd, "/output/")
dir.create(dir_out, showWarnings = FALSE)

# Load support files -----------------------------------------------------------

source(paste0(wd, "code/functions.R"))
crs_latlon <- "+proj=longlat +datum=WGS84" # decimal degrees

# Install Libraries ------------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page.
PKG <- c(
  "devtools",
  # "require",

  # other tidyverse
  "plyr",
  "dplyr",
  "magrittr",
  "tidyr",
  "readxl",
  "viridis",
  "readr",
  "ggplot2",
  "tibble",
  "janitor",
  "data.table",

  # Survey data pull Specific packages
  "akgfmaps",
  # devtools::install_github("afsc-gap-products/akgfmaps",
  # build_vignettes = TRUE)
  "coldpool", # devtools::install_github("afsc-gap-products/coldpool")
  "nwfscSurvey", # remotes::install_github("pfmc-assessments/nwfscSurvey")
  "jsonlite",
  "httr",
  "sp",
  "RODBC",

  # Spatial mapping
  "sf",
  "ggspatial",

  # API pulls
  "jsonlite",
  "httr",

  # parallelizing
  "forcats",
  "purrr",
  "furrr",
  "doParallel",

  # sampling
  "sampling",

  "coldpool",
  "akgfmaps",

  # modeling
  "arrow",
  "future.apply",
  "future.callr",
  "sdmTMB",
  # install.packages("remotes",)
  # remotes::install_github("pbs-assess/sdmTMBextra", dependencies = TRUE")
  "Matrix",
  "MASS",
  "cluster",
  "TMB",
  "INLA"
  # install.packages("INLA",repos=c(getOption("repos"),
  # INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
  # inla.upgrade() # for the stable version
  # Windows will not update a package already loaded, so then you have to
  # remove the package and install it from scratch. There are two suggested
  # packages, ‘graph’ and ‘Rgraphviz’, that are on Bioconductor, and you can
  # install those with:
  #
  #   if (!requireNamespace("BiocManager", quietly = TRUE))
  #     install.packages("BiocManager")
  # BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)

  # options(repos=c( inlabruorg = "https://inlabru-org.r-universe.dev",
  # INLA = "https://inla.r-inla-download.org/R/testing",
  # CRAN = "https://cran.rstudio.com") )
  # install.packages("fmesher")
)

# Use pkg_install() function found in functions.R file to load packages
base::lapply(unique(PKG), pkg_install)

# Update README (sometimes) ----------------------------------------------------

# rmarkdown::render(here::here("code/README.Rmd"),
#                   output_dir = "./",
#                   output_file = here::here("README.md"))

# Run scenarios ----------------------------------------------------------------

## NWFSC California Current ----------------------------------------------------

### Define study species -------------------------------------------------------

spp_list <- data.frame(
  srvy = "CA",
  common_name = c(
    "arrowtooth flounder",
    "bocaccio",
    "canary rockfish",
    "darkblotched rockfish",
    "Dover sole",
    "lingcod",
    "lingcod",
    "longnose skate",
    "Pacific ocean perch",
    "Pacific spiny dogfish",
    "petrale sole",
    "rex sole",
    "sablefish",
    "shortspine thornyhead",
    "yellowtail rockfish",
    "widow rockfish"
  ),
  file_name = c(
    "arrowtooth_flounder",
    "bocaccio",
    "canary_rockfish",
    "darkblotched_rockfish",
    "dover_sole",
    "lingcod_N",
    "lingcod_S",
    "longnose_skate",
    "pacific_ocean_perch",
    "pacific_spiny_dogfish",
    "petrale_sole",
    "rex_sole",
    "sabefish",
    "shortspine_thornyhead",
    "yellowtail_rockfish",
    "widow_rockfish"
  ),
  filter_lat_gt = c(
    34,
    NA,
    NA,
    335,
    NA,
    35,
    NA,
    NA,
    35,
    NA,
    NA,
    NA,
    NA,
    NA,
    35.5,
    33.5
  ),
  filter_lat_lt = c(
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    35,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA
  ),
  filter_depth = c(
    NA,
    500,
    275,
    675,
    NA,
    450,
    450,
    NA,
    500,
    700,
    675,
    700,
    NA,
    NA,
    425,
    675
  ),
  model_fn = c(
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass + depth_m + (depth_m^2)",
    "total_catch_wt_kg ~ 0 + factor(year) + pass",
    "total_catch_wt_kg ~ 0 + factor(year) + pass"
  ),
  # model_fn = c( # name of funcion for sdm. Will build in specificity for this
  # later
  # "species_sdm_fn", "species_sdm_fn", "canary_sdm_fn", "darkblotched_sdm_fn",
  # "species_sdm_fn", "species_sdm_fn", "species_sdm_fn", "species_sdm_fn",
  # "species_sdm_fn", "species_sdm_fn",
  # "species_sdm_lognormal_fn", "species_sdm_fn", "species_sdm_lognormal_fn"),
  # "shortspine_sdm_fn", "species_sdm_fn", "widow_sdm_fn"
  model_family = c(
    "delta_gamma",
    "delta_gamma",
    "delta_lognormal",
    "delta_lognormal",
    "delta_gamma",
    "delta_gamma",
    "delta_gamma",
    "delta_gamma",
    "delta_gamma",
    "delta_gamma",
    "delta_lognormal",
    "delta_gamma",
    "delta_lognormal",
    "delta_gamma",
    "delta_gamma",
    "delta_gamma"
  ),
  model_anisotropy = c(
    TRUE,
    TRUE,
    FALSE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE
  ),
  model_spatiotemporal = c(
    c("iid, iid"),
    c("iid, iid"),
    c("iid, off"),
    c("off, iid"),
    c("iid, iid"),
    c("iid, iid"),
    c("iid, iid"),
    c("iid, iid"),
    c("iid, iid"),
    c("iid, iid"),
    c("iid, iid"),
    c("iid, iid"),
    c("iid, iid"),
    c("iid, iid"),
    c("iid, iid"),
    c("off, off")
  )
)

### Load survey data -----------------------------------------------------------

# source(paste0(wd, "code/data_dl_nw.r"))

catch_ca <- utils::read.csv(paste0(wd, "data/nwfsc_bt_fmp_spp_updated.csv"))
#pulled data again to get 2024
catch <- catch_ca |>
  dplyr::select(
    trawlid = Trawl_id,
    common_name = Common_name,
    longitude_dd = Longitude_dd,
    latitude_dd = Latitude_dd,
    year = Year,
    pass = Pass,
    total_catch_wt_kg,
    depth_m = Depth_m
  ) |>
  dplyr::mutate(srvy = "CA")

### Load grid data -------------------------------------------------------------

load(paste0(wd, "grids/noaa_nwfsc_ca_pred_grid_depth.rda"), verbose = TRUE)
pred_grid <- california_current_grid |> # rename x and y cols
  dplyr::select(
    longitude_dd = longitude,
    latitude_dd = latitude,
    pass = pass_scaled,
    depth_m = depth,
    area_km2 = area_km2_WCGBTS
  ) |>
  dplyr::mutate(srvy = "CA")
grid_yrs <- sdmTMB::replicate_df(pred_grid, "year", unique(catch$year))

### Variables ------------------------------------------------------------------

srvy <- "CA"
seq_from <- 0.1
seq_to <- 1
seq_by <- 0.1
tot_dataframes <- 91
replicate_num <- 10

### Run ------------------------------------------------------------------------

sink(
  file = paste0(dir_out, srvy, "_", Sys.Date(), "_logfile.txt"),
  append = FALSE,
  split = TRUE
) # for screen and log
map(
  seq_len(nrow(spp_list)),
  ~ clean_and_resample(
    spp_list[.x, ],
    catch,
    seq_from,
    seq_to,
    seq_by,
    tot_dataframes,
    replicate_num,
    grid_yrs,
    dir_out
  )
)
sink()

### Plot indices ---------------------------------------------------------------

plot_results(srvy = srvy, dir_out = dir_out)

## Eastern Bering Sea ----------------------------------------------------------

### Define study species -------------------------------------------------------

spp_list <- data.frame(
  srvy = "EBS",
  common_name = c(
    "walleye pollock",
    "snow crab",
    "Pacific cod",
    "red king crab",
    "blue king crab",
    "yellowfin sole",
    "Pacific halibut",
    "Alaska plaice",
    "flathead sole",
    "northern rock sole",
    "arrowtooth flounder"
  ),
  species_code = as.character(c(
    21740,
    68580,
    21720,
    69322,
    69323,
    10210,
    10120,
    10285,
    10130,
    10261,
    10110
  )),
  filter_lat_lt = NA,
  filter_lat_gt = NA,
  filter_depth = NA,
  model_fn = "total_catch_wt_kg ~ 0 + factor(year) + bottom_temperature_c",
  model_family = "delta_gamma",
  model_anisotropy = TRUE,
  model_spatiotemporal = c(c("iid, iid"))
) |>
  dplyr::mutate(
    file_name = gsub(
      pattern = " ",
      replacement = "_",
      x = (tolower(common_name))
    )
  )

### Load survey data -----------------------------------------------------------

# source(paste0(wd, "code/data_dl_ak.r"))

load(file = paste0(wd, "/data/noaa_afsc_catch.rda"))
catch <- noaa_afsc_catch |> dplyr::filter(srvy == "EBS")

### Load grid data -------------------------------------------------------------

load(paste0(wd, "grids/noaa_afsc_ebs_pred_grid_depth.rdata"), verbose = TRUE)

#### Add temperature: Coldpool temperature data
# Data that varies over space and time (bottom temperature)
# Here, bottom temperature, and thereby the cold pool extent, have been show to
# drive the distribution of many species. This is especially true for walleye
# pollock.
# For this we are going to lean on our in-house prepared validated and pre-
# prepared [{coldpool} R package](https://github.com/afsc-gap-products/coldpool)
# (S. Rohan, L. Barnett, and N. Charriere). This data interpolates over the
# whole area of the survey so there are no missing data.
grid_yrs <-
  dplyr::bind_cols(
    pred_grid_depth[, c("longitude_dd", "latitude_dd", "depth_m")],
    terra::unwrap(coldpool::ebs_bottom_temperature) |>
      terra::project(crs_latlon) |>
      terra::extract(pred_grid_depth[, c("longitude_dd", "latitude_dd")])
  )
grid_yrs <- grid_yrs |>
  tidyr::pivot_longer(
    names_to = "year",
    values_to = "bottom_temperature_c",
    cols = names(grid_yrs_temperature)[4:ncol(grid_yrs_temperature)]
  )
save(
  grid_yrs_depth_temperature,
  file = paste0(
    "grids/grid_yr_temperature/noaa_afsc_ebs_pred_grid_depth_temperature.rdata"
  )
)

# # test you extracted correctkt
# ggplot(data = grid_yrs |>
#        dplyr::filter(year %in% c(2022:2024)),
#        mapping = ggplot2::aes(x = longitude_dd, y = latitude_dd,
#        color = bottom_temperature_c)) +
#   geom_point() +
#   facet_wrap(facets = "year")

### Variables ------------------------------------------------------------------

srvy <- "EBS"
seq_from <- 0.2
seq_to <- 1.0
seq_by <- 0.2
tot_dataframes <- 13
replicate_num <- 3

### Run ------------------------------------------------------------------------

sink(
  file = paste0(dir_out, srvy, "_", Sys.Date(), "_logfile.txt"),
  append = FALSE,
  split = TRUE
) # for screen and log
map(
  seq_len(nrow(spp_list)),
  ~ clean_and_resample(
    spp_list[.x, ],
    catch,
    seq_from,
    seq_to,
    seq_by,
    tot_dataframes,
    replicate_num,
    grid_yrs,
    dir_out,
    test = TRUE
  )
)
sink()

### Plot indices ---------------------------------------------------------------

plot_results(srvy = srvy, dir_out = dir_out)

## Northern Bering Sea ---------------------------------------------------------

### Define study species -------------------------------------------------------

spp_list <- data.frame(
  srvy = "NBS",
  common_name = c(
    "walleye pollock",
    "snow crab",
    "Pacific cod",
    "red king crab",
    "blue king crab",
    "yellowfin sole",
    "Pacific halibut",
    "Alaska plaice",
    "flathead sole",
    "northern rock sole",
    "arrowtooth flounder"
  ),
  species_code = as.character(c(
    21740,
    68580,
    21720,
    69322,
    69323,
    10210,
    10120,
    10285,
    10130,
    10261,
    10110
  )),
  filter_lat_lt = NA,
  filter_lat_gt = NA,
  filter_depth = NA,
  model_fn = "total_catch_wt_kg ~ 0 + factor(year)",
  model_family = "delta_gamma",
  model_anisotropy = TRUE,
  model_spatiotemporal = c(c("iid, iid"))
) |>
  dplyr::mutate(
    file_name = gsub(
      pattern = " ",
      replacement = "_",
      x = (tolower(common_name))
    )
  )

### Load survey data -----------------------------------------------------------

# source(paste0(wd, "code/data_dl_ak.r"))

load(file = paste0(wd, "/data/noaa_afsc_catch.rda"))
catch <- noaa_afsc_catch |> dplyr::filter(srvy == "NBS")

### Load grid data -------------------------------------------------------------

load(paste0(wd, "grids/noaa_afsc_nbs_pred_grid_depth.rdata"), verbose = TRUE)
grid_yrs <- sdmTMB::replicate_df(pred_grid_depth, "year", unique(catch$year))

### Variables ------------------------------------------------------------------

srvy <- "NBS"
seq_from <- 0.2
seq_to <- 1.0
seq_by <- 0.2
tot_dataframes <- 13
replicate_num <- 3

### Run ------------------------------------------------------------------------

sink(
  file = paste0(dir_out, srvy, "_", Sys.Date(), "_logfile.txt"),
  append = FALSE,
  split = TRUE
) # for screen and log
map(
  seq_len(nrow(spp_list)),
  ~ clean_and_resample(
    spp_list[.x, ],
    catch,
    seq_from,
    seq_to,
    seq_by,
    tot_dataframes,
    replicate_num,
    grid_yrs,
    dir_out,
    test = TRUE
  )
)
sink()

### Plot indices ---------------------------------------------------------------

plot_results(srvy = srvy, dir_out = dir_out)

## Eastern + Northern Bering Sea -----------------------------------------------

### Define study species -------------------------------------------------------

spp_list <- data.frame(
  srvy = "BS",
  common_name = c(
    "walleye pollock",
    "snow crab",
    "Pacific cod",
    "red king crab",
    "blue king crab",
    "yellowfin sole",
    "Pacific halibut",
    "Alaska plaice",
    "flathead sole",
    "northern rock sole",
    "arrowtooth flounder"
  ),
  species_code = as.character(c(
    21740,
    68580,
    21720,
    69322,
    69323,
    10210,
    10120,
    10285,
    10130,
    10261,
    10110
  )),
  filter_lat_lt = NA,
  filter_lat_gt = NA,
  filter_depth = NA,
  model_fn = "total_catch_wt_kg ~ 0 + factor(year) + bottom_temperature_c + depth_m",
  model_family = "delta_gamma",
  model_anisotropy = TRUE,
  model_spatiotemporal = c(c("iid, iid"))
) |>
  dplyr::mutate(
    file_name = gsub(
      pattern = " ",
      replacement = "_",
      x = (tolower(common_name))
    )
  )

### Load survey data -----------------------------------------------------------

# source(paste0(wd, "code/data_dl_ak.r"))

load(file = paste0(wd, "/data/noaa_afsc_catch.rda"))
catch <- noaa_afsc_catch |> dplyr::filter(srvy %in% c("EBS", "NBS"))

### Load grid data -------------------------------------------------------------

load(paste0(wd, "grids/noaa_afsc_bs_pred_grid_depth.rdata"), verbose = TRUE)
#### Add temperature: Coldpool temperature data
# Data that varies over space and time (bottom temperature)
# Here, bottom temperature, and thereby the cold pool extent, have been show to
# drive the distribution of many species. This is especially true for walleye
# pollock.
# For this we are going to lean on our in-house prepared validated and pre-
# prepared [{coldpool} R package](https://github.com/afsc-gap-products/coldpool)
# (S. Rohan, L. Barnett, and N. Charriere). This data interpolates over the
# whole area of the survey so there are no missing data.
ebs_only <- setdiff(
  names(terra::unwrap(coldpool::ebs_bottom_temperature)),
  names(terra::unwrap(coldpool::nbs_ebs_bottom_temperature))
)
grid_yrs_temperature <- dplyr::full_join(
  dplyr::bind_cols(
    pred_grid_depth[, c("longitude_dd", "latitude_dd", "depth_m", "area_km2")],
    terra::unwrap(coldpool::ebs_bottom_temperature) |> #TOLEDO
      subset(ebs_only) |>
      terra::project(crs_latlon) |>
      terra::extract(pred_grid_depth[, c("longitude_dd", "latitude_dd")])
  ),
  dplyr::bind_cols(
    pred_grid_depth[, c("longitude_dd", "latitude_dd", "depth_m")],
    terra::unwrap(coldpool::nbs_ebs_bottom_temperature) |>
      terra::project(crs_latlon) |>
      terra::extract(pred_grid_depth[, c("longitude_dd", "latitude_dd")])
  )
)

grid_yrs_depth_temperature <- grid_yrs <- grid_yrs_temperature |>
  tidyr::pivot_longer(
    names_to = "year",
    values_to = "bottom_temperature_c",
    cols = names(grid_yrs_temperature)[6:ncol(grid_yrs_temperature)]
  ) |>
  dplyr::mutate(year = as.numeric(year))
save(
  grid_yrs_depth_temperature,
  file = paste0(
    "grids/grid_yr_temperature/noaa_afsc_bs_pred_grid_depth_temperature.rdata"
  )
)

# # test you extracted correct
# ggplot(data = grid_yrs |>
#        dplyr::filter(year %in% c(2022:2024)),
#        mapping = ggplot2::aes(x = longitude_dd, y = latitude_dd,
#        color = bottom_temperature_c)) +
#   geom_point() +
#   facet_wrap(facets = "year")

### Variables ------------------------------------------------------------------

srvy <- "BS"
seq_from <- 0.2
seq_to <- 1.0
seq_by <- 0.2
tot_dataframes <- 13
replicate_num <- 3

### Run ------------------------------------------------------------------------

sink(
  file = paste0(dir_out, srvy, "_", Sys.Date(), "_logfile.txt"),
  append = FALSE,
  split = TRUE
) # for screen and log
map(
  seq_len(nrow(spp_list)),
  ~ clean_and_resample(
    spp_list[.x, ],
    catch,
    seq_from,
    seq_to,
    seq_by,
    tot_dataframes,
    replicate_num,
    grid_yrs,
    dir_out,
    test = TRUE
  )
)
sink()

### Plot indices ---------------------------------------------------------------

plot_results(srvy = srvy, dir_out = dir_out)

## Gulf of Alaska --------------------------------------------------------------

### Define study species -------------------------------------------------------

spp_list <- data.frame(
  srvy = "GOA",
  common_name = c(
    "walleye pollock",
    "Pacific cod",
    "Pacific ocean perch",
    "flathead sole",
    "northern rockfish",
    "arrowtooth flounder"
  ),
  species_code = as.character(c(21740, 21720, 30060, 10130, 30420, 10110)),
  filter_lat_lt = NA,
  filter_lat_gt = NA,
  filter_depth = NA,
  model_fn = "total_catch_wt_kg ~ 0 + factor(year)",
  model_family = "delta_gamma",
  model_anisotropy = TRUE,
  model_spatiotemporal = c(c("iid, iid"))
) |>
  dplyr::mutate(
    file_name = gsub(
      pattern = " ",
      replacement = "_",
      x = (tolower(common_name))
    )
  )

### Load survey data -----------------------------------------------------------

# source(paste0(wd, "code/data_dl_ak.r"))

load(file = paste0(wd, "/data/noaa_afsc_catch.rda"))
catch <- noaa_afsc_catch |> dplyr::filter(srvy == "GOA")

### Load grid data -------------------------------------------------------------

load(paste0(wd, "grids/noaa_afsc_goa_pred_grid_depth.rdata"), verbose = TRUE)
grid_yrs <- sdmTMB::replicate_df(pred_grid_depth, "year", unique(catch$year))

### Variables ------------------------------------------------------------------

srvy <- "GOA"
seq_from <- 0.2
seq_to <- 1.0
seq_by <- 0.2
tot_dataframes <- 13
replicate_num <- 3

### Run ------------------------------------------------------------------------

sink(
  file = paste0(dir_out, srvy, "_", Sys.Date(), "_logfile.txt"),
  append = FALSE,
  split = TRUE
) # for screen and log
map(
  seq_len(nrow(spp_list)),
  ~ clean_and_resample(
    spp_list[.x, ],
    catch,
    seq_from,
    seq_to,
    seq_by,
    tot_dataframes,
    replicate_num,
    grid_yrs,
    dir_out,
    test = TRUE
  )
)
sink()

### Plot indices ---------------------------------------------------------------

plot_results(srvy = srvy, dir_out = dir_out)

## Aleutian Islands ------------------------------------------------------------

### Define study species -------------------------------------------------------

spp_list <- data.frame(
  srvy = "AI",
  common_name = c(
    "walleye pollock",
    "Pacific cod",
    "Pacific ocean perch",
    "flathead sole",
    "northern rockfish",
    "arrowtooth flounder"
  ),
  species_code = as.character(c(21740, 21720, 30060, 10130, 30420, 10110)),
  filter_lat_lt = NA,
  filter_lat_gt = NA,
  filter_depth = NA,
  model_fn = "total_catch_wt_kg ~ 0 + factor(year)",
  model_family = "delta_gamma",
  model_anisotropy = TRUE,
  model_spatiotemporal = c(c("iid, iid"))
) |>
  dplyr::mutate(
    file_name = gsub(
      pattern = " ",
      replacement = "_",
      x = (tolower(common_name))
    )
  )

### Load survey data -----------------------------------------------------------

# source(paste0(wd, "code/data_dl_ak.r"))

load(file = paste0(wd, "/data/noaa_afsc_catch.rda"))
catch <- noaa_afsc_catch |> dplyr::filter(srvy == "AI")

### Load grid data -------------------------------------------------------------

load(paste0(wd, "grids/noaa_afsc_ai_pred_grid_depth.rdata"), verbose = TRUE)
grid_yrs <- sdmTMB::replicate_df(pred_grid_depth, "year", unique(catch$year))

### Variables ------------------------------------------------------------------

srvy <- "AI"
seq_from <- 0.2
seq_to <- 1.0
seq_by <- 0.2
tot_dataframes <- 13
replicate_num <- 3

### Run ------------------------------------------------------------------------

sink(
  file = paste0(dir_out, srvy, "_", Sys.Date(), "_logfile.txt"),
  append = FALSE,
  split = TRUE
) # for screen and log
map(
  seq_len(nrow(spp_list)),
  ~ clean_and_resample(
    spp_list[.x, ],
    catch,
    seq_from,
    seq_to,
    seq_by,
    tot_dataframes,
    replicate_num,
    grid_yrs,
    dir_out,
    test = TRUE
  )
)
sink()

### Plot indices --------------------------------------------------------------

plot_results(srvy = srvy, dir_out = dir_out)

## Spring Northwest Atlantic ---------------------------------------------------

### Define study species -------------------------------------------------------

spp_list <-
  data.frame(
    srvy = "NWA_SPRING",
    common_name = c(
      "Atlantic herring",
      "black sea bass",
      "Atlantic cod",
      "American lobster",
      "longfin squid",
      "mackerel",
      "monkfish",
      "red hake",
      "scup",
      "sea scallop",
      "silver hake",
      "summer flounder",
      "winter flounder"
    ),
    file_name0 = c(
      "AtlanticHerring",
      "BlackSeaBass",
      "Cod",
      "Lobster",
      "LongfinSquid",
      "Mackerel",
      "Monkfish",
      "RedHake",
      "Scup",
      "SeaScallop",
      "SilverHake",
      "SummerFlounder",
      "WinterFlounder"
    ),
    filter_lat_lt = NA,
    filter_lat_gt = NA,
    filter_depth = NA,
    model_fn = "total_catch_wt_kg ~ 0 + factor(year)",
    model_family = "delta_gamma",
    model_anisotropy = TRUE,
    model_spatiotemporal = c(c("iid, iid"))
  ) |>
  dplyr::mutate(
    file_name = gsub(
      pattern = " ",
      replacement = "_",
      x = (tolower(common_name))
    )
  )

### Load survey data -----------------------------------------------------------

# source(paste0(wd, "code/data_dl_ne.r"))

load(file = paste0(wd, "/data/noaa_nefsc_catch.rda"))
catch <- noaa_nefsc_catch |> dplyr::filter(srvy == "SPRING")

### Load grid data -------------------------------------------------------------

load(paste0(wd, "grids/noaa_nefsc_nwa_pred_grid_depth.rdata"), verbose = TRUE)
grid_yrs <- sdmTMB::replicate_df(pred_grid_depth, "year", unique(catch$year))

### Variables ------------------------------------------------------------------

srvy <- "NWA_SPRING"
seq_from <- 0.2
seq_to <- 1.0
seq_by <- 0.2
tot_dataframes <- 13
replicate_num <- 3

### Run ------------------------------------------------------------------------

sink(
  file = paste0(dir_out, srvy, "_", Sys.Date(), "_logfile.txt"),
  append = FALSE,
  split = TRUE
) # for screen and log
map(
  seq_len(nrow(spp_list)),
  ~ clean_and_resample(
    spp_list[.x, ],
    catch,
    seq_from,
    seq_to,
    seq_by,
    tot_dataframes,
    replicate_num,
    grid_yrs,
    dir_out,
    test = TRUE
  )
)
sink()

### Plot indices --------------------------------------------------------------

plot_results(srvy = srvy, dir_out = dir_out)


## Fall Northwest Atlantic -----------------------------------------------------

### Define study species -------------------------------------------------------

spp_list <-
  data.frame(
    srvy = "NWA_FALL",
    common_name = c(
      "Atlantic herring",
      "black sea bass",
      "Atlantic cod",
      "American lobster",
      "longfin squid",
      "mackerel",
      "monkfish",
      "red hake",
      "scup",
      "sea scallop",
      "silver hake",
      "summer flounder",
      "winter flounder"
    ),
    file_name0 = c(
      "AtlanticHerring",
      "BlackSeaBass",
      "Cod",
      "Lobster",
      "LongfinSquid",
      "Mackerel",
      "Monkfish",
      "RedHake",
      "Scup",
      "SeaScallop",
      "SilverHake",
      "SummerFlounder",
      "WinterFlounder"
    ),
    filter_lat_lt = NA,
    filter_lat_gt = NA,
    filter_depth = NA,
    model_fn = "total_catch_wt_kg ~ 0 + factor(year)",
    model_family = "delta_gamma",
    model_anisotropy = TRUE,
    model_spatiotemporal = c(c("iid, iid"))
  ) |>
  dplyr::mutate(
    file_name = gsub(
      pattern = " ",
      replacement = "_",
      x = (tolower(common_name))
    )
  )

### Load survey data -----------------------------------------------------------

# source(paste0(wd, "code/data_dl_ne.r"))

load(file = paste0(wd, "/data/noaa_nefsc_catch.rda"))
catch <- noaa_nefsc_catch |> dplyr::filter(srvy == "FALL")

### Load grid data -------------------------------------------------------------

load(paste0(wd, "grids/noaa_nefsc_nwa_pred_grid_depth.rdata"), verbose = TRUE)
grid_yrs <- sdmTMB::replicate_df(pred_grid_depth, "year", unique(catch$year))

### Variables ------------------------------------------------------------------

srvy <- "NWA_FALL"
seq_from <- 0.2
seq_to <- 1.0
seq_by <- 0.2
tot_dataframes <- 13
replicate_num <- 3

### Run ------------------------------------------------------------------------

sink(
  file = paste0(dir_out, srvy, "_", Sys.Date(), "_logfile.txt"),
  append = FALSE,
  split = TRUE
) # for screen and log
map(
  seq_len(nrow(spp_list)),
  ~ clean_and_resample(
    spp_list[.x, ],
    catch,
    seq_from,
    seq_to,
    seq_by,
    tot_dataframes,
    replicate_num,
    grid_yrs,
    dir_out,
    test = TRUE
  )
)
sink()

### Plot indices ---------------------------------------------------------------

plot_results(srvy = srvy, dir_out = dir_out)
