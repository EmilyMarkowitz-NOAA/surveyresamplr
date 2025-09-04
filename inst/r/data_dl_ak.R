##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Resample_survey_data: Multiple species, multiple years
## Authors:       Em Markowitz, Alaska Fisheries Science Center 
##                (emily.markowitz@noaa.gov)
## Description:   Resample_survey_data: Multiple species, multiple years.
## Date:          March 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Install Libraries ------------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c(
  "here",
  "devtools",
  "plyr",
  "dplyr",
  "magrittr",
  "tidyr",
  "here",
  "readr",
  "tibble",
  "janitor",
  "data.table",
  "jsonlite",
  "httr",
  "sp",
  "RODBC",
  "FishStatsUtils",
  "akgfmaps",
  "VAST" #needed this for FishStatsUtils::convert_shapefile()
)

# Use pkg_install() function found in functions.R file to load packages
source("./inst/r/pkg_install.R")
base::lapply(unique(PKG), pkg_install)

# Test species to pull ---------------------------------------------------------

spp_list <- dplyr::bind_rows(
  data.frame(
    srvy = "GOA",
    common_name = c(
      "walleye pollock",
      "Pacific cod",
      "Pacific ocean perch",
      "flathead sole",
      "northern rockfish",
      "arrowtooth flounder"
    ),
    species_code = as.character(c(21740, 21720, 30060, 10130, 30420, 10110))
  ),
  data.frame(
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
    ))
  )
)
spp_list <- unique(spp_list[, c("common_name", "species_code")])

# CPUE/Catch Load data ---------------------------------------------------------

## Load haul data --------------------------------------------------------------
api_link_haul <- paste0('https://apps-st.fisheries.noaa.gov/ods/foss/afsc_',
                        'groundfish_survey_haul/')

dat <- data.frame()
for (i in seq(0, 500000, 10000)) {
  ## find how many iterations it takes to cycle through the data
  print(i)
  ## query the API link
  res <- httr::GET(url = paste0(api_link_haul, "?offset=", i, "&limit=10000"))
  ## convert from JSON format
  data <- jsonlite::fromJSON(base::rawToChar(res$content))

  ## if there are no data, stop the loop
  if (is.null(nrow(data$items))) {
    break
  }

  ## bind sub-pull to dat data.frame
  dat <- dplyr::bind_rows(
    dat,
    data$items |>
      dplyr::select(-links)
  ) # necessary for API accounting, but not part of the dataset)
}

dat_haul <- dat

## Load species data -----------------------------------------------------------
api_link_species <- paste0('https://apps-st.fisheries.noaa.gov/ods/foss/afsc_',
                           'groundfish_survey_species/')

## query the API link
res <- httr::GET(url = paste0(api_link_species, "?offset=0&limit=10000"))
## convert from JSON format
data <- jsonlite::fromJSON(base::rawToChar(res$content))

## bind sub-pull to dat data.frame
dat_species <- data$items |>
  dplyr::select(-links) 
  # necessary for API accounting, but not part of the dataset)

## Load catch data from FOSS ---------------------------------------------------
api_link_catch <- paste0('https://apps-st.fisheries.noaa.gov/ods/foss/afsc_',
                         'groundfish_survey_catch/')

dat <- data.frame()
for (ii in seq_len(nrow(spp_list))) {
  print(spp_list$common_name[ii])
  for (i in seq(0, 1000000, 10000)) {
    ## find how many iterations it takes to cycle through the data
    print(i)
    ## query the API link
    # res<-httr::GET(url = paste0(api_link_catch, "?offset=",i,"&limit=10000"))
    # res<-httr::GET(url = paste0(api_link_haul, 
    #                      '?limit=10000&q={"species_code":2023,"srvy":"EBS"}'))
    res <- httr::GET(
      url = paste0(
        api_link_catch,
        '?offset=',
        i,
        '&limit=10000&q={"species_code":',
        spp_list$species_code[ii],
        '}'
      )
    )
    # res <- httr::GET(url = paste0('https://apps-st.fisheries.noaa.gov/ods/',
    #                               'foss/afsc_groundfish_survey_catch/?offset',
    #                               '=0&limit=10000&q={%22species_code%22:,
    #                               '21740',%22species_code%22:21720}'))
    ## convert from JSON format
    data <- jsonlite::fromJSON(base::rawToChar(res$content))

    ## if there are no data, stop the loop
    if (is.null(nrow(data$items))) {
      break
    }

    ## bind sub-pull to dat data.frame
    dat <- dplyr::bind_rows(
      dat,
      data$items |>
        dplyr::select(-links)
    ) # necessary for API accounting, but not part of the dataset)
  }
}
dat_catch <- dat

## Zero filled data ------------------------------------------------------------

noaa_afsc_catch <- dplyr::full_join(
  # find all species that have been caught, by survey
  x = dplyr::left_join(dat_catch, dat_haul, by = "hauljoin") |>
    dplyr::select(survey_definition_id, species_code) |>
    dplyr::distinct(),
  # find all haul events (hauljoins), by survey
  y = dat_haul |>
    dplyr::select(survey_definition_id, hauljoin) |>
    dplyr::distinct(),
  relationship = "many-to-many",
  by = "survey_definition_id"
) |>
  dplyr::select(-survey_definition_id) |> # now, redundant
  # get full data
  dplyr::left_join(dat_catch) |>
  dplyr::left_join(dat_haul) |>
  dplyr::left_join(
    dat_species |>
      dplyr::select(species_code, common_name) |>
      dplyr::mutate(species_code = as.numeric(species_code))
  ) |>
  dplyr::select(
    srvy,
    trawlid = hauljoin,
    common_name = common_name,
    species_code,
    total_catch_numbers = count,
    total_catch_wt_kg = weight_kg,
    cpue_kgkm2,
    latitude_dd = latitude_dd_start,
    longitude_dd = longitude_dd_start,
    year,
    bottom_temperature_c = bottom_temperature_c,
    depth_m
  )

## Save data -------------------------------------------------------------------

dat <- noaa_afsc_catch
title <- "Combined AFSC catch, haul, and species data from FOSS"
obj_name <- "noaa_afsc_catch"
author <- paste0("Alaska Fisheries Science Center, compiled by Emily ", 
                 "Markowitz (Emily.Markowitz AT noaa.gov)")
source <- paste0("https://github.com/afsc-gap-products/gap_products and ",
          "https://www.fisheries.noaa.gov/foss/f?p=215:28:14951401791129:::::")
details <- paste0("The Resource Assessment and Conservation Engineering (RACE)",
                  " Division Groundfish Assessment Program (GAP) of the Alaska",
                  " Fisheries Science Center (AFSC) conducts fisheries-",
                  "independent bottom trawl surveys to assess the populations ",
                  "of demersal fish and crab stocks of Alaska.")
description <- paste0("The final, validated survey data are publicly ",
                      "accessible soon after surveys are completed on the ",
                      "Fisheries One Stop Shop (FOSS) platform. This data ",
                      "includes catch, haul, and environmental data collected ",
                      "at each station. On the FOSS data platform, users can ",
                      "interactively select, view, and download data. ",
                      "Descriptive documentation and user-examples are ",
                      "available on the metadata page.")

save(noaa_afsc_catch, file = here::here("data", paste0(obj_name, ".rdata")))
data_documentation(dat, title, obj_name, author, source, details, description)

# Make extrapolation grids -----------------------------------------------------

extrap_grid <- function(
  pred_grid,
  srvy_in,
  srvy_out = NULL,
  noaa_afsc_catch,
  crs_proj = NULL
) {
  # crs_latlon <- "+proj=longlat +datum=WGS84" # decimal degrees
  crs_latlon <- "+proj=longlat +datum=WGS84 +no_defs"
  if (is.null(srvy_out)) {
    srvy_out <- srvy_in
  }

  pred_grid <- pred_grid |>
    dplyr::mutate(pass = 1, srvy = srvy_out)

  if (!is.null(crs_proj)) {
    sp_extrap_raster <- sp::SpatialPoints(
      coords = coordinates(as.matrix(pred_grid[, c(
        "longitude_dd",
        "latitude_dd"
      )])),
      proj4string = CRS(crs_proj)
    ) |>
      sp::spTransform(CRSobj = crs_latlon)
  } else {
    sp_extrap_raster <- sp::SpatialPoints(
      coords = sp::coordinates(as.matrix(pred_grid[, c(
        "longitude_dd",
        "latitude_dd"
      )])),
      proj4string = sp::CRS(crs_latlon)
    )
  }

  ## Add depth -----------------------------------------------------------------

  # x <- noaa_afsc_catch |>
  #   dplyr::filter(
  #     year > 2000 &
  #       srvy %in% srvy_in
  #   ) |> 
  #   # lets assume our ability to assess depth has improved, technologically 
  #   # since before 2000. year negotiable if anyone has strong opinions
  #   dplyr::select(longitude_dd, latitude_dd, depth_m) |>
  #   stats::na.omit() |>
  #   sf::st_as_sf(
  #     coords = c(x = "longitude_dd", y = "latitude_dd"),
  #     crs = sf::st_crs(crs_latlon)
  #   )
  x <- noaa_afsc_catch |>
  dplyr::filter(year > 2000 & srvy %in% srvy_in) |> 
  dplyr::select(longitude_dd, latitude_dd, depth_m) |>
  stats::na.omit() |>
  sf::st_as_sf(coords = c(x = "longitude_dd", y = "latitude_dd"), 
               crs = crs_latlon)
  x_sp <- as(x, "Spatial")

  idw_fit <- gstat::gstat(formula = depth_m ~ 1, locations = x, nmax = 4)

  depth_raster <- raster::predict(
    idw_fit,
    sp_extrap_raster
  ) |>
    # as(sp_extrap_raster, Class = "SpatialPoints")) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = crs_latlon) |>
    stars::st_rasterize()
  # plot(extrap_data0, main = "Interpolated Bottom Depths") 
  # Just so we can see what we are looking at:

  pred_grid_depth <- stars::st_extract(
    x = depth_raster,
    at = as.matrix(pred_grid[, c("longitude_dd", "latitude_dd")])
  ) |>
    dplyr::bind_cols(pred_grid) |>
    dplyr::rename(depth_m = var1.pred) |>
    dplyr::select(-var1.var) |>
    stats::na.omit()

  names(pred_grid_depth) <- tolower(names(pred_grid_depth))

  # save(depth_raster, file = here::here("inst", "exdata", "grids","grid_depth",
  # paste0("noaa_afsc_", tolower(srvy_out),"_depth_raster.rdata")))

  obj_name <- paste0("noaa_afsc_", tolower(srvy_out), "_pred_grid_depth")
  assign(x = obj_name, value = pred_grid_depth)
  dat <- pred_grid_depth
  title <- paste0("Prediction grid for ", srvy_out, " survey")
  author <- paste0("Alaska Fisheries Science Center, compiled by Emily ",
                   "Markowitz (Emily.Markowitz AT noaa.gov)")
  source <- paste0("https://github.com/afsc-gap-products/gap_products and ",
                   "https://github.com/afsc-gap-products/akgfmaps")
  details <- paste0("The Resource Assessment and Conservation Engineering ",
                    "(RACE) Division Groundfish Assessment Program (GAP) of ",
                    "the Alaska Fisheries Science Center (AFSC) conducts ",
                    "fisheries-independent bottom trawl surveys to assess the ",
                    "populations of demersal fish and crab stocks of Alaska.")
  description <- paste0("Vector geometries ('shapefiles') that are commonly ",
                        "needed for mapping and spatial analysis in Alaska ",
                        "marine management areas, marine statistical areas, ",
                        "and fishery-independent survey regions in Alaska.")

  data_documentation(dat, title, obj_name, author, source, details, description)
  # save(pred_grid_depth, file = here::here("data", paste0(obj_name, ".rdata")))

  return(list(
    "pred_grid_depth" = pred_grid_depth,
    "depth_raster" = depth_raster,
    "obj_name" = obj_name
  ))
}

## GOA Extrapolation grid ------------------------------------------------------

pred_grid <- utils::read.csv(here::here(
  "inst",
  "exdata",
  "grids",
  "orig",
  "goa_2025_interpolation_grid.csv"
))

pred_grid <- pred_grid |>
  dplyr::rename(longitude_dd = lon, latitude_dd = lat) |>
  dplyr::select(-X, -Y)

a <- extrap_grid(
  pred_grid = pred_grid,
  srvy_in = "GOA",
  noaa_afsc_catch = noaa_afsc_catch
)

obj_name <- paste0("noaa_afsc_goa_pred_grid_depth")
assign(x = obj_name, value = a$pred_grid_depth)
save(
  noaa_afsc_goa_pred_grid_depth,
  file = here::here("data", paste0(obj_name, ".rdata"))
)

## AI Extrapolation grid -------------------------------------------------------

load(
  here::here("inst", "exdata", "grids", "orig", "aleutian_islands_grid.rda"),
  verbose = TRUE
)

pred_grid <- data.frame(aleutian_islands_grid) |>
  dplyr::rename(longitude_dd = Lon, latitude_dd = Lat, area_km2 = Area_km2)

a <- extrap_grid(
  pred_grid = pred_grid,
  srvy_in = "AI",
  noaa_afsc_catch = noaa_afsc_catch
)

obj_name <- paste0("noaa_afsc_ai_pred_grid_depth")
assign(x = obj_name, value = a$pred_grid_depth)
save(
  noaa_afsc_ai_pred_grid_depth,
  file = here::here("data", paste0(obj_name, ".rdata"))
)

## EBS Extrapolation grid ------------------------------------------------------

load(
  here::here("inst", "exdata", "grids", "orig", "eastern_bering_sea_grid.rda"),
  verbose = TRUE
)

pred_grid_ebs <- pred_grid <- data.frame(eastern_bering_sea_grid) |>
  dplyr::rename(
    longitude_dd = Lon,
    latitude_dd = Lat,
    stratum = Stratum,
    area_km2 = Area_in_survey_km2
  )

a <- extrap_grid(
  pred_grid = pred_grid,
  srvy_in = "EBS",
  noaa_afsc_catch = noaa_afsc_catch
)

obj_name <- paste0("noaa_afsc_ebs_pred_grid_depth")
assign(x = obj_name, value = a$pred_grid_depth)
save(
  noaa_afsc_ebs_pred_grid_depth,
  file = here::here("data", paste0(obj_name, ".rdata"))
)

## NBS Extrapolation grid ------------------------------------------------------

load(
  here::here("inst", "exdata", "grids", "orig", "northern_bering_sea_grid.rda"),
  verbose = TRUE
)

pred_grid_nbs <- pred_grid <- data.frame(northern_bering_sea_grid) |>
  dplyr::rename(
    longitude_dd = Lon,
    latitude_dd = Lat,
    stratum = Stratum,
    area_km2 = Area_in_survey_km2
  )

a <- extrap_grid(
  pred_grid = pred_grid,
  srvy_in = "NBS",
  noaa_afsc_catch = noaa_afsc_catch
)

obj_name <- paste0("noaa_afsc_nbs_pred_grid_depth")
assign(x = obj_name, value = a$pred_grid_depth)
save(
  noaa_afsc_nbs_pred_grid_depth,
  file = here::here("data", paste0(obj_name, ".rdata"))
)

## BS (NBS+EBS) Extrapolation grid ---------------------------------------------

# pred_grid <- dplyr::bind_rows(
#   pred_grid_nbs |> dplyr::mutate(srvy_in  = "NBS"),
#   pred_grid_ebs |> dplyr::mutate(srvy_in  = "EBS"))

crs_latlon <- "+proj=longlat +datum=WGS84" # decimal degrees
shp <- akgfmaps::get_base_layers(select.region = "bs.all", set.crs = crs_latlon)
bs_all <- shp$survey.area
names(bs_all) <- tolower(names(bs_all))
sf::st_write(
  bs_all,
  here::here("inst", "exdata", "grids", "orig", "bs_all.shp"),
  append = FALSE
)

pred_grid <- FishStatsUtils::convert_shapefile(
  file_path = here::here("inst", "exdata", "grids", "orig", "bs_all.shp"),
  quiet = FALSE
)
pred_grid <- pred_grid$extrapolation_grid |>
  dplyr::rename(longitude_dd = Lon, latitude_dd = Lat, area_km2 = Area_km2) |>
  dplyr::select(-N_km, -E_km, -Include)

a <- extrap_grid(
  pred_grid = pred_grid,
  srvy_in = c("NBS", "EBS"),
  srvy_out = "BS",
  noaa_afsc_catch = noaa_afsc_catch
)

obj_name <- paste0("noaa_afsc_bs_pred_grid_depth")
assign(x = obj_name, value = a$pred_grid_depth)
save(
  noaa_afsc_bs_pred_grid_depth,
  file = here::here("data", paste0(obj_name, ".rdata"))
)

# Load Design-based age composition data ---------------------------------------

# TODO: This is something local on Em's computer - figure out how to do this
# in a GHA - might need a to store some secrets so it can run
locations <- c("Z:/Projects/ConnectToOracle.R")
for (i in seq_along(locations)) {
  if (file.exists(locations[i])) {
    source(locations[i])
  }
}

noaa_afsc_age <- RODBC::sqlQuery(
  channel = channel_products,
  query = paste0(
    "SELECT DISTINCT
bb.SURVEY_DEFINITION_ID,
bb.AREA_ID,
bb.SPECIES_CODE,
bb.SEX,
bb.AGE,
bb.YEAR,
bb.LENGTH_MM_MEAN,
bb.LENGTH_MM_SD,
bb.POPULATION_COUNT,
bb.AREA_ID_FOOTPRINT
FROM GAP_PRODUCTS.AKFIN_AGECOMP bb

WHERE bb.AREA_ID IN (99901, 99902, 99903, 99904)
AND bb.SPECIES_CODE IN (",
    paste0(spp_list$species_code, collapse = ","),
    ")
"
  )
) |>
  #   -- WHERE bb.SURVEY_DEFINITION_ID = 98
  # -- AND bb.SPECIES_CODE IN (21740, 10210, 69322)
  # -- AND AREA_ID = 99901
  # -- AND bb.year >= 1982
  janitor::clean_names()

# Save table to local directory
dat <- noaa_afsc_age
title <- "AFSC age composition estimate data"
obj_name <- "noaa_afsc_age"
author <- paste0("Alaska Fisheries Science Center, compiled by Emily Markowitz",
                 " (Emily.Markowitz AT noaa.gov)")
source <- "https://github.com/afsc-gap-products/gap_products"
details <- paste0("The Resource Assessment and Conservation Engineering (RACE)",
                  " Division Groundfish Assessment Program (GAP) of the Alaska",
                  " Fisheries Science Center (AFSC) conducts fisheries-",
                  "independent bottom trawl surveys to assess the populations ",
                  "of demersal fish and crab stocks of Alaska.")
description <- paste0("The final, validated survey data are publicly ",
                      "accessible soon after surveys are completed on the ",
                      "Fisheries One Stop Shop (FOSS) platform. This data ",
                      "includes catch, haul, and environmental data collected ",
                      "at each station. On the FOSS data platform, users can ",
                      "interactively select, view, and download data. ",
                      "Descriptive documentation and user-examples are ",
                      "available on the metadata page.")

save(noaa_afsc_age, file = here::here("data", paste0(obj_name, ".rdata")))
data_documentation(dat, title, obj_name, author, source, details, description)

# Load Design-based length composition data ------------------------------------

noaa_afsc_length <- RODBC::sqlQuery(
  channel = channel_products,
  query = paste0(
    "SELECT DISTINCT
bb.SURVEY_DEFINITION_ID,
bb.AREA_ID,
bb.SPECIES_CODE,
bb.YEAR,
bb.LENGTH_MM,
bb.SEX,
bb.POPULATION_COUNT
FROM GAP_PRODUCTS.AKFIN_SIZECOMP bb

WHERE bb.AREA_ID IN (99901, 99902, 99903, 99904)
AND bb.SPECIES_CODE IN (",
    paste0(spp_list$species_code, collapse = ","),
    ")
"
  )
) |>
  #   -- WHERE bb.SURVEY_DEFINITION_ID = 98
  # -- AND bb.SPECIES_CODE IN (21740, 10210, 69322)
  # -- AND AREA_ID = 99901
  # -- AND bb.year >= 1982
  janitor::clean_names()

# Save table to local directory
dat <- noaa_afsc_length
title <- "AFSC length (size) estimate data"
obj_name <- "noaa_afsc_length"
author <- paste0("Alaska Fisheries Science Center, compiled by Emily Markowitz",
                 " (Emily.Markowitz AT noaa.gov)")
source <- "https://github.com/afsc-gap-products/gap_products"
details <- paste0("The Resource Assessment and Conservation Engineering (RACE)",
                  " Division Groundfish Assessment Program (GAP) of the Alaska",
                  " Fisheries Science Center (AFSC) conducts fisheries-",
                  "independent bottom trawl surveys to assess the populations ",
                  "of demersal fish and crab stocks of Alaska.")
description <- paste0("The final, validated survey data are publicly ",
                      "accessible soon after surveys are completed on the ",
                      "Fisheries One Stop Shop (FOSS) platform. This data ",
                      "includes catch, haul, and environmental data collected ",
                      "at each station. On the FOSS data platform, users can ",
                      "interactively select, view, and download data. ",
                      "Descriptive documentation and user-examples are ",
                      "available on the metadata page.")

save(noaa_afsc_length, file = here::here("data", paste0(obj_name, ".rdata")))
data_documentation(dat, title, obj_name, author, source, details, description)
