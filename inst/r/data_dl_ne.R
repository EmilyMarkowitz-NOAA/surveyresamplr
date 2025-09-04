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
  "devtools",
  "plyr",
  "dplyr",
  "tidyr",
  "here",
  "readr",
  "tibble",
  "janitor",
  "data.table",
  "sp",
  "FishStatsUtils",
  "VAST"
)

# Use pkg_install() function found in functions.R file to load packages
source("./inst/r/pkg_install.R")
base::lapply(unique(PKG), pkg_install)

# Test species to pull ---------------------------------------------------------

spp_list <-
  data.frame(
    srvy = "NWA",
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
    file_name = c(
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
    )
  )

# Catch Load data --------------------------------------------------------------

aaa <- list.files(
  path = here::here("inst/exdata/ne_data/"),
  pattern = "NEFSCBottomTrawl",
  full.names = TRUE
)
aa <- list.files(
  path = here::here("inst/exdata/ne_data/"),
  pattern = "NEFSCBottomTrawl",
  full.names = FALSE
)

dat <- c()
for (i in seq_along(aaa)) {
  dat <- dplyr::bind_rows(
    dat,
    readr::read_csv(file = aaa[i]) |>
      dplyr::mutate(SVSPP = as.character(paste0(SVSPP))) |>
      dplyr::mutate(file = aa[i])
  )
}

noaa_nefsc_catch <- dat |>
  janitor::clean_names() |>
  dplyr::mutate(
    pass = vessel, # is this data set calibrated? who knows lol
    file_name = gsub(x = file, pattern = ".csv", replacement = ""),
    file_name = gsub(
      x = file_name,
      pattern = "NEFSCBottomTrawl",
      replacement = ""
    ),
    srvy = season,
    latitude_dd = latitude / 100,
    longitude_dd = longitude / 100 * -1,
    trawlid = paste0(cruise, "_", stratum, "_", station)
  ) |>
  dplyr::select(
    srvy,
    trawlid,
    file_name,
    total_catch_numbers = tot_n,
    total_catch_wt_kg = tot_kg,
    latitude_dd,
    longitude_dd,
    year,
    bottom_temperature_c = temp_bottom,
    salinity_bottom,
    pass,
    depth_m
  ) |>
  dplyr::left_join(
    y = spp_list |>
      dplyr::select(file_name, common_name = common_name)
  )
# Save data --------------------------------------------------------------------

dat <- noaa_nefsc_catch
obj_name <- "noaa_nefsc_catch"
title <- "Combined NEFSC catch, haul, and species data from FOSS"
author <- paste0(
  "Northeast Fisheries Science Center, compiled by Emily ",
  "Markowitz (Emily.Markowitz AT noaa.gov)"
)
source <- "Data request to the NEFSC"
details <- "ENTER."
description <- "ENTER."

save(noaa_nefsc_catch, file = here::here("data", paste0(obj_name, ".rdata")))
data_documentation(dat, title, obj_name, author, source, details, description)

# Make extrapolation grids -----------------------------------------------------

# this is a bit made up, it would be nice to get a real extrapolation grid from
# the center, but no avail

extrap_grid <- function(
  pred_grid,
  srvy,
  srvy_out = NULL,
  noaa_nefsc_catch,
  crs_proj = NULL
) {
  crs_latlon <- "+proj=longlat +datum=WGS84" # decimal degrees

  if (is.null(srvy_out)) {
    srvy_out <- srvy
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

  x <- noaa_nefsc_catch |>
    dplyr::filter(year > 2010 & srvy %in% c(srvy)) |>
    # lets assume our ability to assess depth has improved, technologically
    # since before 2000. year negotiable if anyone has strong opinions
    dplyr::select(longitude_dd, latitude_dd, depth_m) |>
    stats::na.omit() |>
    sf::st_as_sf(
      coords = c(x = "longitude_dd", y = "latitude_dd"),
      crs = sf::st_crs(crs_latlon)
    )

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
  # paste0("noaa_nefsc_", tolower(srvy_out),"_depth_raster.rdata")))

  obj_name <- paste0("noaa_nefsc_pred_grid_depth")
  assign(x = obj_name, value = pred_grid_depth)
  dat <- pred_grid_depth
  title <- paste0("Prediction grid for ", srvy_out, " survey")
  author <- paste0(
    "Northeast Fisheries Science Center, compiled by Emily ",
    "Markowitz (Emily.Markowitz AT noaa.gov)"
  )
  source <- "Data request to the NEFSC. "
  details <- "ENTER."
  description <- "ENTER."

  save(
    noaa_nefsc_pred_grid_depth,
    file = here::here("data", paste0(obj_name, ".rdata"))
  )
  data_documentation(dat, title, obj_name, author, source, details, description)

  return(list(
    "pred_grid_depth" = pred_grid_depth,
    "depth_raster" = depth_raster
  ))
}

# from https://github.com/NOAA-EDAB/survdat/tree/master/inst/extdata
# nwa_shp <- sf::st_read(dsn = here::here("grids","strata.shp"))

pred_grid <- FishStatsUtils::convert_shapefile(
  file_path = here::here("inst", "exdata", "grids", "orig", "strata.shp"),
  quiet = FALSE
)
pred_grid <- pred_grid$extrapolation_grid |>
  dplyr::rename(longitude_dd = Lon, latitude_dd = Lat, area_km2 = Area_km2) |>
  dplyr::select(-N_km, -E_km, -Include)

a <- extrap_grid(
  pred_grid = pred_grid,
  srvy = c("SPRING", "FALL"),
  srvy_out = "NWA",
  noaa_nefsc_catch = noaa_nefsc_catch
)
