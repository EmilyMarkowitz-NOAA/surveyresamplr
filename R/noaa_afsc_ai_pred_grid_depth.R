#' @title Prediction grid for AI survey
#' @description Vector geometries ('shapefiles') that are commonly needed for
#' mapping and spatial analysis in Alaska marine management areas, marine
#' statistical areas, and fishery-independent survey regions in Alaska.
#' @usage data('noaa_afsc_ai_pred_grid_depth')
#' @author Alaska Fisheries Science Center, compiled by Emily Markowitz
#' (Emily.Markowitz AT noaa.gov)
#' @format A data frame with 5302 observations on the following 7 variables.
#' \describe{
#' \item{\code{srvy}}{Abbreviated survey names. Abbreviated survey names.}
#' \item{\code{latitude_dd}}{Latitude (decimal degrees). Latitude (one
#' hundred thousandth of a decimal degree).}
#' \item{\code{longitude_dd}}{Longitude (decimal degrees). Longitude (one
#' hundred thousandth of a decimal degree).}
#' \item{\code{pass}}{Pass. Pass}
#' \item{\code{depth_m}}{Depth (m). Bottom depth (meters).}
#' \item{\code{stratum}}{Stratum. Statistical area for analyzing data. Strata
#' are often designed using bathymetry and other geographic and
#' habitat-related elements. The strata are unique to each survey region.}
#'  \item{\code{area_km2}}{Area (km2). Area in square kilometers.}
#' }
#' @source https://github.com/afsc-gap-products/gap_products and
#' https://github.com/afsc-gap-products/akgfmaps
#' @keywords species code data
#' @examples
#' data(noaa_afsc_ai_pred_grid_depth)
#' @details The Resource Assessment and Conservation Engineering (RACE)
#' Division Groundfish Assessment Program (GAP) of the Alaska Fisheries Science
#' Center (AFSC) conducts fisheries-independent bottom trawl surveys to assess
#' the populations of demersal fish and crab stocks of Alaska.

'noaa_afsc_ai_pred_grid_depth'
