#' @title Combined NEFSC catch, haul, and species data from FOSS
#' @description ENTER.
#' @usage data('noaa_nefsc_catch')
#' @author Northeast Fisheries Science Center, compiled by Emily Markowitz
#' (Emily.Markowitz AT noaa.gov)
#' @format A data frame with 438975 observations on the following 13
#' variables.
#' \describe{
#' \item{\code{srvy}}{Abbreviated survey names. Abbreviated survey names. }
#' \item{\code{trawlid}}{Trawl ID. This is a unique numeric identifier
#' assigned to each (vessel, cruise, and haul) combination.}
#' \item{\code{common_name}}{Taxon common name. The common name of the marine
#' organism associated with the scientific_name and species_code columns.}
#' \item{\code{total_catch_numbers}}{Taxon count. Total whole number of
#' individuals caught in haul or samples collected.}
#' \item{\code{total_catch_wt_kg}}{Specimen weight (g). Weight of specimen
#' (grams).}
#' \item{\code{latitude_dd}}{Latitude (decimal degrees). Latitude (one
#' hundred thousandth of a decimal degree).}
#' \item{\code{longitude_dd}}{Longitude (decimal degrees). Longitude (one
#' hundred thousandth of a decimal degree).}
#' \item{\code{year}}{Survey year. Year the observation (survey) was
#' collected.}
#' \item{\code{pass}}{Pass. Pass}
#' \item{\code{bottom_temperature_c}}{Bottom temperature (degrees Celsius).
#' Bottom temperature (tenths of a degree Celsius); NA indicates removed or
#' missing values.}
#' \item{\code{depth_m}}{Depth (m). Bottom depth (meters).}
#' \item{\code{file_name}}{File name. Name of origonal source file.}
#' \item{\code{salinity_bottom}}{Bottom salinity. Bottom salinity (parts per
#' million); NA indicates removed or missing values.}
#' \item{\code{total_catch_numbers}}{Total catch (numbers). Total catch in
#' numbers for each tow.}#' }
#' @source Data request to the NEFSC
#' @keywords species code data
#' @examples
#' data(noaa_nefsc_catch)
#' @details ENTER.

"noaa_nefsc_catch"
