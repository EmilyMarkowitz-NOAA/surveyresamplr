#' @title AFSC length (size) estimate data
#' @description The final, validated survey data are publicly accessible soon 
#' after surveys are completed on the Fisheries One Stop Shop (FOSS) platform. 
#' This data includes catch, haul, and environmental data collected at each 
#' station. On the FOSS data platform, users can interactively select, view, 
#' and download data. Descriptive documentation and user-examples are available 
#' on the metadata page.
#' @usage data('noaa_afsc_length')
#' @author Alaska Fisheries Science Center, compiled by Emily Markowitz 
#' (Emily.Markowitz AT noaa.gov)
#' @format A data frame with 81566 observations on the following 7 variables.
#' \describe{
#'   \item{\code{species_code}}{Taxon scientific name. The species code of the 
#' organism associated with the common_name and scientific_name columns.}
#'   \item{\code{year}}{Survey year. Year the observation (survey) was 
#' collected.}
#'   \item{\code{population_count}}{Estimated population. The estimated 
#' population caught in the survey for a species, group, or total for a given 
#' survey.}
#'   \item{\code{survey_definition_id}}{Survey ID. The survey definition ID key 
#' code is an integer that uniquely identifies a survey region/survey design. 
#' The column survey_definition_id is associated with the srvy and survey 
#' columns.}
#'   \item{\code{area_id}}{Area ID. Area ID key code for each statistical area 
#' used to produce production estimates (e.g., biomass, population, age comps, 
#' length comps). Each area ID is unique within each survey.}
#' }
#' @source https://github.com/afsc-gap-products/gap_products
#' @keywords species code data
#' @examples
#' data(noaa_afsc_length)
#' @details The Resource Assessment and Conservation Engineering (RACE) 
#' Division Groundfish Assessment Program (GAP) of the Alaska Fisheries 
#' Science Center (AFSC) conducts fisheries-independent bottom trawl surveys to 
#' assess the populations of demersal fish and crab stocks of Alaska.

"noaa_afsc_length"
