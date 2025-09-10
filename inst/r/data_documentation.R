#' @title Data Documentation
#' @description This function generates a data documentation file for a given
#' dataset.
#' @param dat The dataset to document.
#' @param title The title of the dataset.
#' @param obj_name The name of the object in the documentation.
#' @param author The author of the dataset.
#' @param source The source of the dataset.
#' @param details Additional details about the dataset.
#' @param description A description of the dataset.
#'

data_documentation <- function(
       dat,
       title,
       obj_name,
       author,
       source,
       details,
       description
) {
       column <- data.frame(
              metadata_colname = c(
                     "srvy",
                     "trawlid",
                     "common_name",
                     "species_code",
                     "total_catch_numbers",
                     "total_catch_wt_kg",
                     "cpue_kgkm2",
                     "latitude_dd",
                     "longitude_dd",
                     "year",
                     "pass",
                     "bottom_temperature_c",
                     "depth_m",
                     "geometry",
                     "stratum",
                     "area_km2",
                     "biomass_mt",
                     "biomass_var",
                     "population_count",
                     "population_var",
                     "survey_definition_id",
                     "area_id",
                     "file_name",
                     "salinity_bottom",
                     "age",
                     "length_cm",
                     "sex",
                     "area_swept_ha",
                     "total_catch_numbers",
                     "project"
              ),
              metadata_colname_long = c(
                     "Abbreviated survey names",
                     "Trawl ID",
                     "Taxon common name",
                     "Taxon scientific name",
                     "Taxon count",
                     "Specimen weight (g)",
                     "Weight CPUE (kg/km2)",
                     "Latitude (decimal degrees)",
                     "Longitude (decimal degrees)",
                     "Survey year",
                     "Pass",
                     "Bottom temperature (degrees Celsius)",
                     "Depth (m)",
                     "Spatial geometry",
                     "Stratum",
                     "Area (km2)",
                     "Estimated biomass",
                     "Estimated biomass variance",
                     "Estimated population",
                     "Estimated population variance",
                     "Survey ID",
                     "Area ID",
                     "File name",
                     "Bottom salinity",
                     "Age",
                     "Length (cm)",
                     "Sex",
                     "Area swept (ha)",
                     "Total catch (numbers)",
                     "Project"
              ),
              metadata_colname_desc = c(
                     "Abbreviated survey names.",
                     paste0(
                            "This is a unique numeric identifier ",
                            "assigned to each (vessel, cruise, and ",
                            "haul) combination."
                     ),
                     paste0(
                            "The common name of the marine organism ",
                            "associated with the scientific_name and ",
                            "species_code columns."
                     ),
                     paste0(
                            "The species code of the organism ",
                            "associated with the common_name and ",
                            "scientific_name columns."
                     ),
                     paste0(
                            "Total whole number of individuals caught",
                            " in haul or samples collected."
                     ),
                     "Weight of specimen (grams).",
                     paste0(
                            "Catch weight (kilograms) per unit effort",
                            " (area swept by the net, units square ",
                            "kilometers)."
                     ),
                     paste0(
                            "Latitude (one hundred thousandth of a ",
                            "decimal degree)."
                     ),
                     paste0(
                            "Longitude (one hundred thousandth of a ",
                            "decimal degree)."
                     ),
                     "Year the observation (survey) was collected.",
                     "Pass",
                     paste0(
                            "Bottom temperature (tenths of a degree ",
                            "Celsius); NA indicates removed or ",
                            "missing values."
                     ),
                     "Bottom depth (meters).",
                     "Spatial geometry.",
                     paste0(
                            "Statistical area for analyzing data. ",
                            "Strata are often designed using ",
                            "bathymetry and other geographic and ",
                            "habitat-related elements. The strata ",
                            "are unique to each survey region."
                     ),
                     "Area in square kilometers.",
                     "The estimated total biomass.",
                     paste0(
                            "The estimated variance associated with ",
                            "the total biomass."
                     ),
                     paste0(
                            "The estimated population caught in the ",
                            "survey for a species, group, or total ",
                            "for a given survey."
                     ),
                     paste0(
                            "The estimated population variance ",
                            "caught in the survey for a species, ",
                            "group, or total for a given survey."
                     ),
                     paste0(
                            "The survey definition ID key code is an",
                            " integer that uniquely identifies a ",
                            "survey region/survey design. The column",
                            " survey_definition_id is associated with",
                            " the srvy and survey columns."
                     ),
                     paste0(
                            "Area ID key code for each statistical",
                            " area used to produce production ",
                            "estimates (e.g., biomass, population, ",
                            "age comps, length comps). Each area ID ",
                            "is unique within each survey."
                     ),
                     "Name of origonal source file.",
                     paste0(
                            "Bottom salinity (parts per million); NA",
                            " indicates removed or missing values."
                     ),
                     "Age of fish (years).",
                     "Length of fish in centimeters.",
                     "Sex of fish F = female, M = male, U = unsexed.",
                     "Area swept for each tow in hectares.",
                     "Total catch in numbers for each tow.",
                     paste0(
                            "Survey project name. This is exclusively",
                            " used for NWFSC surveys."
                     )
              )
       )

       column <- column[column$metadata_colname %in% names(dat), ]

       str0 <- paste0(
              "#' @title ",
              title,
              "
          #' @description ",
              description,
              "
          #' @usage data('",
              obj_name,
              "')
          #' @author ",
              author,
              "
          #' @format A data frame with ",
              nrow(dat),
              " observations on the following ",
              ncol(dat),
              " variables.
          #' \\describe{
          ",
              paste0(
                     paste0(
                            "#'   \\item{\\code{",
                            column$metadata_colname,
                            "}}{",
                            column$metadata_colname_long,
                            ". ",
                            column$metadata_colname_desc,
                            "}"
                     ),
                     collapse = "\n"
              ),
              "   }
          #' @source ",
              source,
              "
          #' @keywords species code data
          #' @examples
          #' data(",
              obj_name,
              ")
          #' @details ",
              details
       )

       str0_wrapped <- wrap_lines(str0, width = 78)
       writeLines(
              c(str0_wrapped, "", paste0("'", obj_name, "'")),
              con = here::here("R", paste0(obj_name, ".R"))
       )
}


wrap_lines <- function(text, width = 78) {
  # Split the text into lines
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))
  wrapped <- unlist(lapply(lines, function(line) {
    if (grepl("^#'", line)) {
      # Remove the #' and possible following space
      content <- sub("^#'\\s*", "", line)
      # Wrap the content (without prefix)
      wrapped_content <- strwrap(content, width = width - 3)
      # Prepend #' to every wrapped line
      paste0("#' ", wrapped_content)
    } else {
      # For any line not starting with #', just output as is 
      # (or wrap without prefix)
      strwrap(line, width = width)
    }
  }))
  # Always add #' to any line that doesn't already have it 
  # (e.g., for empty lines)
  wrapped <- ifelse(grepl("^#'", wrapped), wrapped, paste0("#' ", wrapped))
  paste0(wrapped, collapse = "\n")
}