#' Clean and Resample Species Data
#'
#' This function cleans up the catch data for a specific species and then
#' performs
#' resampling tests,
#'
#' @details
#' This function performs the following steps:
#' \itemize{
#'   \item Cleans up the catch data for the specified species using
#' `cleanup_by_species`.
#'   \item Performs resampling tests on the cleaned data using `resample_tests`.
#' }
#'
#' @param spp_info A data frame row containing information about the species.
#' @param catch A data frame containing the catch data.
#' @param seq_from A numeric value specifying the start of the sequence for
#' data frames.
#' @param seq_to A numeric value specifying the end of the sequence for data
#' frames.
#' @param seq_by A numeric value specifying the step size of the sequence for
#' data frames.
#' @param tot_dataframes An integer specifying the total number of data frames
#' to generate.
#' @param replicate_num An integer specifying the number of replicates.
#' @param grid_yrs A data frame or list containing grid years information.
#' @param dir_out A character string specifying the directory for output files.
#' @param test Logical. TRUE/FALSE. If TRUE, will only run first two resampling
#' tests.
#' @param n_knots Numeric. Default  = 500.
#' @param model_type String. Default = "wrapper_sdmtmb", but can be any preset
#' wrapper_*() function or a premade home built function.
#' @param bio A data frame containing the biological data, if applicable. NULL
#' is default.
#' @export
#' @importFrom dplyr filter mutate bind_rows
#' @importFrom utils write.csv
#' @return A list of data frames containing the cleaned and resampled catch
#' data.
#' @examples
#' dir_out <- here::here("vignettes", "output")
#' catch <- surveyresamplr::noaa_nwfsc_catch
#' bio <- surveyresamplr::noaa_nwfsc_bio
#' grid_yrs <- sdmTMB::replicate_df(
#'   dat = surveyresamplr::noaa_nwfsc_catch, time_name = "year",
#'   time_values = unique(catch$year)
#' )
#' spp_list <- data.frame(
#'   srvy = "CA",
#'   common_name = "arrowtooth flounder",
#'   file_name = "arrowtooth_flounder",
#'   filter_lat_gt = 34,
#'   filter_lat_lt = NA,
#'   filter_depth = NA,
#'   model_fn = "total_catch_wt_kg ~ 0 + factor(year) + pass",
#'   model_family = "delta_gamma",
#'   model_anisotropy = TRUE,
#'   model_spatiotemporal = "iid, iid"
#' )
#' clean_and_resample(
#'   spp_info = spp_list,
#'   catch,
#'   seq_from = 0.1,
#'   seq_to = 1,
#'   seq_by = 0.1,
#'   tot_dataframes = 91,
#'   replicate_num = 10,
#'   grid_yrs = grid_yrs,
#'   dir_out = dir_out,
#'   bio = bio
#' )
#'
clean_and_resample <- function(
  spp_info,
  catch,
  seq_from = 0.1,
  seq_to = 1,
  seq_by = 0.1,
  tot_dataframes = 91,
  replicate_num = 10,
  grid_yrs,
  dir_out,
  test = FALSE,
  n_knots = 500,
  model_type = "wrapper_sdmtmb",
  bio = NULL
) {
  message(paste0(spp_info$srvy, " ", spp_info$common_name))
  # check input variables
  # do all of the model function variables exist in grid_yrs and the catch data?
  aa <- spp_info$model_fn
  aa <- gsub(x = aa, pattern = "factor(", replacement = "", fixed = TRUE)
  aa <- gsub(x = aa, pattern = ")", replacement = "", fixed = TRUE)
  aa <- gsub(x = aa, pattern = "^2", replacement = "", fixed = TRUE)
  aa <- gsub(x = aa, pattern = "(", replacement = "", fixed = TRUE)
  aa <- strsplit(x = aa, split = " ")[[1]]
  aa <- aa[which(!(aa %in% c("+", "0", "~", "total_catch_wt_kg")))]

  if (sum(aa %in% names(grid_yrs)) != length(aa)) {
    stop(paste0(
      "ERROR: Not all variables called in function are available in the ",
      "grid_yrs object: ",
      aa[!(aa %in% names(grid_yrs))]
    ))
    # CA shortspine thornyhead
    # Error in `map()`:
    #   ℹ In index: 1.
    # Caused by error in `clean_and_resample()`:
    #   ! ERROR: Not all variables called in function are available in the
    # grid_yrs object: (depth_m^2
    # Run `rlang::last_trace()` to see where the error occurred.
  }

  if (sum(aa %in% names(catch)) != length(aa)) {
    stop(paste0(
      "ERROR: Not all variables called in function are available in the ",
      "catch data object: ",
      aa[!(aa %in% names(catch))]
    ))
  }

  message("Starting cleanup of catch data")

  spp_dfs <- cleanup_by_species(
    catch = catch,
    spp_info = spp_info,
    seq_from = seq_from,
    seq_to = seq_to,
    seq_by = seq_by,
    tot_dataframes = tot_dataframes,
    replicate_num = replicate_num
  )

  if (!is.null(bio)) {
    message("Starting cleanup of biological composition data")
    bio_df <- bio |>
      dplyr::filter(common_name == spp_info$common_name) |>
      dplyr::mutate(
        trawlid = as.double(trawlid),
        latitude_dd = as.numeric(latitude_dd),
        depth_m = as.numeric(depth_m)
      )

    # Apply depth and latitude filters
    if (!is.na(spp_info$filter_lat_lt) | is.null(spp_info$filter_lat_lt)) {
      bio_df <- bio_df |> dplyr::filter(latitude_dd < spp_info$filter_lat_lt)
    }
    if (!is.na(spp_info$filter_lat_gt) | is.null(spp_info$filter_lat_gt)) {
      bio_df <- bio_df |> dplyr::filter(latitude_dd > spp_info$filter_lat_gt)
    }
    if (!is.na(spp_info$filter_depth) | is.null(spp_info$filter_depth)) {
      bio_df <- bio_df |> dplyr::filter(depth_m < spp_info$filter_depth)
    }

    bio_resampled <- lapply(spp_dfs, function(catch_data) {
      replicate_id <- unique(catch_data$source) # Get replicate ID
      catch_data$trawlid <- as.double(catch_data$trawlid)
      # Ensure trawlid is numeric
      matched_bio <- bio_df |> dplyr::filter(trawlid %in% catch_data$trawlid)
      matched_bio <- matched_bio |> dplyr::mutate(source = replicate_id)
      # Add replicate ID as a column
      return(matched_bio)
    })

    bio_spp_dfs <- dplyr::bind_rows(bio_resampled)

    dir_spp <- paste0(
      dir_out,
      paste0(spp_info$srvy, "_", spp_info$file_name, "/")
    )
    if (!dir.exists(dir_spp)) {
      dir.create(dir_spp, showWarnings = FALSE)
    }
    utils::write.csv(bio_spp_dfs, file = paste0(dir_spp, "bio.csv"))
    message(paste0("Resampled bio data CSV written to ", dir_spp, "bio.csv"))
  }

  try(
    {
      resample_tests(
        spp_dfs = spp_dfs,
        spp_info = spp_info,
        grid_yrs = grid_yrs,
        dir_out = dir_out,
        test = test,
        n_knots = n_knots,
        model_type = model_type
      )
    },
    silent = FALSE
  )
}
