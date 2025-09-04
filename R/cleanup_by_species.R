#' Cleanup species function
#'
#' Filter catch by species, create a vector of tows and give tows a random
#' assignment to be resampled,
#'
#' @param catch full catch df
#' @param spp_info A data frame containing information about the test species.
#' @param seq_from A numeric value specifying the start of the sequence for
#' data frames.
#' @param seq_to A numeric value specifying the end of the sequence for data
#' frames.
#' @param seq_by A numeric value specifying the step size of the sequence for
#' data frames.
#' @param tot_dataframes the number of data frames you want to output. Effort x
#' replicates - (replicates - 1). 5x3-2
#' @param replicate_num An integer specifying the number of replicates.
#'
#' @importFrom dplyr filter bind_rows
#' @importFrom purrr map2 map
#'
#' @examples
#' catch <- surveyresamplr::noaa_nwfsc_catch
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
#' cleanup_by_species(
#'   catch = catch,
#'   spp_info = spp_info,
#'   seq_from = 0.1,
#'   seq_to = 1,
#'   seq_by = 0.1,
#'   tot_dataframes = 91,
#'   replicate_num = 10
#' )
#' @export
#' @return List of resampled catch dataframes
#'
cleanup_by_species <- function(
  catch,
  spp_info,
  seq_from = 0.1,
  seq_to = 1.0,
  seq_by = 0.1,
  tot_dataframes = 91,
  replicate_num = 10
) {
  # Filter the catch data frame by species
  df <- catch |>
    dplyr::filter(
      common_name == spp_info$common_name
    )

  # Implement latitude and depth filters
  if (!is.na(spp_info$filter_lat_lt) | is.null(spp_info$filter_lat_lt)) {
    df <- df |> dplyr::filter(latitude_dd < spp_info$filter_lat_lt)
  }
  if (!is.na(spp_info$filter_lat_gt) | is.null(spp_info$filter_lat_gt)) {
    df <- df |> dplyr::filter(latitude_dd > spp_info$filter_lat_gt)
  }
  if (!is.na(spp_info$filter_depth) | is.null(spp_info$filter_depth)) {
    df <- df |> dplyr::filter(depth_m < spp_info$filter_depth)
  }

  # Split the data frame by year
  catch_split <- base::split(df, df$year)

  # Use tow_fn to create a vector of tows for including or excluding.
  tows <- base::lapply(catch_split, tow_fn)

  # Assign random 1s and 0s based on the specified proportions to a list of
  # dataframes
  props <- as.data.frame(seq(from = seq_from, to = seq_to, by = seq_by))
  names(props) <- "trawlid"

  # match the structure of the catch data
  props <- base::rep(props, length(tows))

  tows_assigned <- purrr::map2(
    tows,
    props,
    include_or_exclude,
    replicate_num = replicate_num
  )
  # tows_assigned <- purrr::pmap(list(.x = tows, .y = props,
  #                                   .z = replicate_num),
  #                              include_or_exclude)

  # remove replicates of the 1 effort level
  tows_assigned <- base::lapply(tows_assigned, function(x) {
    x <- x[1:tot_dataframes]
    return(x)
  })

  tows_assigned_resampled <- purrr::map(tows_assigned, function(x) {
    purrr::map(x, function(y) {
      y[y$RandomAssignment == 1, ]
    })
  })

  tows_assigned_resampled <- unlist(tows_assigned_resampled, recursive = FALSE)

  alldata_resampled <- join_dfs(tows_assigned_resampled, df, "trawlid")

  names(alldata_resampled) <- base::substr(names(alldata_resampled), 6, 50)
  # it would be good to replace 50 with a logical indicating the end

  species_all_yrs <- alldata_resampled |>
    dplyr::bind_rows(.id = "source")

  # Split the data frame by source
  species_all_yrs <- base::split(species_all_yrs, species_all_yrs$source)

  names(species_all_yrs) <- base::gsub(
    x = names(species_all_yrs),
    pattern = ".",
    replacement = "",
    fixed = TRUE
  )

  # Remove dfs and lists so they don't take up memory
  rm("df", "catch_split", "tows", "props", "tows_assigned", "alldata_resampled")

  return(species_all_yrs)
}


#' Tow Function
#'
#' Create a vector of tows for including or excluding.
#'
#' @param x catch_split data frame
#' @return data frame of tows to include or exclude
#'
#'
tow_fn <- function(x) {
  tows <- as.data.frame(x$trawlid)
  tows <- unique(tows)
  tows <- as.data.frame(tows[!is.na(tows)])
  names(tows) <- "trawlid"
  return(tows)
}


#' Join Data Frames
#'
#' Function to join a list of data frames to a main data frame using a shared
#' column.
#'
#' @param list_of_dfs The list of data frames that you would like to join to
#' the main data frame
#' @param main_df The main data frame that you want to join the list of data
#' frames
#' @param shared_column The column that all the data frames share which will be
#' used to join the data frames together.
#'
#' @return A list of merged dataframes by the shared_column
#'
#'
join_dfs <- function(list_of_dfs, main_df, shared_column) {
  # Merge the list of data frames with the main data frame using the
  # shared_column
  merged_dfs <- base::lapply(list_of_dfs, function(df) {
    merged_df <- base::merge(df, main_df, by = shared_column)
    return(merged_df)
  })
  return(merged_dfs)
}
