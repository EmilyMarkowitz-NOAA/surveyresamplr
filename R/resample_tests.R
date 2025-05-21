#' Resample Tests and Run SDM Processing
#'
#' This function resamples species data frames, runs species distribution models (SDMs) in parallel, and saves the results.
#'
#' @param spp_dfs A list of species data frames.
#' @param spp_info A data frame containing information about the test species.
#' @param grid_yrs A data frame or list containing grid years information.
#' @param dir_out A character string specifying the directory for output files.
#' @param test Logical. Default = FALSE. If TRUE, will only run first two resampling tests.
#' @param parallel Logical. Default = FALSE. If TRUE, will run models using \code{furrr::future_map()}.
#' @param n_knots Numeric. Default  = 500.
#' @param model_type String. Default = "wrapper_sdmtmb", but can be any preset wrapper_*() function or a premade home built function.
#'
#' @importFrom arrow write_parquet read_parquet
#' @importFrom future plan
#' @importFrom utils read.csv write.csv
#' @importFrom sdmTMB tidy sanity
#' @importFrom dplyr filter mutate across everything bind_rows bind_cols
#'
#' @export
#'
#' @details
#' This function performs the following steps:
#' \itemize{
#'   \item Sets up directories for output files.
#'   \item Reduces the list of data frames to the last two entries for testing purposes.
#'   \item Saves each data frame in Parquet format.
#'   \item Sets up parallel processing using the \code{furrr} package.
#'   \item Runs species distribution models (SDMs) in parallel.
#'   \item Saves the results of the SDM processing into CSV files.
#' }
#' @examples
#' \dontrun{
#' resample_tests() # TO DO: NEED EXAMPLE OF HOW TO USE
#' }
resample_tests <- function(spp_dfs, spp_info, grid_yrs, dir_out, test = FALSE, parallel = FALSE, n_knots = 500, model_type = "wrapper_sdmtmb") {
  # rename n_knots to knots or else wrapper function gets confused since it also has an n_knots
  knots <- n_knots

  # set directories for outputs

  dir_spp <- paste0(dir_out, paste0(spp_info$srvy, "_", spp_info$file_name, "/"))

  if (!dir.exists(dir_spp)) {
    dir.create(dir_spp, showWarnings = FALSE)
  }

  if (test) {
    spp_dfs <- spp_dfs[names(spp_dfs)[(length(names(spp_dfs)) - 1):length(names(spp_dfs))]] # reduce DFs for testing
  }
  spp_files <- as.list(names(spp_dfs)) # make the names file
  for (i in seq_along(spp_dfs)) { # Save each dataframe separately
    arrow::write_parquet(spp_dfs[[i]], paste0(dir_spp, paste0("df_", i, ".parquet")))
  }
  rm(spp_dfs) # Optional: Remove from memory
  gc()

  # set up parallel processing
  future::plan(future.callr::callr, workers = 6) # Adjust the number of workers based on available memory

  # Remove large objects before parallel execution
  gc()

  message("...Starting parallel SDM processing")

  assign(value = get(model_type), x = "wrapper_model")

  innards <- function(i, dir_spp, n_knots) {
    message(paste0("\n...", spp_files[[i]], "\n"))
    gc() # Free memory
    # Load only the required dataframe
    spp_df <- arrow::read_parquet(paste0(dir_spp, paste0("df_", i, ".parquet")))
    # Run species SDM function
    fit0 <- wrapper_model(
      x = spp_df,
      y = spp_files[[i]],
      z = grid_yrs,
      dir_spp = dir_spp,
      spp_info = spp_info,
      n_knots = knots
    )
    # fit <- readRDS(file = paste0(dir_spp, "fit_", spp_files[[i]], ".rds")) # for testing
    # index <- readRDS(file = paste0(dir_spp, "index_", spp_files[[i]], ".rds")) # for testing
    # fit0 <- list("fit" = fit, "index" = index)
    # Ensure extracted objects are dataframes, Store results in lists
    # fit
    if (!file.exists(paste0(dir_spp, "fit_df.csv"))) {
      fit_df <- c()
    } else {
      fit_df <- utils::read.csv(file = paste0(dir_spp, "fit_df.csv")) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    }
    fit_df <- fit_df |>
      dplyr::bind_rows(
        dplyr::bind_cols(
          spp_info |>
            dplyr::mutate(effort = as.character(spp_files[[i]])),
          data.frame(
            data.frame(sdmTMB::tidy(fit0$fit, conf.int = TRUE))
          )
        ) |>
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      )
    utils::write.csv(fit_df, file = paste0(dir_spp, "fit_df.csv"))
    # fit pars
    if (!file.exists(paste0(dir_spp, "fit_pars.csv"))) {
      fit_pars <- c()
    } else {
      fit_pars <- utils::read.csv(file = paste0(dir_spp, "fit_pars.csv")) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    }
    fit_pars <- fit_pars |>
      dplyr::bind_rows(
        dplyr::bind_cols(
          spp_info |>
            dplyr::mutate(effort = as.character(spp_files[[i]])),
          data.frame(
            data.frame(tidy(fit0$fit, effects = "ran_pars", conf.int = TRUE))
          )
        ) |>
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      )
    utils::write.csv(fit_pars, file = paste0(dir_spp, "fit_pars.csv"))
    # fit check
    if (!file.exists(paste0(dir_spp, "fit_check.csv"))) {
      fit_check <- c()
    } else {
      fit_check <- utils::read.csv(file = paste0(dir_spp, "fit_check.csv")) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    }
    fit_check <- fit_check |>
      dplyr::bind_rows(
        dplyr::bind_cols(
          spp_info |>
            dplyr::mutate(effort = as.character(spp_files[[i]])),
          data.frame(
            data.frame(sdmTMB::sanity(fit0$fit))
          )
        ) |>
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      )
    utils::write.csv(fit_check, file = paste0(dir_spp, "fit_check.csv"))
    # index
    if (!file.exists(paste0(dir_spp, "index.csv"))) {
      index <- c()
    } else {
      index <- utils::read.csv(file = paste0(dir_spp, "index.csv")) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    }
    index <- index |>
      dplyr::bind_rows(
        dplyr::bind_cols(
          spp_info |>
            dplyr::mutate(effort = as.character(spp_files[[i]])),
          data.frame(
            data.frame(fit0$index)
          )
        ) |>
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      )
    utils::write.csv(index, file = paste0(dir_spp, "index.csv"))
    # Explicitly remove objects after processing
    rm("fit0", "spp_df")
    gc()
  }

  if (parallel == TRUE) {
    # Run SDMs in parallel
    furrr::future_map(seq_along(spp_files), function(i) {
      innards(i, dir_spp, n_knots)
      # NULL
    }, .progress = TRUE, .options = furrr::furrr_options(seed = TRUE))
  } else {
    for (i in seq_along(spp_files)) {
      innards(i, dir_spp, n_knots)
    }
  }
  message("...Parallel SDM processing complete")
}
