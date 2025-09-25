#' Resample Tests and Run SDM Processing
#'
#' This function resamples species data frames, runs species distribution models
#'  (SDMs) in parallel, and saves the results.
#'
#' @param spp_dfs A list of species data frames.
#' @param spp_info A data frame containing information about the test species.
#' @param grid_yrs A data frame or list containing grid years information.
#' @param dir_out A character string specifying the directory for output files.
#' @param test Logical. Default = FALSE. If TRUE, will only run first two
#' resampling tests.
#' @param parallel Logical. Default = FALSE. If TRUE, will run models using
#' \code{furrr::future_map()}.
#' @param n_knots Numeric. Default is to select the n_knots depending on the 
#' number of points in the data set.
#' @param model_type String. Default = "wrapper_sdmtmb", but can be any preset
#' wrapper_*() function or a premade home built function.
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
#'   \item Reduces the list of data frames to the last two entries for testing
#' purposes.
#'   \item Saves each data frame in Parquet format.
#'   \item Sets up parallel processing using the \code{furrr} package.
#'   \item Runs species distribution models (SDMs) in parallel.
#'   \item Saves the results of the SDM processing into CSV files.
#' }
#' @examples
#' \dontrun{
#' resample_tests() # TO DO: NEED EXAMPLE OF HOW TO USE
#' }
resample_tests <- function(spp_dfs, spp_info, grid_yrs, dir_out, test = FALSE,
                           parallel = FALSE, n_knots = 500,
                           model_type = "wrapper_sdmtmb") {
  # rename n_knots to knots or else wrapper function gets confused since it
  # also has an n_knots
    # Use the select_n_knots function if n_knots is NULL
  if (is.null(n_knots)) {
    knots <- select_n_knots(n_points = nrow(spp_dfs[[1]]))
  } else {
    knots <- n_knots
  }

  # set directories for outputs
  dir_spp <- paste0(
    dir_out,
    paste0(spp_info$srvy, "_", spp_info$file_name, "/")
  )

  if (!dir.exists(dir_spp)) {
    dir.create(dir_spp, showWarnings = FALSE)
  }

  # Handle test mode
  if (test) {
    spp_names <- names(spp_dfs)
    spp_dfs <- spp_dfs[spp_names[(length(spp_names) - 1):length(spp_names)]]
    # reduce DFs for testing
  }

  # Save dataframes as Parquet files
  spp_files <- as.list(names(spp_dfs)) # make the names file
  for (i in seq_along(spp_dfs)) { # Save each dataframe separately
    arrow::write_parquet(
      spp_dfs[[i]],
      paste0(dir_spp, paste0("df_", i, ".parquet"))
    )
  }
  rm(spp_dfs) # Optional: Remove from memory
  gc()

  # Define the parallel execution function
  run_parallel_models <- function(i) {
    # Create a unique temporary directory for this worker session
    temp_dir <- file.path(tempdir(), paste0("worker_", i))
    dir.create(temp_dir, showWarnings = FALSE)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
    
    # Copy the necessary file to the temporary directory
    file_to_process <- paste0("df_", i, ".parquet")
    file.copy(from = paste0(dir_spp, file_to_process), to = file.path(temp_dir, file_to_process))

    # Load required data from the temporary directory
    spp_df <- arrow::read_parquet(file.path(temp_dir, file_to_process))

    # Run species SDM function
    # Dynamically call the model wrapper function based on model_type
    model_func <- get(model_type, mode = "function")
    fit0 <- do.call(
      model_func,
      list(
        x = spp_df,
        y = spp_files[[i]],
        z = grid_yrs,
        dir_spp = dir_spp,
        spp_info = spp_info,
        n_knots = knots
      )
    )

    # Return a list of all results
    list(
      fit_df = dplyr::bind_cols(
        spp_info |> dplyr::mutate(effort = spp_files[[i]]),
        data.frame(sdmTMB::tidy(fit0$fit, conf.int = TRUE))
      ),
      fit_pars = dplyr::bind_cols(
        spp_info |> dplyr::mutate(effort = spp_files[[i]]),
        data.frame(sdmTMB::tidy(fit0$fit, 
                                effects = "ran_pars", 
                                conf.int = TRUE))
      ),
      fit_check = dplyr::bind_cols(
        spp_info |> dplyr::mutate(effort = spp_files[[i]]),
        data.frame(sdmTMB::sanity(fit0$fit))
      ),
      index = dplyr::bind_cols(
        spp_info |> dplyr::mutate(effort = spp_files[[i]]),
        data.frame(fit0$index)
      )
    )
  }

  message("...Starting SDM processing")
  if (parallel) {
    n_cores <- future::availableCores()
    n_workers <- round(n_cores * 0.75)
    if (Sys.info()['sysname'] == 'Windows') {
      future::plan(future::multisession, workers = n_workers)
      message("...Running in parallel with multisession")
    } else {
      # Use multicore for Linux/macOS for better performance
      future::plan(future::multicore, workers = n_workers)
      message("...Running in parallel with multicore")
    }
    results <- furrr::future_map(
      seq_along(spp_files),
      run_parallel_models,
      .progress = TRUE,
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
      results <- lapply(seq_along(spp_files), run_parallel_models)
  }

  message("...SDM processing complete. Saving results.")
  
  # Combine and save results in the main session
  all_fit_df <- purrr::map_dfr(results, "fit_df")
  all_fit_pars <- purrr::map_dfr(results, "fit_pars")
  all_fit_check <- purrr::map_dfr(results, "fit_check")
  all_index <- purrr::map_dfr(results, "index")

  utils::write.csv(all_fit_df, file = paste0(dir_spp, "fit_df.csv"), 
                   row.names = FALSE)
  utils::write.csv(all_fit_pars, file = paste0(dir_spp, "fit_pars.csv"), 
                   row.names = FALSE)
  utils::write.csv(all_fit_check, file = paste0(dir_spp, "fit_check.csv"),
                   row.names = FALSE)
  utils::write.csv(all_index, file = paste0(dir_spp, "index.csv"), 
                   row.names = FALSE)
  
  # Final memory cleanup
  rm(list = c("all_fit_df", "all_fit_pars", "all_fit_check", "all_index"))
  gc()
}

#' Automatically select number of knots for sdmTMB mesh
#'
#' @param n_points The number of data points in the dataset.
#' @return A numeric value representing the suggested number of knots.
#' 
#' @export
#'
#' @examples
#' # For a small dataset
#' select_n_knots(n_points = 500)
#'
#' # For a large dataset
#' select_n_knots(n_points = 10000)
#'
select_n_knots <- function(n_points) {
  if (n_points < 1000) {
    # For very small datasets, a small number of knots is sufficient.
    n_knots <- 50
  } else if (n_points < 5000) {
    # For small to medium datasets, a moderate number of knots works well.
    n_knots <- 100
  } else if (n_points < 10000) {
    # For larger datasets, increase the number of knots to capture spatial effects.
    n_knots <- 200
  } else if (n_points < 50000) {
    # For very large datasets, a larger mesh is needed.
    n_knots <- 500
  } else {
    # For extremely large datasets, a very large mesh might be necessary,
    # but be mindful of memory.
    n_knots <- 1000
  }
  return(n_knots)
}


#   # set up parallel processing
#   future::plan(future.callr::callr, workers = 6)
#   # Adjust the number of workers based on available memory
  
#   # Remove large objects before parallel execution
#   gc()

#   message("...Starting parallel SDM processing")

#   assign(value = get(model_type), x = "wrapper_model")

#   innards <- function(i, dir_spp, n_knots) {
#     message(paste0("\n...", spp_files[[i]], "\n"))
#     gc() # Free memory
#     # Load only the required dataframe
#     spp_df <- arrow::read_parquet(paste0(dir_spp, paste0("df_", i, ".parquet")))
#     # Run species SDM function
#     fit0 <- wrapper_model(
#       x = spp_df,
#       y = spp_files[[i]],
#       z = grid_yrs,
#       dir_spp = dir_spp,
#       spp_info = spp_info,
#       n_knots = knots
#     )
#     # fit <- readRDS(file = paste0(dir_spp, "fit_", spp_files[[i]], ".rds"))
#     #  # for testing
#     # index <- readRDS(file = paste0(dir_spp, "index_", spp_files[[i]], ".rds"))
#     # # for testing
#     # fit0 <- list("fit" = fit, "index" = index)
#     # Ensure extracted objects are dataframes, Store results in lists
#     # fit
#     if (!file.exists(paste0(dir_spp, "fit_df.csv"))) {
#       fit_df <- c()
#     } else {
#       fit_df <- utils::read.csv(file = paste0(dir_spp, "fit_df.csv")) |>
#         dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
#     }
#     fit_df <- fit_df |>
#       dplyr::bind_rows(
#         dplyr::bind_cols(
#           spp_info |>
#             dplyr::mutate(effort = as.character(spp_files[[i]])),
#           data.frame(
#             data.frame(sdmTMB::tidy(fit0$fit, conf.int = TRUE))
#           )
#         ) |>
#           dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
#       )
#     utils::write.csv(fit_df, file = paste0(dir_spp, "fit_df.csv"))
#     # fit pars
#     if (!file.exists(paste0(dir_spp, "fit_pars.csv"))) {
#       fit_pars <- c()
#     } else {
#       fit_pars <- utils::read.csv(file = paste0(dir_spp, "fit_pars.csv")) |>
#         dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
#     }
#     fit_pars <- fit_pars |>
#       dplyr::bind_rows(
#         dplyr::bind_cols(
#           spp_info |>
#             dplyr::mutate(effort = as.character(spp_files[[i]])),
#           data.frame(
#             data.frame(tidy(fit0$fit, effects = "ran_pars", conf.int = TRUE))
#           )
#         ) |>
#           dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
#       )
#     utils::write.csv(fit_pars, file = paste0(dir_spp, "fit_pars.csv"))
#     # fit check
#     if (!file.exists(paste0(dir_spp, "fit_check.csv"))) {
#       fit_check <- c()
#     } else {
#       fit_check <- utils::read.csv(file = paste0(dir_spp, "fit_check.csv")) |>
#         dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
#     }
#     fit_check <- fit_check |>
#       dplyr::bind_rows(
#         dplyr::bind_cols(
#           spp_info |>
#             dplyr::mutate(effort = as.character(spp_files[[i]])),
#           data.frame(
#             data.frame(sdmTMB::sanity(fit0$fit))
#           )
#         ) |>
#           dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
#       )
#     utils::write.csv(fit_check, file = paste0(dir_spp, "fit_check.csv"))
#     # index
#     if (!file.exists(paste0(dir_spp, "index.csv"))) {
#       index <- c()
#     } else {
#       index <- utils::read.csv(file = paste0(dir_spp, "index.csv")) |>
#         dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
#     }
#     index <- index |>
#       dplyr::bind_rows(
#         dplyr::bind_cols(
#           spp_info |>
#             dplyr::mutate(effort = as.character(spp_files[[i]])),
#           data.frame(
#             data.frame(fit0$index)
#           )
#         ) |>
#           dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
#       )
#     utils::write.csv(index, file = paste0(dir_spp, "index.csv"))
#     # Explicitly remove objects after processing
#     rm("fit0", "spp_df")
#     gc()
#   }

#   if (parallel == TRUE) {
#     # Run SDMs in parallel
#     furrr::future_map(seq_along(spp_files), function(i) {
#       innards(i, dir_spp, n_knots)
#       # NULL
#     }, .progress = TRUE, .options = furrr::furrr_options(seed = TRUE))
#   } else {
#     for (i in seq_along(spp_files)) {
#       innards(i, dir_spp, n_knots)
#     }
#   }
#   message("...Parallel SDM processing complete")
# }
