#' Species distribution model function using tinyVAST
#'
#' Function to create a mesh, fit the WHAM model, and get the index.
#' TOLEDO: need to build out, this is just a holding spot
#' Exports fit.rds and index.rds files to the designated species folder.
#' Learn more: https://github.com/vast-lib/tinyVAST
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-
#' processing of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' @param z A dataframe with the new data to predict on.
#' @param dir_spp A character string specifying the directory for output files.
#' @param spp_info information about the species test run.
#' @param n_knots Numeric. Default  = 500.
#' names(speciesname_df)
#' @import tinyVAST
#' @importFrom sdmTMB make_mesh sdmTMB get_index
#' @importFrom stats predict
#' @export
#' @examples
#' \dontrun{
#' wrapper_tinyVAST() # TO DO: NEED EXAMPLE OF HOW TO USE
#' }
wrapper_tinyVAST <- function(x, y, z, dir_spp, spp_info, n_knots = 500) {
  # make mesh
  mesh <- sdmTMB::make_mesh(
    x,
    xy_cols = c("longitude_dd", "latitude_dd"),
    n_knots = n_knots
  )

  eval(parse(
    text = paste0(
      "fit <- sdmTMB::sdmTMB(
    ",
      spp_info$model_fn,
      ",
    data = x,
    mesh = mesh,
    family = ",
      spp_info$model_family,
      '(),
    time = "year",
    anisotropy = ',
      spp_info$model_anisotropy,
      ',
    spatiotemporal = as.list(c("',
      gsub(
        pattern = ", ",
        replacement = '", "',
        x = spp_info$model_spatiotemporal
      ),
      '"))
  )'
    )
  ))

  # get the index
  predictions <- stats::predict(fit, newdata = z, return_tmb_object = TRUE)
  index <- sdmTMB::get_index(
    predictions,
    area = z$area_km2,
    bias_correct = TRUE
  )

  # save file
  out <- list(
    "fit" = fit,
    "predictions" = predictions,
    "index" = index
  )
  saveRDS(
    out,
    paste0(dir_spp, "modelout_", y, ".rds")
  )
  # saveRDS(index, paste0(dir_spp, "index_", y, ".rds"))

  return(out)
}
