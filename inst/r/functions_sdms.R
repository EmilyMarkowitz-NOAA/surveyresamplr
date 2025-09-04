#' Species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Used for arrowtooth flounder, bocaccio, dover sole, lingcod north, lingcod
#' south, longnose skate, pacific ocean perch (pop), pacific spiny dogfish, rex
#' sole, yellowtail.
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-
#' processing of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @importFrom sdmTMB sdmTMB
#'
species_sdm_fn <- function(x, mesh) {
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(year) + pass,
    data = x,
    mesh = mesh,
    family = delta_gamma(),
    time = "year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid"))
  )
  return(fit)
}


#' Species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Used for arrowtooth flounder, bocaccio, dover sole, lingcod north, lingcod
#' south, longnose skate, pacific ocean perch (pop), pacific spiny dogfish, rex
#' sole, yellowtail.
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-
#' processing of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @importFrom sdmTMB sdmTMB
#'
species_sdm_fn_ak <- function(x, mesh) {
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(year),
    data = x,
    mesh = mesh,
    family = delta_gamma(),
    time = "year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid"))
  )
  return(fit)
}

#' Species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Used for arrowtooth flounder, bocaccio, dover sole, lingcod north, lingcod
#' south, longnose skate, pacific ocean perch (pop), pacific spiny dogfish, rex
#' sole, yellowtail.
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-
#' processing of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @importFrom sdmTMB sdmTMB
#'
species_sdm_fn_ak_temperature <- function(x, mesh) {
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(year) + bottom_temperature_c,
    data = x,
    mesh = mesh,
    family = delta_gamma(),
    time = "year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid"))
  )
  return(fit)
}


#' Species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Used for arrowtooth flounder, bocaccio, dover sole, lingcod north, lingcod
#' south, longnose skate, pacific ocean perch (pop), pacific spiny dogfish, rex
#' sole, yellowtail.
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-
#' processing of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @importFrom sdmTMB sdmTMB
#'
species_sdm_fn_nwa <- function(x, mesh) {
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(year),
    data = x,
    mesh = mesh,
    family = delta_gamma(),
    time = "year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid"))
  )
  return(fit)
}

#' Species distribution model function using delta_lognormal family
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Used for petrale sole, sablefish.
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-
#' processing of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @importFrom sdmTMB sdmTMB
#'
species_sdm_lognormal_fn <- function(x, mesh) {
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(year) + pass,
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid"))
  )
  return(fit)
}

#' Canary species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Different from species_sdm_lognormal_fn in that the spatiotemporal
#' list is c("iid", "off").
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-
#' processing of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @importFrom sdmTMB sdmTMB
#'
canary_sdm_fn <- function(x, mesh) {
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(year) + pass,
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "year",
    anisotropy = FALSE,
    spatiotemporal = as.list(c("iid", "off"))
  )
  return(fit)
}

#' Darkbloched species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Different from species_sdm_lognormal_fn in that the spatiotemporal
#' list is c("off", "iid").
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-
#' processing of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @importFrom sdmTMB sdmTMB
#'
darkblotched_sdm_fn <- function(x, mesh) {
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(year) + pass,
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("off", "iid"))
  )
  return(fit)
}

#' Shortspine species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Different from species_sdm_lognormal_fn in that the model contains
#' depth_m and depth_m^2
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-
#' processing of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @importFrom sdmTMB sdmTMB
#'
shortspine_sdm_fn <- function(x, mesh) {
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(year) + pass + depth_m + (depth_m^2),
    data = x,
    mesh = mesh,
    family = delta_lognormal(),
    time = "year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("iid", "iid"))
  )
  return(fit)
}


#' Shortspine species distribution model function
#'
#' Function to create a mesh, fit the sdmTMB model, and get the index.
#' Exports fit.rds and index.rds files to the designated species folder.
#' Different from species_sdm_fn in that the mesh is 200 n_knots and the
#' spatiotemporal list is c("off", "off")
#'
#' @param x speciesname_df[[i]] which is a data frame from a list of data frames
#' created from the cleanup_by_species() function and any further post-
#' processing of depth filters (see the smaller_function.R file for those).
#' @param y speciesname_files[[i]] which is an item in a list created from
#' names(speciesname_df)
#' @importFrom sdmTMB sdmTMB
#'
widow_sdm_fn <- function(x, mesh) {
  fit <- sdmTMB::sdmTMB(
    total_catch_wt_kg ~ 0 + factor(year) + pass,
    # fyear and pass_scaled were specified in the configs doc.
    data = x,
    mesh = mesh,
    family = delta_gamma(),
    time = "year",
    anisotropy = TRUE,
    spatiotemporal = as.list(c("off", "off"))
  )
  return(fit)
}
