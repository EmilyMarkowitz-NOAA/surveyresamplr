# test-wrapper_sdmtmb.R
# Test for wrapper_sdmtmb using testthat

test_that("wrapper_sdmtmb runs and returns expected output structure with a 
          larger n and spatial signal", {
  # Skip test if sdmTMB not installed
  skip_if_not_installed("sdmTMB")

  # Minimal synthetic data for x (fit), z (prediction), and dummy area
  set.seed(42)
  # Increase n to a value suitable for a spatial model, e.g., 2000
  n <- 2000
  df_x <- data.frame(
    longitude_dd = runif(n, -126, -124),
    latitude_dd = runif(n, 48, 50),
    year = sample(2010:2015, n, replace = TRUE),
    area_km2 = runif(n, 1, 10)
  )

  # Add a spatial signal to the catch data
  # Catch is now a function of latitude
  df_x$catch <- rpois(n, 10 * (df_x$latitude_dd - 48))

  df_z <- data.frame(
    longitude_dd = runif(n, -126, -124),
    latitude_dd = runif(n, 48, 50),
    year = sample(2010:2015, n, replace = TRUE),
    area_km2 = runif(n, 1, 10)
  )
  # Dummy y and dir_spp
  y <- "test"
  dir_spp <- tempdir() # Use temp directory for testing
  on.exit(unlink(dir_spp, recursive = TRUE), add = TRUE)

  # Minimal species info for sdmTMB - simplified to poisson for robustness
  spp_info <- list(
    model_fn = "catch ~ 1",
    model_family = "poisson",
    model_anisotropy = "FALSE",
    model_spatiotemporal = "off"
  )

  # Remove any output file if it exists
  out_file <- file.path(dir_spp, paste0("modelout_", y, ".rds"))
  if (file.exists(out_file)) file.remove(out_file)

  # Run the function with a reasonable number of knots for a larger n
  result <- wrapper_sdmtmb(
    x = df_x,
    y = y,
    z = df_z,
    dir_spp = dir_spp,
    spp_info = spp_info,
    n_knots = 100
  )

  # Check output structure
  expect_type(result, "list")
  expect_named(result, c("fit", "predictions", "index"))

  # Check output files
  expect_true(file.exists(out_file))

  # Check that fit and predictions are sdmTMB objects
  expect_s3_class(result$fit, "sdmTMB")
  expect_s3_class(result$predictions, "sdmTMB")

  # Check index is a data.frame
  expect_s3_class(result$index, "data.frame")
})

test_that("wrapper_sdmtmb selects n_knots automatically when NULL", {
  skip_if_not_installed("sdmTMB")
  n <- 2000
  df_x <- data.frame(
    longitude_dd = runif(n, -126, -124),
    latitude_dd = runif(n, 48, 50),
    year = sample(2010:2015, n, replace = TRUE),
    area_km2 = runif(n, 1, 10)
  )

  # Add a spatial signal to the catch data
  df_x$catch <- rpois(n, 10 * (df_x$latitude_dd - 48))

  df_z <- df_x
  y <- "auto_knots"

  dir_spp <- tempdir()
  on.exit(unlink(dir_spp, recursive = TRUE), add = TRUE)

  spp_info <- list(
    model_fn = "catch ~ 1",
    model_family = "poisson",
    model_anisotropy = "FALSE",
    model_spatiotemporal = "off"
  )
  # Should use select_n_knots for 2000 points (expect 100 knots)
  result <- wrapper_sdmtmb(
    x = df_x,
    y = y,
    z = df_z,
    dir_spp = dir_spp,
    spp_info = spp_info,
    n_knots = NULL
  )
  expect_type(result, "list")
  expect_named(result, c("fit", "predictions", "index"))
  expect_true(file.exists(file.path(dir_spp, paste0("modelout_", y, ".rds"))))
})

test_that("select_n_knots returns expected values for different n_points", {
  expect_equal(select_n_knots(500), 50)
  expect_equal(select_n_knots(2000), 100)
  expect_equal(select_n_knots(7000), 200)
  expect_equal(select_n_knots(20000), 500)
  expect_equal(select_n_knots(60000), 1000)
})
