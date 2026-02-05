# Mock species SDM wrapper function for testing
wrapper_sdmtmb <- function(x, y, z, dir_spp, spp_info, n_knots) {
  # Return a list with mock components
  list(
    fit = list(), # Empty object, to be handled by sdmTMB::tidy and sanity
    index = data.frame(year = 1:3, index = c(1,2,3))
  )
}

# Mock sdmTMB::tidy and sdmTMB::sanity
sdmTMB::tidy <- function(fit, conf.int = FALSE, effects = NULL) {
  # Return a simple tidy data frame
  data.frame(term = "intercept", estimate = 1.23)
}
sdmTMB::sanity <- function(fit) {
  # Return a simple sanity data frame
  data.frame(sane = TRUE)
}

# Setup test data
result <- tibble::tibble(
  source = c("0.1_2", "0.1_4", "0.1_4", "0.2_2", "0.2_3", "0.2_4", "0.2_4", "0.3_2", "0.3_3"),
  trawlid = c(5, 1, 2, 5, 5, 1, 2, 5, 5),
  RandomAssignment = rep(1, 9),
  srvy = rep("CA", 9),
  common_name = rep("arrowtooth flounder", 9),
  total_catch_numbers = c(130, 100, 120, 130, 130, 100, 120, 130, 130),
  total_catch_wt_kg = c(40, 10, 20, 40, 40, 10, 20, 40, 40),
  latitude_dd = c(38, 35, 36, 38, 38, 35, 36, 38, 38),
  longitude_dd = c(-125.3, -124.8, -124.9, -125.3, -125.3, -124.8, -124.9, -125.3, -125.3),
  year = c(2001, 2001, 2002, 2001, 2001, 2001, 2002, 2001, 2001),
  pass = c(1, 1, 2, 1, 1, 1, 2, 1, 1),
  depth_m = c(150, 270, 130, 150, 150, 270, 130, 150, 150),
  area_swept_ha = c(1.6, 1.2, 1.5, 1.6, 1.6, 1.2, 1.5, 1.6, 1.6)
)
spp_dfs <- split(result, result$source)


spp_info <- data.frame(
    srvy = "CA",
    common_name = "arrowtooth flounder",
    filter_lat_gt = 34,
    filter_lat_lt = NA,
    filter_depth = NA,
    model_fn = "total_catch_wt_kg ~ 0 + factor(year) + pass",
    model_family = "delta_gamma",
    model_anisotropy = TRUE,
    model_spatiotemporal = "iid, iid",
    stringsAsFactors = FALSE
  )

grid_yrs <- data.frame(
  longitude_dd = c(-124.81, -124.85, -125.32, -125.02, -124.55, -123.99, -125.76, -125.46, -124.36, -124.08),
  latitude_dd  = c(46.85, 47.60, 48.25, 47.81, 46.42, 45.59, 48.08, 47.94, 43.75, 44.67),
  pass         = rep(0, 10),
  depth_m      = c(-159, -112, -97, -30, -191, -35, -655, -335, -110, -11),
  area_km2     = rep(0, 10),
  srvy         = rep("CA", 10)
)

dir_out <- tempdir()

test_that("resample_tests runs and creates output files", {
  # Remove any prior test directory
  test_dir <- paste0(dir_out, paste0(spp_info$srvy, "_Test_Species/"))
  if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)

  expect_no_error({
    resample_tests(
      spp_dfs = spp_dfs,
      spp_info = spp_info,
      grid_yrs = grid_yrs,
      dir_out = dir_out,
      test = TRUE,
      parallel = FALSE,
      n_knots = NULL,
      model_type = "wrapper_sdmtmb"
    )
  })

  # Check output files exist
  expect_true(file.exists(file.path(test_dir, "fit_df.csv")))
  expect_true(file.exists(file.path(test_dir, "fit_pars.csv")))
  expect_true(file.exists(file.path(test_dir, "fit_check.csv")))
  expect_true(file.exists(file.path(test_dir, "index.csv")))

  # Check that parquet files exist
  expect_true(file.exists(file.path(test_dir, "df_1.parquet")))
  expect_true(file.exists(file.path(test_dir, "df_2.parquet")))

  # Read and check CSV contents
  fit_df <- read.csv(file.path(test_dir, "fit_df.csv"))
  expect_true("estimate" %in% names(fit_df))
  fit_index <- read.csv(file.path(test_dir, "index.csv"))
  expect_true(all(c("year", "index") %in% names(fit_index)))
})

test_that("select_n_knots returns expected values", {
  expect_equal(select_n_knots(500), 50)
  expect_equal(select_n_knots(3000), 100)
  expect_equal(select_n_knots(9000), 200)
  expect_equal(select_n_knots(20000), 500)
  expect_equal(select_n_knots(100000), 1000)
})

