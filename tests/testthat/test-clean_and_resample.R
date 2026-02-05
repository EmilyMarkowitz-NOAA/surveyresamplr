# Test for clean_and_resample function in surveyresamplr

test_that("clean_and_resample runs without error and processes realistic bio and catch data", {
  # Minimal spp_info row
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
  
  # Usable catch data matching noaa_nwfsc_catch structure
  catch <- data.frame(
    trawlid = 1:3,
    common_name = rep("arrowtooth flounder", 3),
    longitude_dd = c(-124.8186, -124.8533, -125.3244),
    latitude_dd = c(46.71917, 47.64056, 48.32917),
    year = 2001:2003,
    pass = c(1, 2, 1),
    area_swept_ha = c(1.2, 1.5, 1.3),
    total_catch_numbers = c(16, 22, 50),
    total_catch_wt_kg = c(28, 40, 60),
    depth_m = c(270, 130, 115),
    srvy = rep("CA", 3),
    stringsAsFactors = FALSE
  )
  
  # grid_yrs with required columns
  grid_yrs <- data.frame(
    longitude_dd = rep(c(-124.8186, -124.8533, -125.3244), each = 3),
    latitude_dd  = rep(c(46.71917, 47.64056, 48.32917), times = 3),
    pass         = rep(c(1, 2, 1), times = 3),
    depth_m      = rep(c(270, 130, 115), times = 3),
    area_km2     = runif(9, 2, 3), # random area between 2 and 3 km2
    stringsAsFactors = FALSE
  )

  # Usable bio data matching noaa_nwfsc_bio structure
  bio <- data.frame(
    trawlid = 1:3,
    common_name = rep("arrowtooth flounder", 3),
    longitude_dd = c(-124.9829, -124.8533, -125.0133),
    latitude_dd = c(47.01417, 47.72861, 47.84111),
    year = 2001:2003,
    pass = c(1, 2, 1),
    sex = c("F", "M", "F"),
    length_cm = c(56, 35, 31),
    age = c(11, 3, 4),
    depth_m = c(170, 100, 120),
    project = rep("NWFSC", 3),
    srvy = rep("CA", 3),
    stringsAsFactors = FALSE
  )

  # Temporary output directory
  dir_out <- withr::local_tempdir()

  # Mock dependencies
  stub(clean_and_resample, "cleanup_by_species", function(...) {
    list(
      data.frame(source = "rep1", trawlid = 1:3),
      data.frame(source = "rep2", trawlid = 4:6)
    )
  })
  stub(clean_and_resample, "resample_tests", function(...) {
    TRUE
  })

  # Run function and check that it doesn't error
  expect_no_error(
    clean_and_resample(
      spp_info = spp_info,
      catch = catch,
      seq_from = 0.1,
      seq_to = 1,
      seq_by = 0.1,
      tot_dataframes = 3,
      replicate_num = 2,
      grid_yrs = grid_yrs,
      dir_out = dir_out,
      bio = bio
    )
  )

  # Optionally, check that bio.csv was written (if bio is provided)
  bio_file <- file.path(dir_out, "CA_arrowtooth_flounder", "bio.csv")
  expect_true(file.exists(bio_file))

  # Check that the written bio.csv contains expected column names
  out_bio <- read.csv(bio_file)
  expect_true(all(c("trawlid", "common_name", "latitude_dd", "depth_m", "source") %in% names(out_bio)))
})