# Test for cleanup_by_species function in surveyresamplr

test_that("cleanup_by_species returns expected structure and filters correctly", {
  # Minimal spp_info row (arrowtooth flounder, filter latitude > 34)
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
  
  # Minimal catch data matching required structure
  catch <- data.frame(
    srvy = rep("CA", 5),
    trawlid = 1:5,
    common_name = c("arrowtooth flounder", "arrowtooth flounder", "arrowtooth flounder", "other fish", "arrowtooth flounder"),
    total_catch_numbers = c(100, 120, 110, 50, 130),
    total_catch_wt_kg = c(10, 20, 30, 5, 40),
    latitude_dd = c(35, 36, 33, 37, 38),
    longitude_dd = c(-124.8, -124.9, -124.7, -124.8, -125.3),
    year = c(2001, 2002, 2001, 2002, 2001),
    pass = c(1, 2, 1, 2, 1),
    depth_m = c(270, 130, 115, 200, 150),
    area_swept_ha = c(1.2, 1.5, 1.3, 1.4, 1.6),
    stringsAsFactors = FALSE
  )

  # Run function with reduced effort (for test speed)
  result <- cleanup_by_species(
    catch = catch,
    spp_info = spp_info,
    seq_from = 0.1,
    seq_to = 0.3,
    seq_by = 0.1,
    tot_dataframes = 11,
    replicate_num = 4
  )
  
  # Check output is a named list of data frames
  expect_type(result, "list")
  expect_true(all(vapply(result, is.data.frame, logical(1))))
  # Check output data frames have at least the columns expected from catch
  expect_true(all(c("trawlid", "common_name", "latitude_dd", "depth_m") %in% names(result[[1]])))
  
  # Check filter works (should only include common_name == "arrowtooth flounder" and latitude_dd > 34)
  all_lat <- unlist(lapply(result, function(df) df$latitude_dd))
  all_names <- unlist(lapply(result, function(df) df$common_name))
  expect_true(all(all_lat > 34))
  expect_true(all(all_names == "arrowtooth flounder"))
  
  # Check that list names are present
  expect_true(!is.null(names(result)))
})
