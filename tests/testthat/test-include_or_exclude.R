# Mock function for testing purposes
# This simulates the output of your tow_fn function
tow_fn_mock <- function(x) {
  df <- data.frame(trawlid = 1:5)
  return(df)
}

# Define test data
mock_catch_data <- data.frame(
  year = c(2000, 2000, 2001, 2001),
  trawlid = c(1, 2, 3, 4),
  common_name = c("walleye pollock", "walleye pollock", "walleye pollock",
                  "walleye pollock"),
  total_catch_weight = c(52, 10, 70, 40),
  stringsAsFactors = FALSE
)

# Run the preparatory steps that would happen before calling the function
catch_split <- base::split(mock_catch_data, mock_catch_data$year)
tows <- base::lapply(catch_split, tow_fn_mock)

props <- as.data.frame(seq(from = 0.1, to = 0.5, by = 0.1)) # Use a smaller sequence for testing
names(props) <- "trawlid"

proportions <- base::rep(props, length(tows))
replicate_num <- 3

# Run the function once for all tests to use
result_list <- include_or_exclude(
  df = tows[[1]],
  proportions = proportions,
  replicate_num = replicate_num
)

# Test suite for include_or_exclude
test_that("include_or_exclude produces a list of data frames with correct structure", {
  
  # Check that the output is a list
  expect_type(result_list, "list")
  
  # Check that each element of the list is a data frame
  expect_true(all(sapply(result_list, is.data.frame)))
  
  # Check that the number of data frames is correct
  # 5 proportions * 3 replicates = 15 data frames
  expect_length(result_list, length(proportions) * replicate_num)
  
  # Check the dimensions of the data frames
  # Each data frame should have 5 rows (from tows_fn_mock) and 2 columns
  expect_true(all(sapply(result_list, ncol) == 2))
  expect_true(all(sapply(result_list, nrow) == 5))
  
  # Check for the presence of the new 'RandomAssignment' column
  expect_true(all(sapply(result_list, function(df) "RandomAssignment" %in% names(df))))
})

test_that("RandomAssignment column contains only 1s and 0s", {
  # The values should be binary
  all_values <- unlist(sapply(result_list, `[[`, "RandomAssignment"))
  expect_true(all(all_values %in% c(0, 1)))
})

test_that("The names of the list elements are correctly formatted", {
  # The names should be in the format "proportion_replicate"
  expected_names <- c(
    "0.1_1", "0.1_2", "0.1_3",
    "0.2_1", "0.2_2", "0.2_3",
    "0.3_1", "0.3_2", "0.3_3",
    "0.4_1", "0.4_2", "0.4_3",
    "0.5_1", "0.5_2", "0.5_3"
  )
  expect_equal(names(result_list), expected_names)
})

test_that("RandomAssignment reflects the given proportion on average", {
  # This is a statistical test, so we can't expect perfect equality.
  # We test a single, well-defined proportion.
  p_test <- 0.5
  
  # Find all data frames for a specific proportion
  proportional_dfs <- result_list[grepl(paste0("^", p_test), names(result_list))]
  
  # Count the number of '1's across all replicates for this proportion
  total_ones <- sum(unlist(sapply(proportional_dfs, `[[`, "RandomAssignment")))
  
  # Total number of assignments for this proportion
  total_assignments <- length(proportional_dfs) * nrow(proportional_dfs[[1]])
  
  # The observed proportion should be close to the expected proportion
  observed_proportion <- total_ones / total_assignments
  
  # Due to random sampling, we use a tolerance.
  expect_equal(observed_proportion, p_test, tolerance = 0.1)
})

