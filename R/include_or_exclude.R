#' Include or Exclude
#'
#' Specify how to downsample using output of tow_fn. For simple random sampling,
#' a proportion of tows should do. Utilized in the cleanup_by_species function.
#'
#' @param df tows data frame
#' @param proportions proportions developed using: props <- as.data.frame(seq
#' (0.1,1.0, by = 0.1)) replicated by the length of the tows dataframe. The name
#' of the props is "trawlid".
#' @param replicate_num going to be 10 for NWFSC and 3 for AK
#' @examples
#' catch_split <- base::split(catch, catch$year) # Catch data for 1 species
#' tows <- base::lapply(catch_split, tow_fn)
#' props <- as.data.frame(seq(from = 0.1, to = 1, by = 0.1))
#' names(props) <- "trawlid"
#' props <- base::rep(props, length(tows))
#' tows_assigned <- purrr::map2(tows, props, include_or_exclude,
#'   replicate_num = 10
#' )
#'
#' @export
#' @return List of dataframes with random assignments of which data to include
#'
include_or_exclude <- function(df, proportions, replicate_num) {
  # Get the number of rows in the dataframe
  num_rows <- nrow(df)

  # Use base::lapply to create a list of dataframes
  result_list <- base::lapply(proportions, function(p) {
    # Generate a random vector of 1s and 0s based on the specified proportion
    set.seed(1)
    random_vectors <- base::replicate(
      replicate_num,
      sample(c(1, 0), size = num_rows, replace = TRUE, prob = c(p, 1 - p)),
      simplify = FALSE
    )

    # Create a new dataframe with the random assignments
    base::lapply(random_vectors, function(rv) {
      cbind(df, RandomAssignment = rv)
    })
  })

  # flatten into single list
  result_list <- do.call(c, result_list)

  # Set names for the list elements based on proportions
  names(result_list) <- paste0(
    rep(proportions, each = replicate_num),
    "_",
    rep(1:replicate_num, times = length(proportions))
  )

  # Return the list of dataframes
  return(result_list)
}
