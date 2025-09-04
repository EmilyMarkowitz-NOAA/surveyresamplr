#' Plot Results and Save Figures
#'
#' This function compiles results from species distribution models (SDMs) output
#' of the clean_and_resample function for which it generates plots, and saves 
#' the figures.
#'
#' @param srvy A character string specifying the survey identifier.
#' @param dir_out A character string specifying the directory for output files.
#' @param dir_final A character string specifying the directory for new result 
#' files.
#' @importFrom ggplot2 ggplot aes geom_boxplot facet_wrap labs 
#' scale_color_discrete geom_line theme_bw theme element_text ggsave
#' @importFrom dplyr bind_rows
#' @importFrom utils write.csv
#'
#' @details
#' This function performs the following steps:
#' \itemize{
#'   \item Creates a directory for saving images.
#'   \item Searches for relevant files in the output directory.
#'   \item Compiles data from the found files.
#'   \item Saves the compiled data into CSV files.
#'   \item Generates various plots such as boxplots and time series of biomass 
#'   estimates.
#'   \item TO DO: CREATE BIO PLOTS
#'   \item Saves the generated plots as PNG files.
#'   \item Saves the list of plots in an RData file.
#' }
#' @return A list of plots and tables generated during the function execution.
#' @examples
#' dir_out <- here::here("vignettes", "output")
#' srvy <- "CA"
#' plot_results(srvy = srvy, dir_out = dir_out)
#' @export
plot_results <- function(srvy, dir_out, dir_final = NULL) {
  # create directory for images to be saved to
  if (is.null(dir_final)) {
    dir_final <- paste0(dir_out, paste0(srvy, "_0results/"))
  }

  dir.create(dir_final, showWarnings = FALSE)

  # find files
  aaa <- base::list.files(path = dir_out, pattern = srvy, full.names = TRUE)
  aaa <- aaa[!base::grepl(pattern = "figures", x = aaa)]
  aaa <- aaa[!base::grepl(pattern = ".txt", x = aaa)]
  aaa <- aaa[!base::grepl(pattern = "0results", x = aaa)]

  # compile files for each org
  fit_df <- fit_pars <- fit_check <- index <- data.frame()
  for (i in seq_along(aaa)) {
    if (file.exists(paste0(aaa[i], "/fit_df.csv"))) {
      fit_df <- fit_df |>
        dplyr::bind_rows(utils::read.csv(paste0(aaa[i], "/fit_df.csv")))
    }
    if (file.exists(paste0(aaa[i], "/fit_pars.csv"))) {
      fit_pars <- fit_pars |>
        dplyr::bind_rows(utils::read.csv(paste0(aaa[i], "/fit_pars.csv")))
    }
    if (file.exists(paste0(aaa[i], "/fit_check.csv"))) {
      fit_check <- fit_check |>
        dplyr::bind_rows(utils::read.csv(paste0(aaa[i], "/fit_check.csv")))
    }
    if (file.exists(paste0(aaa[i], "/index.csv"))) {
      index <- index |>
        dplyr::bind_rows(utils::read.csv(paste0(aaa[i], "/index.csv")))
    }
  }

  utils::write.csv(x = fit_df, file = paste0(dir_final, "/fit_df.csv"))
  utils::write.csv(x = fit_pars, file = paste0(dir_final, "/fit_pars.csv"))
  utils::write.csv(x = fit_check, file = paste0(dir_final, "/fit_check.csv"))
  utils::write.csv(x = index, file = paste0(dir_final, "/index.csv"))

  table_list <- list(
    "fit_df" = fit_df,
    "fit_pars" = fit_pars,
    "fit_check" = fit_check,
    "index" = index
  )

  # plotting -------------------------------------------------------------------

  plot_list <- c()
  i <- 0
  theme_custom <- ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  ## log biomass estimates boxplot ---------------------------------------------
  p1 <- ggplot2::ggplot(
    data = index,
    mapping = ggplot2::aes(
      x = as.factor(effort),
      y = log_est,
      color = effort
    )
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~common_name, scales = "free_y") +
    ggplot2::labs(
      x = "Proprotion of effort",
      y = "Log biomass estimate"
    ) +
    # ggplot2::scale_color_viridis_d(
    #   name = "Effort",
    #   option = "D") +
    ggplot2::scale_color_discrete(name = "Effort") +
    theme_custom

  i <- i + 1
  plot_list[[i]] <- p1
  names(plot_list)[i] <- "index_boxplot_log_biomass"

  # log(?) SE boxplot ----------------------------------------------------------
  p1 <- ggplot2::ggplot(
    data = index,
    mapping = ggplot2::aes(
      x = as.factor(effort),
      y = se,
      color = effort
    )
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~common_name, scales = "free_y") +
    ggplot2::labs(
      x = "Proprotion of effort",
      y = "Standard error of log biomass estimate"
    ) +
    ggplot2::scale_color_discrete(name = "Effort") +
    # ggplot2::scale_color_viridis_d(
    #   name = "Effort",
    #   option = "D") +
    theme_custom

  i <- i + 1
  plot_list[[i]] <- p1
  names(plot_list)[i] <- "index_boxplot_log_biomass_SE"

  # biomass estimates boxplot --------------------------------------------------
  p1 <- ggplot2::ggplot(
    data = index,
    mapping = ggplot2::aes(
      x = as.factor(effort),
      y = est,
      color = effort
    )
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~common_name, scales = "free_y") +
    ggplot2::labs(
      x = "Proprotion of effort",
      y = "Biomass estimate"
    ) +
    ggplot2::scale_color_discrete(name = "Effort") +
    # ggplot2::scale_color_viridis_d(
    #   name = "Effort",
    #   option = "D") +
    theme_custom

  i <- i + 1
  plot_list[[i]] <- p1
  names(plot_list)[i] <- "index_boxplot_biomass"

  # biomass estimates over time ------------------------------------------------
  p1 <- ggplot2::ggplot(
    data = index,
    mapping = ggplot2::aes(
      x = year,
      y = est,
      color = effort
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~common_name, scales = "free_y") +
    ggplot2::labs(
      x = "years",
      y = "Biomass estimate"
    ) +
    ggplot2::scale_color_discrete(name = "Effort") +
    # ggplot2::scale_color_viridis_d(
    #   name = "Effort",
    #   option = "D") +
    theme_custom +
    ggplot2::theme(legend.position = "right")

  i <- i + 1
  plot_list[[i]] <- p1
  names(plot_list)[i] <- "index_timeseries_biomass"

  # ADD BIO COMP PLOTS HERE ----------------------------------------------------

  # save -----------------------------------------------------------------------
  for (ii in seq_along(plot_list)) {
    ggplot2::ggsave(
      filename = paste0(names(plot_list)[ii], ".png"),
      plot = plot_list[[ii]],
      path = dir_final,
      width = 8,
      height = 8,
      device = "png",
      dpi = 300
    )
  }

  out <- list(
    "plots" = plot_list,
    "tables" = table_list
  )

  base::save(out, file = paste0(dir_final, "analysisoutput.rdata"))

  return(out)
}

# The following lines are used to declare global variables
# to avoid "no visible binding for global variable" warnings in R CMD check.
utils::globalVariables(
  c(
    "depth_m",
    "effort",
    "log_est",
    "se",
    "est",
    "year"
  )
)