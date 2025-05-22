#' Package install
#' 
#' @param p name of package
#' On google workstations you can use the library install bash script Elizabeth created. 
#' Copy that into you working
#' directory, run 'chmod u+x ubuntu_libraries.sh' and then run './ubuntu_libraries.sh'
#' @example 
#' pkgs <- c("dplyr, "nwfscSurvey", "sdmTMB)
#' base::lapply(pkgs, pkg_install)
#' 
pkg_install <- function(p){
  if(grepl("/home/user/", getwd())){
    system("chmod a+x ubuntu_libraries.sh")
    system("./ubuntu_libraries.sh")
  }
  if(!require(p, character.only = TRUE)) {
    if (p == 'coldpool') {
      devtools::install_github("afsc-gap-products/coldpool")
    } else if (p == "akgfmapas") {
      devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
    } else if (p == 'nwfscSurvey') {
      remotes::install_github("pfmc-assessments/nwfscSurvey")
    } else if (p == "gapctd") {
      devtools::install_github("afsc-gap-products/gapctd")
    } else if (p == 'gapindex') {
      remotes::install_github("afsc-gap-products/gapindex")
    } else if (p == "FishStatsUtils") {
      remotes::install_github("James-Thorson/FishStatsUtils")
    } else if (p == "surveyjoin") {
      pak::pkg_install("DFO-NOAA-Pacific/surveyjoin")
    } else {
      install.packages(p)
    }
    require(p, character.only = TRUE)}
}

