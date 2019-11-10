# Required Packages -------------------------------------------------------
library(readr) # Data Wrangling
library(dplyr)
library(rlang)
# Data Wrangling Section --------------------------------------------------
#' @title Data Cleansing
#' @description This function return a dataframw with fixed date and coordinate format
#' @param rawData a filename. It uses data from the NOAA site.
#'
#' @example
#' \dontrun{
#'  eq_clean_data("/data/signif.txt")
#' }
#'
#'
# This function merge dates information and change the latitude and longitude to numeric
#`
eq_clean_data <- function(rawData){
  dset <- rawData %>%
    dplyr::mutate_(
      year_fix = ~stringr::str_pad(as.character(abs(YEAR)),
                                   side = "left", pad = "0"),
      date_paste = ~paste(year_fix, MONTH, DAY, sep = "-"),
      DATE = ~lubridate::ymd(date_paste, truncated = 2)) %>%
    dplyr::select_(quote(-year_fix), quote(-date_paste))

  lubridate::year(dset$DATE) <- dset$YEAR

  dset <- dset %>%
    dplyr::mutate_(LATITUDE = ~as.numeric(LATITUDE),
                   LONGITUDE = ~as.numeric(LONGITUDE))

  dset <- eq_location_clean(dset)

  return(dset)
}


#'@title Location column cleansing
#'@description This function change the location column into a clean location name bar the country name
#'@param rawData a filename. It uses data from the NOAA site. Can also be object produce by the \code{eq_clean_data}
#'
#'@example
#'\dontrun{
#' eq_location_clean("/data/signif.txt")
#'}
# This function cleans out the Location Name character so it only shows the city or locations
eq_location_clean <- function(rawData){

  rawData %>%
  dplyr::mutate(LOCATION_NAME = ~LOCATION_NAME %>%
           stringr::str_replace(paste0(rlang::.data$COUNTRY, ":"), "") %>%
           stringr::str_trim("both") %>%
           stringr::str_to_title())

}
