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

  data <- readr::read_delim(rawData, delim = "\t")

  dset <- data %>%
    dplyr::mutate(
      year_fix = stringr::str_pad(as.character(abs(YEAR)), width = 4,
                                   side = "left", pad = "0"),
      date_paste = paste(year_fix, MONTH, DAY, sep = "-"),
      DATE = lubridate::ymd(date_paste, truncated = 2))

  dset <- dset %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE),
                   LONGITUDE = as.numeric(LONGITUDE))

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
eq_location_clean <- function(dataset){

  dataset %>%
    tidyr::separate(LOCATION_NAME, into = c("country", "locationName"),
                    sep = ":") %>%
    dplyr::mutate(LOCATION_NAME = str_to_title(str_trim(locationName)))

}
