# Required Packages -------------------------------------------------------
library(readr) # Data Wrangling
library(dplyr)

#' @title Data Cleansing
#' @description This function return a dataframw with fixed date and coordinate format
#' @param rawData a filename. It uses data from the NOAA site.
#'
#' @example
#' \dontrun{
#'  eq_clean_data("/data/signif.txt")
#' }
# Data Wrangling Section --------------------------------------------------

# This function merge dates information and change the latitude and longitude to numeric
#`
eq_clean_data <- function(rawData){
  readr::read_delim(rawData, delim = "\t") %>%
    dplyr::mutate(dates = lubridate::dmy(paste0(DAY,"-", MONTH,"-", YEAR))) %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE),
           LONGITUDE = as.numeric(LONGITUDE))
}


# This function cleans out the Location Name character so it only shows the city or locations
eq_location_clean <- function(rawData){

  rawData %>%
  dplyr::mutate(LOCATION_NAME = LOCATION_NAME %>%
           stringr::str_replace(paste0(COUNTRY, ":"), "") %>%
           stringr::str_trim("both") %>%
           stringr::str_to_title())

}

devtools::document()
