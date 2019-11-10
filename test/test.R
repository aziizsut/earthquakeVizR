context("Test function for the Earthquake Viz") # with the testthat 2.01 it is not mandatory anymore

fnames <- "signif.txt"

test_that("eq_clean_data returns data frame class", {
  expect_is(eq_clean_data(fnames), "data.frame")
})

test_that("eq_clean_data$DATE is Date type", {
  expect_is(eq_clean_data(fnames)$DATE, "Date")
})

test_that("eq_clean_data returns numeric coordinates", {
  expect_is(eq_clean_data(fnames)$LATITUDE, "numeric")
  expect_is(eq_clean_data(fnames)$LONGITUDE, "numeric")
})

test_that("eq_location_clean returns a data frame", {
  expect_is(eq_location_clean(eq_clean_data(fnames)), "data.frame")
})

test_that("geom_timeline returns ggplot object", {
  g <- fnames %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY == "JAPAN", YEAR > 2005) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    geom_timeline()
  expect_is(g, "ggplot")
})

test_that("geom_timeline_label returns ggplot object", {
  g <- fnames %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY == "JAPAN", YEAR > 2005) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    geom_timeline_label(aes(label = LOCATION_NAME))
  expect_is(g, "ggplot")
})

test_that("theme_timeline returns ggplot object", {
  g <- fnames %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY == "JAPAN", YEAR > 2005) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    ))
  expect_is(g, "ggplot") # the return object should be in ggplot class
})

test_that("eq_map returns leaflet object", {
  leafy <- fnames %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(dates) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")
  expect_is(leafy, "leaflet") # the return object should be in a leaflet class
})

test_that("eq_create_label returns character vector", {
  expect_is(eq_create_label(eq_clean_data(fnames)), "character")
})
