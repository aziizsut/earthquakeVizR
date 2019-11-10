#' Earthquake Timeline NOAA
#'
#' @description This geom plots Earthquake timeline filtered by countries and time
#'
#' @param x as Earthquake data points
#'
#' @inheritParams ggplot2::geom_point
#'
#' @details The function requires cleaned version of the dataset using \code{eq_clean_data} and \code{eq_clean_location}
#'
#' @export
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' data %>% eq_clean_data() %>%
#'    filter(COUNTRY == "JAPAN", YEAR > 2005) %>%
#'    ggplot(aes(x = DATE,
#'               y = COUNTRY,
#'               color = as.numeric(TOTAL_DEATHS),
#'               size = as.numeric(EQ_PRIMARY)
#'    )) +
#'    geom_timeline() +
#'    theme_timeline() +
#'    labs(size = "Richter scale value", color = "# deaths")
#' }
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @importFrom ggplot2 aes draw_key_point
#' @importFrom grid pointsGrob linesGrob gList gpar
#' @importFrom scales alpha
GeomTimeline <-
  ggplot2::ggproto(
    "GeomTimeline", ggplot2::Geom,
    required_aes = c("x"),
    default_aes = ggplot2::aes(colour = "grey", size = 1.5, alpha = 0.5,
                               shape = 21, fill = "grey", stroke = 0.5),
    draw_key = ggplot2::draw_key_point,
    draw_panel = function(data, panel_scales, coord) {

      if (!("y" %in% colnames(data))) {
        data$y <- 0.15
      }

      coords <- coord$transform(data, panel_scales)

      points <- grid::pointsGrob(
        coords$x, coords$y,
        pch = coords$shape, size = unit(coords$size / 4, "char"),
        gp = grid::gpar(
          col = scales::alpha(coords$colour, coords$alpha),
          fill = scales::alpha(coords$colour, coords$alpha)
        )
      )
      y_lines <- unique(coords$y)

      lines <- grid::polylineGrob(
        x = unit(rep(c(0, 1), each = length(y_lines)), "npc"),
        y = unit(c(y_lines, y_lines), "npc"),
        id = rep(seq_along(y_lines), 2),
        gp = grid::gpar(col = "grey",
                        lwd = .pt)
      )

      grid::gList(points, lines)
    }
  )



#' Earthquakes Label Timeline
#'
#' @description This geom plots timeline labels of earthquakes.
#' The function requires \code{geom_timeline} produced class
#'
#' @inheritParams ggplot2::geom_text

#' @param n_max An integer. Maximum number of earthquake to be plotted
#'
#' @details The function plots timeline labels of earthquakes based on cleaned
#' NOAA data. It uses thee product of \code{geom_timeline} and require \code{label} as input
#'
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' data %>% eq_clean_data() %>%
#'    filter(COUNTRY == "JAPAN", YEAR > 2005) %>%
#'    ggplot(aes(x = DATE,
#'               y = COUNTRY,
#'               color = as.numeric(TOTAL_DEATHS),
#'               size = as.numeric(EQ_PRIMARY)
#'    )) +
#'    geom_timeline() +
#'    geom_timeline_label(aes(label = LOCATION_NAME), n_max = 5) +
#'    theme_timeline() +
#'    labs(size = "Richter scale value", color = "No. of deaths")
#' }
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", ..., na.rm = FALSE,
                                n_max = NULL, show.legend = NA,
                                inherit.aes = TRUE) {

  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...)
  )
}

GeomTimelineLabel <-
  ggplot2::ggproto(
    "GeomTimelineLabel", ggplot2::Geom,
    required_aes = c("x", "label"),
    draw_key = ggplot2::draw_key_blank,
    setup_data = function(data, params) {
      if (!is.null(params$n_max)) {
        if (!("size" %in% colnames(data))) {
          stop(paste("'size' aesthetics needs to be",
                     "Define n_max."))
        }
        data <- data %>%
          dplyr::group_by_("group") %>%
          dplyr::top_n(params$n_max, size) %>%
          dplyr::ungroup()
      }
      data
    },
    draw_panel = function(data, panel_scales, coord, n_max) {

      if (!("y" %in% colnames(data))) {
        data$y <- 0.15
      }

      coords <- coord$transform(data, panel_scales)
      n_grp <- length(unique(data$group))
      offset <- 0.2 / n_grp

      lines <- grid::polylineGrob(
        x = unit(c(coords$x, coords$x), "npc"),
        y = unit(c(coords$y, coords$y + offset), "npc"),
        id = rep(1:dim(coords)[1], 2),
        gp = grid::gpar(
          col = "grey"
        )
      )

      names <- grid::textGrob(
        label = coords$label,
        x = unit(coords$x, "npc"),
        y = unit(coords$y + offset, "npc"),
        just = c("left", "bottom"),
        rot = 45
      )

      grid::gList(lines, names)
    }
  )
