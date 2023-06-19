#' A function for generating radar plots with a shared scaled axis,
#' or multiple axis with real values displayed for each variable (spider chart).
#'
#' @param p_data A data frame containing plotting data.
#' with the first column containing the group/observation names,
#' and the other columns being descriptory variables for each group/observation.
#' @param polygon Whether to use circles or polygons as the background. Defaults to TRUE.
#' @param scaled Whether to display a single axis with scaled data or
#' individual axis (a radar or spider chart). Defaults to FALSE.
#' @param draw_axis Whether to draw variable axis or not. Defaults to TRUE.
#' @param background_color The background color of the chart. Defaults to "gray 99".
#' @param area_fill Whether or not to fill the shapes made by connecting data points. Defaults to TRUE.
#' @param fill_opacity How opaque are the shape fills. Defaults to 0.05.
#' @param central_distance The offset from the center of the radar chart. Defaults to 0.2.
#' @param axis_name_offset The axis name offset from the axis. Defaults to 0.2.
#' @param digit_rounding To how many digits are the numbers rounded. Defaults to 2.
#' @returns A ggplot chart.
#' @export
#' @examples
#' mtcars_gr <- mtcars %>%
#'  tibble::rownames_to_column(var = "group") %>%
#'  tibble::as_tibble() %>%
#'  tail(3) %>%
#' dplyr::select(1:7)
#'
#' ggspider(mtcars_gr, polygon = TRUE)
#'
#' ggspider(mtcars_gr, polygon = FALSE)
#'
#' ggspider(mtcars_gr, polygon = TRUE, scaled = TRUE)
#'
#' ggspider(mtcars_gr, polygon = FALSE, scaled = TRUE)
#'
#' iris_summary <- iris %>%
#'  dplyr::group_by(Species) %>%
#'  dplyr::summarise(across(everything(), mean))
#'
#' ggspider(iris_summary)

ggspider <- function(p_data,
                     polygon = TRUE,
                     scaled = FALSE,
                     draw_axis = TRUE,
                     background_color = "gray99",
                     area_fill = TRUE,
                     fill_opacity = 0.05,
                     central_distance = 0.2,
                     axis_name_offset = 0.2,
                     digit_rounding = 2
){

  legend_title <- names(p_data)[[1]]
  p_data <- p_data %>% dplyr::rename(group = 1) %>% dplyr::mutate(group = factor(group))

  circle_coords <- function(r, n_axis = ifelse(polygon == TRUE, ncol(p_data) - 1, 100)){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    x <- r*cos(fi)
    y <- r*sin(fi)

    tibble::tibble(x, y, r)
  }

  (step_1 <- purrr::map_df(seq(0, 1, 0.25) + central_distance, circle_coords) %>%
      ggplot2::ggplot(ggplot2::aes(x, y)) +
      ggplot2::geom_polygon(data = circle_coords(1 + central_distance), alpha = 1, fill = background_color, lty = 2) +
      ggplot2::geom_path(ggplot2::aes(group = r), lty = 2, alpha = 0.5) +
      ggplot2::theme_void())


  axis_coords <- function(n_axis){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
    x1 <- central_distance*cos(fi)
    y1 <- central_distance*sin(fi)
    x2 <- (1 + central_distance)*cos(fi)
    y2 <- (1 + central_distance)*sin(fi)

    tibble::tibble(x = c(x1, x2), y = c(y1, y2), id = rep(1:n_axis, 2))
  }

  text_data <- p_data %>%
    dplyr::select(-group) %>%
    purrr::map_df(~ min(.) + (max(.) - min(.)) * seq(0, 1, 0.25)) %>%
    dplyr::mutate(r = seq(0, 1, 0.25)) %>%
    tidyr::pivot_longer(-r, names_to = "parameter", values_to = "value")

  text_coords <- function(r, n_axis = ncol(p_data) - 1){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
    x <- r*cos(fi)
    y <- r*sin(fi)

    tibble::tibble(x, y, r = r - central_distance)
  }

  labels_data <- purrr::map_df(seq(0, 1, 0.25) + central_distance, text_coords) %>%
    dplyr::bind_cols(text_data %>% dplyr::select(-r))


  rescaled_coords <- function(r, n_axis){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    tibble::tibble(r, fi) %>% dplyr::mutate(x = r*cos(fi), y = r*sin(fi)) %>% dplyr::select(-fi)
  }

  rescaled_data <- p_data %>%
    dplyr::mutate(across(-group, scales::rescale)) %>%
    dplyr::mutate(copy = dplyr::pull(., 2)) %>% #da se moze geom_path spojiti opet na pocetnu tocku
    tidyr::pivot_longer(-group, names_to = "parameter", values_to = "value") %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(coords = rescaled_coords(value + central_distance, ncol(p_data) - 1)) %>%
    tidyr::unnest(cols = c(coords))

  step_1 +
    {if(draw_axis == TRUE) ggplot2::geom_line(data = axis_coords(ncol(p_data) - 1), ggplot2::aes(x, y, group = id), alpha = 0.3)} +
    ggplot2::geom_point(data = rescaled_data, ggplot2::aes(x, y, group = group, col = group), size = 3) +
    ggplot2::geom_path(data = rescaled_data, ggplot2::aes(x, y, group = group, col = group), size = 1) +
    {if(area_fill == TRUE) ggplot2::geom_polygon(data = rescaled_data, ggplot2::aes(x, y, group = group, col = group, fill = group), size = 1, alpha = fill_opacity, show.legend = FALSE)} +
    {if(scaled == TRUE){
      ggplot2::geom_text(data = labels_data %>% dplyr::filter(parameter == labels_data$parameter[[1]]), ggplot2::aes(x, y, label = r), alpha = 0.65,
                         family = theme_get()$text[["family"]],
                         size = theme_get()$text[["size"]]/2.75,
                         fontface ="plain")
    }
      else{
        ggplot2::geom_text(data = labels_data, ggplot2::aes(x, y, label = round(value, digit_rounding)), alpha = 0.65,
                           family = theme_get()$text[["family"]],
                           size = theme_get()$text[["size"]]/2.75,
                           fontface ="plain")
      }
    } +
    ggplot2::geom_text(data = text_coords(1 + central_distance + axis_name_offset), ggplot2::aes(x, y), label = labels_data$parameter[1:(ncol(p_data)-1)],
                       family = theme_get()$text[["family"]],
                       size = theme_get()$text[["size"]]/2.75,
                       fontface ="plain") +
    ggplot2::labs(col = legend_title) +
    ggplot2::theme(legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 12),
                   legend.title = ggplot2::element_text(size = 12))
}
