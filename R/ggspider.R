#' A function for generating radar/spider charts with a shared scaled axis,
#' or multiple axis with real values displayed for each variable (spider chart).
#'
#' @param p_data A data frame containing plotting data.
#' with the first column containing the group/observation names,
#' and the other columns being descriptory variables for each group/observation.
#' @param ci_data A data frame confidence interval data containing four columns.
#' The first column specifies the group, the second the feature, and the third and fourth left and
#' right confidence interval borders, respectively. See the vignette for an example. Defaults to NULL.
#' @param polygon Whether to use circles or polygons as the background. Defaults to TRUE.
#' @param scaled Whether to display a single axis with scaled data or
#' individual axis (a radar or spider chart). Defaults to FALSE.
#' @param draw_axis Whether to draw variable axis or not. Defaults to TRUE.
#' @param n_labels How many labels to use per axis. Defaults to 5.
#' @param zero_centered Whether to fix the smallest displayed value to 0 on each axis. Defaults to FALSE.
#' @param subset Names of row identifiers to plot as a subset of data,
#' but with axis range specified using the full dataset. Useful when trying to compare subsets of data.
#' @param reorder_axis Names of features (columns) for which to reverse axis order from default min-max to max-min. Defaults to NULL.
#' @param background_color The background color of the chart. Defaults to "gray 99".
#' @param area_fill Whether or not to fill the shapes made by connecting data points. Defaults to TRUE.
#' @param fill_opacity How opaque are the shape fills. Defaults to 0.05.
#' @param central_distance The offset from the center of the radar chart. Defaults to 0.2.
#' @param axis_name_offset The axis name offset from the axis. Defaults to 0.2.
#' @param digit_rounding To how many digits are the numbers rounded. Defaults to 2.
#' @param axis_label_font_size Font size for the axis labels.
#' @param axis_label_font_face Font face for the axis labels.
#' @param axis_name_font_size Font size for the axis names.
#' @param axis_name_font_face Font face for the axis names.
#' @returns A ggplot based spider/radar chart.
#' @export
#' @examples
#' mtcars_gr <- mtcars %>%
#'  tibble::rownames_to_column(var = "group") %>%
#'  tibble::as_tibble() %>%
#'  tail(3) %>%
#'  dplyr::select(1:7)
#'
#' ggspider(mtcars_gr, polygon = TRUE)
#'
#' ggspider(mtcars_gr, polygon = FALSE)
#'
#' ggspider(mtcars_gr, polygon = TRUE, scaled = TRUE)
#'
#' ggspider(mtcars_gr, polygon = FALSE, scaled = TRUE)
#'
#' ggspider(mtcars_gr, reorder_axis = c("mpg", "hp"))
#'
#' iris_summary <- iris %>%
#'  dplyr::group_by(Species) %>%
#'  dplyr::summarise(across(everything(), mean))
#'
#' ggspider(iris_summary)
#' ggspider(iris_summary, reorder_axis = c("Sepal.Length"))
#'
#' iris_summary_ci <- iris %>%
#'  dplyr::group_by(Species) %>%
#'  dplyr::summarise(across(everything(), ~ 1.97*sd(.)/sqrt(n())))
#'
#' iris_ci <- iris_summary %>%
#'  tidyr::pivot_longer(-1, names_to = "parameter", values_to = "mean") %>%
#'  dplyr::left_join(iris_summary_ci %>%
#'  tidyr::pivot_longer(-1, names_to = "parameter", values_to = "ci")) %>%
#'  dplyr::mutate(min = mean - ci, max = mean + ci) %>%
#'  dplyr::select(-mean, -ci)
#'
#' ggspider(iris_summary, ci_data = iris_ci)
#' ggspider(iris_summary, ci_data = iris_ci, reorder_axis = c("Sepal.Length"))

ggspider <- function(p_data,
                     ci_data = NULL,
                     polygon = TRUE,
                     scaled = FALSE,
                     draw_axis = TRUE,
                     n_labels = 5,
                     zero_centered = FALSE,
                     subset = NULL,
                     reorder_axis = NULL,
                     background_color = "gray99",
                     area_fill = TRUE,
                     fill_opacity = 0.05,
                     central_distance = 0.2,
                     axis_name_offset = 0.2,
                     digit_rounding = 2,
                     axis_label_font_size = NULL,
                     axis_label_font_face = NULL,
                     axis_name_font_size = NULL,
                     axis_name_font_face = NULL
){

  legend_title <- names(p_data)[[1]]
  p_data <- p_data %>% dplyr::rename(group = 1) %>% dplyr::mutate(group = factor(group))

  if(zero_centered == TRUE){
    zero_tibble <- tibble::as_tibble(as.list(setNames(rep(0, ncol(p_data)), names(p_data)))) %>% dplyr::mutate(group = "zero_centered")
    p_data <- p_data %>% dplyr::bind_rows(zero_tibble)
  }
  
  if(!is.null(reorder_axis)){
    p_data <- p_data %>% dplyr::mutate(dplyr::across(dplyr::all_of(reorder_axis), ~ -.))

    if(!is.null(ci_data)){
      ci_data <- ci_data %>%
        dplyr::mutate(c = min,
               min = dplyr::case_when(parameter %in% reorder_axis ~ -max, TRUE ~ min),
               max = dplyr::case_when(parameter %in% reorder_axis ~ -c, TRUE ~ max))
    }
  }


  if(!is.null(ci_data)){
    ci_data <- ci_data %>% dplyr::rename(group = 1, parameter = 2)
  }

  circle_coords <- function(r, n_axis = ifelse(polygon == TRUE, ncol(p_data) - 1, 100)){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    x <- r*cos(fi)
    y <- r*sin(fi)

    tibble::tibble(x, y, r)
  }

  step_1 <- purrr::map_df(seq(0, 1, 0.25) + central_distance, circle_coords) %>%
      ggplot2::ggplot(ggplot2::aes(x, y)) +
      ggplot2::geom_polygon(data = circle_coords(1 + central_distance), alpha = 1, fill = background_color, lty = 2) +
      ggplot2::geom_path(ggplot2::aes(group = r), lty = 2, alpha = 0.5) +
      ggplot2::theme_void()


  axis_coords <- function(n_axis){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
    x1 <- central_distance*cos(fi)
    y1 <- central_distance*sin(fi)
    x2 <- (1 + central_distance)*cos(fi)
    y2 <- (1 + central_distance)*sin(fi)

    tibble::tibble(x = c(x1, x2), y = c(y1, y2), id = rep(1:n_axis, 2))
  }

  if(is.null(ci_data)){
    text_data <- p_data %>%
      dplyr::select(-group) %>%
      purrr::map_df(~ min(.) + (max(.) - min(.)) * seq(0, 1, 1/(n_labels - 1))) %>%
      dplyr::mutate(r = seq(0, 1, 1/(n_labels - 1))) %>%
      tidyr::pivot_longer(-r, names_to = "parameter", values_to = "value")
  }else{
    text_data <-
      p_data %>%
      dplyr::select(-group) %>%
      purrr::map_df(~ min(.) + (max(.) - min(.)) * seq(0, 1, 1/(n_labels - 1))) %>%
      dplyr::mutate(r = seq(0, 1, 1/(n_labels - 1))) %>%
      tidyr::pivot_longer(-r, names_to = "parameter", values_to = "value") %>%
      dplyr::select(r, parameter) %>%
      dplyr::left_join(
        ci_data %>%
        dplyr::group_by(parameter) %>%
        dplyr::reframe(min = min(min), max = max(max)) %>%
        dplyr::group_by(parameter) %>%
        dplyr::mutate(data = list(tibble::tibble(r = seq(0, 1, 1/(n_labels - 1)), value = min + (max-min)*r))) %>%
        tidyr::unnest("data") %>%
        dplyr::select(r, parameter, value) %>%
        dplyr::arrange(r), by = c("parameter", "r")
      )
  }

  text_coords <- function(r, n_axis = ncol(p_data) - 1){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
    x <- r*cos(fi)
    y <- r*sin(fi)

    tibble::tibble(x, y, r = r - central_distance)
  }

  labels_data <- purrr::map_df(seq(0, 1, 1/(n_labels - 1)) + central_distance, text_coords) %>%
    dplyr::bind_cols(text_data %>% dplyr::select(-r))


  if(!is.null(reorder_axis)){
    labels_data <- labels_data %>% dplyr::mutate(value = case_when(parameter %in% reorder_axis ~ -value,
                                                                   TRUE ~ value))
  }


  rescaled_coords <- function(r, n_axis){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    tibble::tibble(r, fi) %>% dplyr::mutate(x = r*cos(fi), y = r*sin(fi)) %>% dplyr::select(-fi)
  }

  if(!is.null(ci_data)){
    pdata_long_rescaled <- ci_data %>%
      dplyr::select(group, parameter, y = min) %>%
      dplyr::mutate(measure = "min") %>%
      dplyr::bind_rows(ci_data %>%
                   dplyr::select(group, parameter, y = max) %>%
                   dplyr::mutate(measure = "max")) %>%
      dplyr::bind_rows(p_data %>%
                  tidyr::pivot_longer(-1, names_to = "parameter", values_to = "mean") %>%
                  dplyr::select(group, parameter, y = mean) %>%
                  dplyr::mutate(measure = "mean")) %>%
      dplyr::group_by(parameter) %>%
      dplyr::mutate(y = scales::rescale(y))

    rescaled_min <- pdata_long_rescaled %>% filter(measure == "min") %>%
      dplyr::select(-measure) %>%
      tidyr::pivot_wider(names_from = "parameter", values_from = "y") %>%
      dplyr::mutate(copy = dplyr::pull(., 2)) %>% #da se moze geom_path spojiti opet na pocetnu tocku
      tidyr::pivot_longer(-group, names_to = "parameter", values_to = "value") %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(coords = rescaled_coords(value + central_distance, ncol(p_data) - 1)) %>%
      tidyr::unnest(cols = c(coords)) %>%
      dplyr::select(group, parameter, xmin = x, ymin = y)

    rescaled_max <- pdata_long_rescaled %>% filter(measure == "max") %>%
      dplyr::select(-measure) %>%
      tidyr::pivot_wider(names_from = "parameter", values_from = "y") %>%
      dplyr::mutate(copy = dplyr::pull(., 2)) %>% #da se moze geom_path spojiti opet na pocetnu tocku
      tidyr::pivot_longer(-group, names_to = "parameter", values_to = "value") %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(coords = rescaled_coords(value + central_distance, ncol(p_data) - 1)) %>%
      tidyr::unnest(cols = c(coords)) %>%
      dplyr::select(group, parameter, xmax = x, ymax = y)

    rescaled_ci <- rescaled_min %>% dplyr::left_join(rescaled_max, by = dplyr::join_by(group, parameter))

    rescaled_ci_alt <- rescaled_min %>% dplyr::rename(x = xmin, y = ymin) %>% dplyr::bind_rows(rescaled_max %>% dplyr::rename(x = xmax, y = ymax))
  }else{
    pdata_long_rescaled <- p_data %>%
                  tidyr::pivot_longer(-1, names_to = "parameter", values_to = "mean") %>%
                  dplyr::select(group, parameter, y = mean) %>%
                  dplyr::mutate(measure = "mean") %>%
      dplyr::group_by(parameter) %>%
      dplyr::mutate(y = scales::rescale(y))
  }

  rescaled_data <- pdata_long_rescaled %>% dplyr::filter(measure == "mean") %>%
    dplyr::select(-measure) %>%
    tidyr::pivot_wider(names_from = "parameter", values_from = "y") %>%
    dplyr::mutate(copy = dplyr::pull(., 2)) %>% #da se moze geom_path spojiti opet na pocetnu tocku
    tidyr::pivot_longer(-group, names_to = "parameter", values_to = "value") %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(coords = rescaled_coords(value + central_distance, ncol(p_data) - 1)) %>%
    tidyr::unnest(cols = c(coords))

  rescaled_data <- if(is.null(subset) & zero_centered == FALSE){
    rescaled_data
  } else if(is.null(subset) & zero_centered == TRUE) {
    rescaled_data %>% dplyr::filter(group != "zero_centered")
  } else {
      rescaled_data %>% dplyr::filter(group %in% subset)
  }

  step_1 +
    {if(draw_axis == TRUE) ggplot2::geom_line(data = axis_coords(ncol(p_data) - 1), ggplot2::aes(x, y, group = id), alpha = 0.3)} +
    {if(!is.null(ci_data)) ggplot2::geom_segment(data = rescaled_ci,
                          ggplot2::aes(x = xmin, y = ymin, xend = xmax, yend = ymax, col = group, lwd = 1),
                          alpha = 0.5, lineend = "square", show.legend = FALSE,
                          arrow = arrow(ends = "both", angle = 90, length = unit(.1,"cm")))} +
    scale_linewidth(range = c(0, 4)) +
    ggplot2::geom_point(data = rescaled_data, ggplot2::aes(x, y, group = group, col = group), size = 2, stroke = 2) +
    ggplot2::geom_path(data = rescaled_data, ggplot2::aes(x, y, group = group, col = group), size = 1) +
    {if(area_fill == TRUE) ggplot2::geom_polygon(data = rescaled_data, ggplot2::aes(x, y, group = group, col = group, fill = group), size = 1, alpha = fill_opacity, show.legend = FALSE)} +
    {if(scaled == TRUE){
      ggplot2::geom_text(data = labels_data %>% dplyr::filter(parameter == labels_data$parameter[[1]]), ggplot2::aes(x, y, label = r), alpha = 0.65,
                         family = theme_get()$text[["family"]],
                         size = ifelse(is.null(axis_label_font_size), theme_get()$text[["size"]]/2.75, axis_label_font_size),
                         fontface = ifelse(is.null(axis_label_font_face), "plain", axis_label_font_face))
    }else{
        ggplot2::geom_text(data = labels_data, ggplot2::aes(x, y, label = round(value, digit_rounding)), alpha = 0.65,
                           family = theme_get()$text[["family"]],
                           size = ifelse(is.null(axis_label_font_size), theme_get()$text[["size"]]/2.75, axis_label_font_size),
                           fontface = ifelse(is.null(axis_label_font_face), "plain", axis_label_font_face))
      }
    } +
    ggplot2::geom_text(data = text_coords(1 + central_distance + axis_name_offset), ggplot2::aes(x, y), label = labels_data$parameter[1:(ncol(p_data)-1)],
                       family = theme_get()$text[["family"]],
                       size = ifelse(is.null(axis_name_font_size), theme_get()$text[["size"]]/2.75, axis_name_font_size),
                       fontface = ifelse(is.null(axis_name_font_face), "plain", axis_name_font_face)) +
    ggplot2::labs(col = legend_title) +
    ggplot2::theme(legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 12),
                   legend.title = ggplot2::element_text(size = 12))
}

