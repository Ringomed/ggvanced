#' A function for generating parallel plots with separate axis for each variable
#' @param p_data A data frame containing plotting data.
#' with the first column containing the group/observation names,
#' and the other columns being descriptory variables for each group/observation.
#' @param tick_number Number of ticks/breaks on the vertical axis. Defaults to 5.
#' @param tick_length Length of ticks on the vertical axis. Defaults to 0.1.
#' @param label_nudge_x How much to nudge the vertical axis labels in the horizontal direction. Defaults to -0.3.
#' @param label_nudge_y How much to nudge the vertical axis labels in the vertical direction. Defaults to 0.
#' @param label_text_size The font size of vertical axis labels. Defaults to 3.5.
#' @param group_name_text_size The font size of horizontal axis labels. Defaults to 11.
#' @param digit_rounding To how many digits are the numbers rounded. Defaults to 2.
#' @returns A ggplot chart.
#' @export
#' @examples
#' mtcars_gr <- mtcars %>%
#'  tibble::rownames_to_column(var = "group") %>%
#'  tibble::as_tibble() %>%
#'  tail(3) %>%
#'  dplyr::select(1:7)
#'
#' ggparallel(mtcars_gr)
#'
#' iris_summary <- iris %>%
#'  dplyr::group_by(Species) %>%
#'  dplyr::summarise(across(everything(), mean))
#'
#' ggparallel(iris_summary)

ggparallel <- function(p_data,
                    tick_number = 5,
                    tick_length = 0.1,
                    label_nudge_x = -0.3,
                    label_nudge_y = 0,
                    label_text_size = 3.5,
                    group_name_text_size = 11,
                    digit_rounding = 2
){
  legend_title <- names(p_data)[[1]]
  p_data <- p_data %>% dplyr::rename(group = 1) %>% dplyr::mutate(group = factor(group))

  point_coords <- p_data %>%
    dplyr::mutate(dplyr::across(-group, scales::rescale)) %>%
    tidyr::pivot_longer(names_to = "parameter", values_to = "value", -group) %>%
    dplyr::rename(plot_value = value) %>%
    dplyr::left_join(p_data %>%
                tidyr::pivot_longer(names_to = "parameter", values_to = "value", -group) %>%
                dplyr::rename(real_value = value))

  axis_coords <- tibble::tibble(parameter = unique(point_coords$parameter), plot_value = list(c(0, 1))) %>% tidyr::unnest(cols = c(plot_value))

  label_coords <- p_data %>%
    dplyr::reframe(across(-group, range)) %>%
    t %>%
    as.data.frame() %>%
    dplyr::mutate(parameter = rownames(.), .before = V1) %>%
    dplyr::rename(min = V1, max = V2) %>%
    tibble::as_tibble() %>%
    dplyr::group_by(parameter) %>%
    dplyr::mutate(coords = list(tibble::tibble(plot_value = seq(0, 1, 1/(tick_number - 1)), real_value = min + seq(0, 1, 1/(tick_number - 1))*(max-min)))) %>%
    tidyr::unnest(cols = c(coords))

  tick_coords <- tibble::tibble(y = rep(seq(0, 1, 1/(tick_number - 1)), ncol(p_data) - 1),
                        x = list(rep(1:(ncol(p_data) - 1), tick_number))) %>%
    tidyr::unnest(cols = c(x))

  point_coords %>%
    ggplot2::ggplot(ggplot2::aes(parameter, plot_value)) +
    ggplot2::geom_line(ggplot2::aes(group = group, col = group)) +
    ggplot2::geom_point(ggplot2::aes(col = group)) +
    ggplot2::theme_classic() +
    ggplot2::geom_line(data = axis_coords, ggplot2::aes(parameter, plot_value, group = parameter), alpha = 0.3) +
    ggplot2::geom_segment(data = tick_coords, ggplot2::aes(x = x, xend = x - tick_length, y = y, yend = y)) +
    ggplot2::geom_text(data = label_coords, ggplot2::aes(parameter, plot_value, label = round(real_value, digit_rounding)),
              nudge_x = label_nudge_x,
              nudge_y = label_nudge_y,
              size = label_text_size) +
    ggplot2::labs(col = legend_title) +
    ggplot2::theme(legend.position = "bottom",
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.line = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = group_name_text_size))
}
