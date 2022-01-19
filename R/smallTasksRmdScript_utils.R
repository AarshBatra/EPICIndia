## Small tasks Rmd script utility functions-------------------------------

#' This function plots the bar graphs for data that for which counts for
#' various categories is available.
#'
#' @param df data frame that will be used to plot the data
#' @param map_x the column name (in quotes) that will be mapped to the x axis.
#' @param map_y the column name (in quotes) that will be mapped to the y axis.
#' @param fill the color with which the bar graphs will be filled in.
#'             This should be a character vector of length 1.
#' @param y_axis_ticks the spacing of tick marks on the y axis.
#'                     this should be a numeric vector containing
#'                     points at which tick marks need to be placed.
#'
#' @param xlab x axis label. This should be a character vector of length 1.
#' @param ylab y axis label. This should be a character vector of length 1.
#' @param title Graph Title. This should be a character vector of length 1.
#'
#' @return a ggplot object.
#'
#' @examples
#' plot_bar_transp_dept_format_data(dataset, "col_a", "count_for_col_a", "midnightblue", "col_a", "col_a_count",
#' "Graph Title")
#'
#' @export

plot_bar_transp_dept_format_data <- function(df, map_x, map_y, fill, y_axis_ticks,
                                             xlab, ylab, title){

  ggplot(df, mapping = aes(x = !!as.symbol(map_x), y = !!as.symbol(map_y))) +
    geom_col(fill = fill, width = 0.6) +
    scale_y_continuous(breaks = y_axis_ticks) +
    geom_text(mapping = aes(label = !!as.symbol(map_y)), position = position_dodge(width=0.9), vjust=-0.25) +
    labs(x = xlab, y = ylab) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggthemes::theme_tufte()

}
