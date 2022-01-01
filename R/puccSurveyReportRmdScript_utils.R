## PUCC Survey Report Rmd file utility functions-------------------------------


#' Plot custom bar graph for a given discrete factor column
#'
#' This function plots a custom bar graph for a given factor column.
#'
#' @import ggplot2
#' @import ggthemes
#'
#' @param df dataframe used to generate the bar graph
#' @param col_to_plot name (character string of length 1) of the  column from
#'                    \code{df} whose distribution is to be plotted.
#' @param type_of_graph this can take one of two character values: "count" or
#'                      "percentage"
#'
#' @param xlab label for the x-axis
#' @param ylab label for the y-axis
#' @param title title for the graph
#' @param facet if set to \code{TRUE}, a facetted graph will be produced. By default
#'              the value of this parameter is set as \code{FALSE}.
#' @param facet_by a character string of length 1, specifying the name of the variable
#'        /column by which we want to facet the graph
#'
#' @return a ggplot object containing the graph
#'
#' @examples
#'
#' plot_bargraph(df = mtcars, col_to_plot = "gear", type_of_graph = "percentage",
#' xlab = "gear", ylab = "percentage",
#' title = "distribution of gears for mtcars dataset", facet = FALSE)
#'
#'
#' @export

plot_bargraph <- function(df, col_to_plot, type_of_graph = "percentage", xlab = " ",
                          ylab = " ", title = " ", facet = FALSE, facet_by = " "){
  if(type_of_graph == "percentage"){
    if(facet == FALSE){
      df %>%
        ggplot(mapping = aes(x = !!as.symbol(col_to_plot))) +
        geom_bar(mapping = aes(y = (..count..)/sum(..count..)), color = "burlywood2", width = 0.5) +
        labs(x = xlab,
             y = ylab, title = title) +
        scale_y_continuous(labels = scales::percent) +
        ggthemes::theme_solarized()
    } else if (facet == TRUE) {
      df %>%
        ggplot(mapping = aes(x = !!as.symbol(col_to_plot))) +
        geom_bar(mapping = aes(y = (..count..)/sum(..count..)), color = "burlywood2", width = 0.5) +
        labs(x = xlab,
             y = ylab, title = title) +
        scale_y_continuous(labels = scales::percent) +
        ggthemes::theme_solarized() +
        facet_wrap(vars(!!as.symbol(facet_by)))
    }

  } else if (type_of_graph == "count"){
    if(facet == FALSE){
      df %>%
        ggplot(mapping = aes(x = !!as.symbol(col_to_plot))) +
        geom_bar(stat = "count", color = "burlywood2", width = 0.5) +
        labs(x = xlab,
             y = ylab, title = title) +
        ggthemes::theme_solarized()
    } else if(facet == TRUE){
      df %>%
        ggplot(mapping = aes(x = !!as.symbol(col_to_plot))) +
        geom_bar(stat = "count", width = 0.5) +
        labs(x = xlab,
             y = ylab, title = title) +
        ggthemes::theme_solarized() +
        facet_wrap(vars(!!as.symbol(facet_by)), scales = "free_y")
    }

  }

}



#' Generate a summary table given a grouping variable
#'
#' Given a dataframe, generate a summary table, given a column name by which
#' to group_by. This table contains both percentages and counts.
#'
#' @importFrom dplyr group_by summarise
#' @importFrom knitr kable
#'
#' @param df dataframe to be summarised given \code{col_to_grp_by}
#'
#' @param col_to_grp_by a character vector of column name(s) to group by
#' @param arrange_by a character vector (length 1) of column name to arrange by once grouping
#' is done in \code{col_to_grp_by}
#'
#' @return a summarised dataframe grouped by \code{col_to_grp_by} containing counts
#'         and percentages
#'
#' @examples
#' summarise_df_by_colname(df = mtcars, col_to_grp_by = "gear")
#' summarise_df_by_colname(df = mtcars, col_to_grp_by = c("gear", "cyl"))
#'
#' @export

summarise_df_by_colname <- function(df, col_to_grp_by){
  df %>%
    dplyr::group_by(across(col_to_grp_by)) %>%
    dplyr::summarise(count = n()) %>%
    ungroup() %>%
    mutate(percentage = (count/sum(count)*100))
}


#' time hms formatter for creating labels for \code{time_only} values in graphs
#'
#' Generates custom labels for \code{time_only} column. Once generated these can
#' be plotted on a graph.
#'
#' @importFrom stringr str_c
#'
#' @param num_seconds_since_12am this takes in a time value in seconds (with the reference
#'                               starting point being 00:00:00) in seconds. So,
#'                               for example 1 am in the morning would be equal
#'                               to 3600 seconds. So, entering "3600" in
#'                               \code{num_seconds_since_12am} will output
#'                               "01:00:00" which is 1 am in the morning. The outputs
#'                               will be in 24 hr time format.
#'
#' @return a string representing time in "hh:mm:ss" format
#'
#' @examples
#'
#' time_hms_formatter(3600) # This outputs: "01:00:00" which is 3600 seconds past from
#' "00:00:00"
#'
#' time_hms_formatter(7200) # This outputs: "02:00:00" which is 7200 seconds past from
#' "00:00:00"
#'
#'
#' @export
#'

time_hms_formatter <- function(num_seconds_since_12am){

 # calculating values of hours, minutes and seconds in separate parts
 h_orig <- num_seconds_since_12am/3600
 h_floor <- floor(num_seconds_since_12am/3600)
 h_resid <- h_orig - h_floor
 h_resid_sec <- h_resid * 3600

 m_orig <- h_resid_sec/60
 m_floor <- floor(m_orig)
 m_resid <- m_orig - m_floor
 m_resid_sec <- m_resid * 60
 m_resid_sec_floor <- floor(m_resid_sec)

 # combining the separate hours, minutes and seconds value in a string
 lab <- stringr::str_c(h_floor, m_floor, m_resid_sec_floor, sep = ":")
 return(lab)
}
