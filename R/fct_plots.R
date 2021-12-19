#' Summary plot
#'
#' @description A function to produce a summary plot of a given y variable, chossing with varibles will be used for colours and for facets
#'
#' @param df A data.frame containing the data to be plotted.
#' @param df.labels A data.frame with plot labels.
#' @param y_var A string with the column name of the variable for the y axis.
#' @param colour_var A string with the column name of the variable to determine the colours.
#' @param f_rows_var A string with the column name of the variable to the determine the rows for the facet_grid.
#' @param f_cols_var A string with the column name of the variable to the determine the columns for the facet_grid.
#'
#' @return
#' @export
#'
#' @examples
plot_summary <-
  function(df,
           y_var,
           colour_var,
           f_rows_var,
           f_cols_var,
           df.labels,
           log_space,
           free_y_scale = F) {
    # browser()
    y.var <- as.name(y_var)
    colour.var <- as.name(colour_var)
    f.rows.var <- as.name(f_rows_var)
    f.cols.var <- as.name(f_cols_var)
    
    
    y_label <-
      df.labels %>% dplyr::filter(var == y_var) %>% dplyr::pull(label) %>% as.expression()
    
    if (free_y_scale) {
      facet_scales <- "free_y"
    } else{
      facet_scales <- "fixed"
    }
    
    p <- df %>%
      ggplot2::ggplot(ggplot2::aes(x = time_h, y = {{y.var}}, colour = {{colour.var}})) +
      ggplot2::stat_summary(fun.data = "mean_cl_boot",
                            geom = "point",
                            alpha = 0.6) +
      ggplot2::stat_summary(fun.data = "mean_sdl",
                            geom = "errorbar",
                            alpha = 0.6) +
      ggplot2::stat_summary(fun.data = "mean_cl_boot",
                            geom = "line",
                            alpha = 0.6) +
      ggplot2::facet_grid(
        rows = ggplot2::vars({{f.rows.var}}),
        cols = ggplot2::vars({{f.cols.var}}),
        scales = facet_scales
      ) +
      ggplot2::theme_light(14) +
      ggthemes::scale_colour_colorblind() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(x = p.label.time.h,
                    y = y_label)
    
    if (log_space) {
      p <- p +
        ggplot2::scale_y_continuous(trans = "log", breaks = 1:10)
    }
    
    p
  }



plot_growth_rates <- function(df,
                              x_var,
                              colour_var,
                              f_rows_var,
                              f_cols_var) {
  
  x.var <- as.name(x_var)
  colour.var <- as.name(colour_var)
  f.rows.var <- as.name(f_rows_var)
  f.cols.var <- as.name(f_cols_var)
  
  df %>% 
    ggplot2::ggplot(ggplot2::aes(x = {{x.var}}, y = estimate)) +
    ggplot2::geom_col(ggplot2::aes(fill = {{colour.var}}), position = "dodge") +
    ggplot2::facet_grid(
      rows = ggplot2::vars({{f.rows.var}}),
      cols = ggplot2::vars({{f.cols.var}}),
      scales = "free_x"
    ) +
    ggthemes::scale_fill_colorblind() +
    ggplot2::theme_light(14) +
    ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(
      y = p.label.mu,
      x = "Strain"
    )
  
}


plot_full_spectrum <- function(df,
                               y_var,
                               colour_var,
                               f_rows_var,
                               f_cols_var) {
  y.var <- as.name(y_var)
  colour.var <- as.name(colour_var)
  f.rows.var <- as.name(f_rows_var)
  f.cols.var <- as.name(f_cols_var)
  
  df %>% 
    ggplot2::ggplot(
    ggplot2::aes(
      x = nm,
      y = as.factor(time_h),
      height = {{y.var}},
      colour = {{colour.var}},
      fill = {{colour.var}},
      group = interaction(time_h, {{colour.var}})
    )
  ) +
    ggridges::geom_ridgeline(alpha = 0.05)  +
    ggplot2::facet_grid(
      rows = ggplot2::vars({{f.rows.var}}),
      cols = ggplot2::vars({{f.cols.var}})
    ) +
    ggthemes::scale_colour_colorblind() +
    ggthemes::scale_fill_colorblind() +
    ggplot2::labs(x = p.label.nm,
         y = p.label.time.h) +
    ggplot2::theme_light(14) +
    ggplot2::theme(legend.position = "bottom")
  
}
