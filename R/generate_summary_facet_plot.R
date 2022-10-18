#' Computes summary data for each experimental conition/cell.
#'
#' @param param_summary_data parameter summary data (created from compute_parameter_summary)
#' @export
generate_summary_facet_plot <- function(condition_data, lower_y_limit, upper_y_limit, ticks,
                                        #parameters that change across Experiments 1-3
                                        x_axis_var = 'midpoint', x_axis_name = 'Midpoint location (days)',
                                        facet_var = 'measurement_spacing',
                                        exp_num,
                                        num_rows = 2, num_cols = 2,
                                        legend_position = c(.50, .50), legend_direction = 'horizontal',
                                        file_name, dodge_width = 0.8,
                                        point_size = 15, line_size = 2, error_bar_width = 0.8,
                                        error_bar_size = 2.5,
                                        y_axis_var = 'estimate',  y_axis_name = 'Parameter Bias (Percentage Error)',
                                        grouping_var = 'number_measurements',
                                        #fill_var = 'conv_fail',
                                        #fill_legend_title = 'Convergence \nSuccess',
                                        shape_legend_title = 'Number of \nMeasurements',
                                        panel_spacing_y = 12, dodge_position =  position_dodge(width = 0.8)) {

  file_name <- paste(exp_num, 'summary_plot.pdf', sep = '')

  #create base plot
  base_plot <- create_base_plot_unfiltered_summ(parameter_data = condition_data, x_axis_var = x_axis_var, y_axis_var = y_axis_var,
                              grouping_var = grouping_var)

  #primary aesthetic specifications (points, lines, error bars, hline)
  plot_visualizations <- create_summary_data_visualizations(dodge_position = dodge_position,
                                                    point_size = point_size, line_size =  line_size)
  #create legend
  legend_details <-  create_legend_unfiltered_summ(shape_legend_title = shape_legend_title, x_axis_name =  x_axis_name)

  #facets
  if (str_detect(string = exp_num, pattern = '3')) {
   facet_details <- create_time_struc_facets(facet_var = facet_var,
                                              y_axis_name = y_axis_name, lower_y_limit = lower_y_limit, upper_y_limit = upper_y_limit, ticks = ticks)
  }

  else {
    facet_details <- create_spacing_facets(facet_var = facet_var, lower_y_limit = lower_y_limit, upper_y_limit = upper_y_limit,
                                              y_axis_name = y_axis_name, ticks = ticks)
  }

  #thematic elements
  thematic_details <- create_thematic_elements(legend_position = legend_position, legend_direction = legend_direction,
                                               panel_spacing_y = panel_spacing_y)


  #assemble plot
  summary_facet_plot <- base_plot + plot_visualizations + legend_details + facet_details + thematic_details

##create PDF of faceted plot
  set_panel_size(p = summary_facet_plot, height = unit(x = 28, units = 'cm'),
                 width = unit(x = 40, units = 'cm'),
                 file =  file_name)
}

#create facets for measurement spacing types
create_spacing_facets <- function(facet_var, y_axis_name, lower_y_limit, upper_y_limit, ticks = 2) {


  spacing_facet_details <- facet_wrap_custom( ~ get(facet_var), scales = "free", ncol = 2, nrow = 2,
                                              dir = 'h', labeller = label_parsed,
                                              scale_overrides = list(scale_override(1,
                                                                                    scale_y_continuous(name =  y_axis_name,
                                                                                                       breaks = seq(from = lower_y_limit, to = upper_y_limit, by = ticks),
                                                                                                       limits = c(lower_y_limit, upper_y_limit))),
                                                                     scale_override(2,
                                                                                    scale_y_continuous(name =  y_axis_name,
                                                                                                       breaks = seq(from = lower_y_limit, to = upper_y_limit, by = ticks),
                                                                                                       limits = c(lower_y_limit, upper_y_limit))),
                                                                     scale_override(3,
                                                                                    scale_y_continuous(name =  y_axis_name,
                                                                                                       breaks = seq(from = lower_y_limit, to = upper_y_limit, by = ticks),
                                                                                                       limits = c(lower_y_limit, upper_y_limit))),
                                                                     scale_override(4,
                                                                                    scale_y_continuous(name =  y_axis_name,
                                                                                                       breaks = seq(from = lower_y_limit, to = upper_y_limit, by = ticks),
                                                                                                       limits = c(lower_y_limit, upper_y_limit)))))

  return(spacing_facet_details)
}

#create facets for measurement spacing types
create_time_struc_facets <- function(facet_var, y_axis_name, lower_y_limit, upper_y_limit, ticks = 2) {


  time_struc_facet_details <- facet_wrap_custom( ~ get(facet_var), scales = "free", ncol = 2, nrow = 2,
                                                 dir = 'h', labeller = label_parsed,
                                                 scale_overrides = list(scale_override(1,
                                                                                       scale_y_continuous(name =  y_axis_name,
                                                                                                          breaks = seq(from = lower_y_limit, to = upper_y_limit, by = ticks),
                                                                                                          limits = c(lower_y_limit, upper_y_limit))),
                                                                        scale_override(2,
                                                                                       scale_y_continuous(name =  y_axis_name,
                                                                                                          breaks = seq(from = lower_y_limit, to = upper_y_limit, by = ticks),
                                                                                                          limits = c(lower_y_limit, upper_y_limit))),
                                                                        scale_override(3,
                                                                                       scale_y_continuous(name =  y_axis_name,
                                                                                                          breaks = seq(from = lower_y_limit, to = upper_y_limit, by = ticks),
                                                                                                          limits = c(lower_y_limit, upper_y_limit)))))

  return(time_struc_facet_details)
}

#has hline removed
create_summary_data_visualizations <- function(dodge_position, point_size, line_size) {

  primary_plot <- list(
    geom_point(position = dodge_position, size = point_size, fill = 'black'),
    geom_line(size = line_size,  position = dodge_position))

  return(primary_plot)
}

create_base_plot_unfiltered_summ <- function(parameter_data, x_axis_var, y_axis_var, grouping_var) {

  base_plot <-  ggplot(data = parameter_data, aes(x = !!sym(x_axis_var), y = !!sym(y_axis_var),
                                                  group = !!sym(grouping_var),
                                                  linetype = !!sym(grouping_var),
                                                  shape = !!sym(grouping_var)))

  return(base_plot)

}

create_legend_unfiltered_summ <- function(shape_legend_title, x_axis_name) {

  #legend details + x-axis name
  legend_details <- list(
    scale_shape_manual(name = shape_legend_title, values=c(22,21,24,23),
                       guide  = guide_legend(override.aes = list(fill = c("black")),
                                             guide = guide_legend(order = 0))),
    scale_linetype_manual(name = shape_legend_title, values = rev(c('dotted', 'dashed', 'longdash', 'solid'))),
    #scale_x_discrete(name = x_axis_name))
    labs(x = x_axis_name))

  return(legend_details)
}

