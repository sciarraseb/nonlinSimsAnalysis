#' Generate facetted plots
#'
#' @param analytical_data experiment data
#' @param target_col experimental conditions
#' @export
generate_day_likert_facet_plot <- function(analytical_data, target_col = 'measurement_spacing',
                                           target_value, x_axis_var, x_axis_name, exp_num,
                                          beta_lower, beta_upper, beta_ticks) {

  #if exp_num contains 1, center beta estimates
  if (str_detect(string = exp_num, pattern = '1')){
    analytical_data$days <- center_beta_param_data(day_data = analytical_data$days)
    }

  #filter for rows that match measurement spacing/time structuredness condition
  day_parameter_data <- analytical_data$days %>% filter(!!sym(target_col) == target_value)
  likert_parameter_data <- analytical_data$likert %>% filter(!!sym(target_col) == target_value)

  #file names for PDFs
  days_file_name = paste('Figures/', exp_num, 'plot_days_', tolower(target_value), '.pdf', sep = '')
  likert_file_name = paste('Figures/', exp_num, 'plot_likert_', tolower(target_value),'.pdf', sep = '')

 # days_file_name = paste(exp_num, 'plot_days_', tolower(target_value), '.pdf', sep = '')
 # likert_file_name = paste(exp_num, 'plot_likert_', tolower(target_value),'.pdf', sep = '')

  #setup variable for dodging
  dodge_position <- position_dodge(width = 0.8)

  #generate day facet plot
  generate_day_parameter_facet_plot(parameter_data = day_parameter_data, file_name = days_file_name,
                                    dodge_position = dodge_position,
                                    beta_lower = beta_lower, beta_upper = beta_upper, beta_ticks = beta_ticks,
                                    #parameters that change across Experiments 1-3
                                    x_axis_var = x_axis_var, x_axis_name = x_axis_name,
                                    exp_num = exp_num)


  #generate likert facet plot
  generate_likert_parameter_facet_plot(parameter_data = likert_parameter_data, file_name = likert_file_name,
                                dodge_position = dodge_position,
                                #parameters that change across Experiments 1-3
                                x_axis_var = x_axis_var, x_axis_name = x_axis_name)
}

center_beta_param_data <- function(day_data) {

  #extract beta_fixed_data
  beta_rows <- which(str_detect(string = day_data$parameter, pattern = 'beta\\[fixed\\]'))
  beta_fixed_data <- day_data[beta_rows, ]

  #replace estimate, upper_ci, lower_ci
  day_data$estimate[beta_rows] <- as.numeric(as.character(beta_fixed_data$pop_value)) - beta_fixed_data$estimate
  day_data$upper_ci[beta_rows] <- as.numeric(as.character(beta_fixed_data$pop_value)) - beta_fixed_data$upper_ci
  day_data$lower_ci[beta_rows] <- as.numeric(as.character(beta_fixed_data$pop_value)) - beta_fixed_data$lower_ci

  return(day_data)
}


generate_day_parameter_facet_plot <- function(parameter_data, num_rows = 2, num_cols = 2,
                                              legend_position = c(0.50, .50), legend_direction = 'horizontal',
                                              file_name, dodge_width = 0.8, h_line_alpha = 0.8, h_line_size = 40,
                                              point_size = 15, line_size = 2, error_bar_width = 0.8,
                                              error_bar_size = 2.5,
                                              y_axis_var = 'estimate',  y_axis_name = 'Estimate (Days)',
                                              grouping_var = 'number_measurements',
                                              grouping_var2 = 'bias_status',
                                              facet_var = 'parameter',
                                              #fill_var = 'conv_fail',
                                              fill_legend_title = 'Percentage \nRemoved Values',
                                              shape_legend_title = 'Number of \nMeasurements',
                                              panel_spacing_y = 20, dodge_position,
                                              #parameters that change across Experiments 1-3
                                              x_axis_var = 'midpoint', x_axis_name = 'Midpoint location (days)',
                                              beta_lower, beta_upper, beta_ticks, exp_num) {

  #place beta_fixed pop values at 0
  if (str_detect(string = exp_num, pattern = '1')) {
    beta_fixed_rows <- str_detect(string = parameter_data$parameter, pattern = 'beta\\[fixed\\]')
    parameter_data$pop_value[beta_fixed_rows] <- 0
  }

  #create base plot
  base_plot <- create_base_plot_unfiltered(parameter_data = parameter_data, x_axis_var = x_axis_var, y_axis_var = y_axis_var,
                                grouping_var = grouping_var, grouping_var2 = grouping_var2)

  #primary aesthetic specifications (points, lines, error bars, hline)
  linesizes <- generate_line_sizes(unit = 'day', param_data = parameter_data, beta_ticks, beta_upper, beta_lower)

  plot_visualizations <- create_data_visualizations_unfiltered(dodge_position = dodge_position,
                                                    point_size = point_size, line_size =  line_size,
                                                    lower_ci = lower_ci, upper_ci = upper_ci,
                                                    error_bar_width = error_bar_width, h_line_alpha = h_line_alpha,
                                                    linesizes = linesizes)

  #create legend
  legend_details <- create_legend(shape_legend_title = shape_legend_title,
                                  fill_legend_title = fill_legend_title, x_axis_name =  x_axis_name)

  #facets
  facet_details <- create_days_param_facets(facet_var = facet_var, num_rows = num_rows, num_cols = num_cols,
                                            y_axis_name = y_axis_name, beta_lower = beta_lower, beta_upper = beta_upper, beta_ticks = beta_ticks)

  #thematic elements
  thematic_details <- create_thematic_elements(legend_position = legend_position, legend_direction = legend_direction,
                                               panel_spacing_y = panel_spacing_y)


  #assemble plot
  day_parameter_facet_plot <- base_plot + plot_visualizations + legend_details + facet_details + thematic_details

  #create PDF of faceted plot
  set_panel_size(p = day_parameter_facet_plot, height = unit(x = 28, units = 'cm'),
                 width = unit(x = 40, units = 'cm'),
                 file =  file_name)

}

generate_likert_parameter_facet_plot <- function(parameter_data,  num_cols = 2,
                                          file_name, dodge_width = 0.8, h_line_alpha = 0.8, h_line_size = 15,
                                          point_size = 15, line_size = 2, error_bar_width = 0.8,
                                          error_bar_size = 2.5,
                                          y_axis_var = 'estimate',  y_axis_name = 'Estimate (Likert Units [Scale of 1-5])',
                                          grouping_var = 'number_measurements',
                                          grouping_var2 = 'bias_status',
                                          facet_var = 'parameter',
                                          #fill_var = 'conv_fail',
                                          #fill_legend_title = 'Percentage \nRemoved Values',
                                          shape_legend_title = 'Number of \nMeasurements',
                                          panel_spacing_y = 4, dodge_position,
                                          #arguments that need to be changed to Likert facets
                                          legend_position = c(0.73, 0.24), legend_direction = 'horizontal', num_rows = 3,
                                          #parameters that change across Experiments 1-3
                                          x_axis_var = 'midpoint', x_axis_name = 'Midpoint location (days)') {

  #create base plot
  base_plot <- create_base_plot_unfiltered(parameter_data = parameter_data, x_axis_var = x_axis_var, y_axis_var = y_axis_var,
                                grouping_var = grouping_var, grouping_var2 = grouping_var2)

  #primary aesthetic specifications (points, lines, error bars, hline)
  linesizes <- generate_line_sizes(unit = 'likert', param_data = parameter_data, beta_ticks = NA, beta_upper = NA, beta_lower = NA)
  plot_visualizations <- create_data_visualizations_unfiltered(dodge_position = dodge_position,
                                                    point_size = point_size, line_size =  line_size,
                                                    lower_ci = lower_ci, upper_ci = upper_ci,
                                                    error_bar_width = error_bar_width, h_line_alpha = h_line_alpha,
                                                    linesizes = linesizes)

  #create legend
  legend_details <- create_legend(shape_legend_title = shape_legend_title,
                                  fill_legend_title = fill_legend_title, x_axis_name =  x_axis_name)

  #facets
  facet_details <- create_likert_param_facets(facet_var = facet_var, num_rows = num_rows, num_cols = num_cols,
                                            y_axis_name = y_axis_name)

  #thematic elements
  thematic_details <- create_thematic_elements(legend_position = legend_position, legend_direction = legend_direction,
                                               panel_spacing_y = panel_spacing_y)

  #assemble plot
  likert_parameter_facet_plot <- base_plot + plot_visualizations + legend_details + facet_details + thematic_details

  #create PDF of faceted plot
  set_panel_size(p = likert_parameter_facet_plot, height = unit(x = 28, units = 'cm'),
                 width = unit(x = 40, units = 'cm'),
                 file =  file_name)

}

generate_line_sizes <- function(unit = 'likert', param_data, beta_ticks, beta_upper, beta_lower) {

  #for each parameter, we take 10% of its value and then multiply it by the margin and then by two (for the upper and lower portion)
  if (unit == 'likert') {
    line_sizes <- generate_likert_line_sizes(param_data)
  }

  else {
    line_sizes <- generate_day_line_sizes(param_data, beta_ticks, beta_upper, beta_lower)
  }

  return(line_sizes)
}

generate_likert_line_sizes <- function(param_data) {

  ##theta_fixed,  theta_rand, alpha_fixed, alpha_rand, epsilon
  likert_pop_values <- c(3, 0.05, 3.32, 0.05, 0.05)
  likert_tick_length <- .05

  #345 points equals range of entire axis; each likert estimation plot y-axis ha range of .20 and tick size of 0.05
  margin_lengths <- (likert_tick_length/.20)*345

  #compute number of margin lengths that fall within each parameter's tolerable error (i.e., 10% of pop value)
  total_margin_lengths <- ((0.1*likert_pop_values)/likert_tick_length)*margin_lengths*2

  #for each parameter, we take 10% of its value and then multiply it by the margin and then by two (for the upper and lower portion)
  line_sizes <- rep(total_margin_lengths, each = nrow(param_data)/length(total_margin_lengths))

  return(line_sizes)
}

generate_day_line_sizes <- function(param_data, beta_ticks, beta_upper, beta_lower) {

  beta_fixed_value <- 180
  #beta_rand, gamma_fixed, gamma_rand
  day_pop_values <- c(10, 20, 4)
  day_tick_length <- 5

  #345 points equals range of entire axis; each day estimation plot y-axis has range of 55 and tick size of 5
  ##beta_fixed length is set separately
  beta_margin_length <-(beta_ticks/(beta_upper - beta_lower))*345
  days_margin_lengths <- (day_tick_length/55)*345

  beta_total_margin_length <- ((0.1*beta_fixed_value)/beta_ticks)*beta_margin_length*2
  other_day_total_margin_lengths <- ((0.1*day_pop_values)/day_tick_length)*days_margin_lengths*2

  all_day_total_margin_lengths <- c(beta_total_margin_length, other_day_total_margin_lengths)

  line_sizes <- rep(all_day_total_margin_lengths, each = nrow(param_data)/length(all_day_total_margin_lengths))

  return(line_sizes)
}


generate_filtered_unfiltered_facet_plot <- function(parameter_data, num_rows = 1, num_cols = 2,
                                              legend_position = c(0.50, .50), legend_direction = 'horizontal',
                                              file_name, dodge_width = 0.8, h_line_alpha = 0.8, h_line_size = 15,
                                              point_size = 15, line_size = 2, error_bar_width = 0.8,
                                              error_bar_size = 2.5,
                                              y_axis_var = 'estimate',  y_axis_name = 'Estimate (days)',
                                              grouping_var = 'number_measurements',
                                              grouping_var2 = 'bias_status',
                                              facet_var = 'parameter',
                                              shape_legend_title = 'Number of \nmeasurements',
                                              panel_spacing_y = 12, dodge_position,
                                              #parameters that change across Experiments 1-3
                                              x_axis_var = 'sample_size', x_axis_name = 'Sample size (*N*)',
                                              beta_lower, beta_upper, ticks, exp_num) {

  dodge_position <- position_dodge(width = dodge_width)

  #create base plot
  base_plot <- create_base_plot_unfiltered(parameter_data = parameter_data, x_axis_var = x_axis_var, y_axis_var = y_axis_var,
                                grouping_var = grouping_var, grouping_var2 = grouping_var2)

  #primary aesthetic specifications (points, lines, error bars, hline)
  plot_visualizations <- create_data_visualizations_unfiltered(dodge_position = dodge_position,
                                                    point_size = point_size, line_size =  line_size,
                                                    lower_ci = lower_ci, upper_ci = upper_ci,
                                                    error_bar_width = error_bar_width, h_line_alpha = h_line_alpha,
                                                    h_line_size = h_line_size)

  #create legend
  legend_details <- create_legend_unfiltered(shape_legend_title = shape_legend_title, x_axis_name =  x_axis_name)

  #facets
  facet_details <- create_filtered_unfiltered_param_facets(facet_var = facet_var, num_rows = num_rows, num_cols = num_cols,
                                            y_axis_name = y_axis_name, beta_lower = beta_lower, beta_upper = beta_upper, beta_ticks = ticks)

  #thematic elements
  thematic_details <- create_thematic_elements(legend_position = legend_position, legend_direction = legend_direction,
                                               panel_spacing_y = panel_spacing_y)


  #assemble plot
  day_parameter_facet_plot <- base_plot + plot_visualizations + legend_details + facet_details + thematic_details

  #create PDF of faceted plot
  set_panel_size(p = day_parameter_facet_plot, height = unit(x = 28, units = 'cm'),
                 width = unit(x = 40, units = 'cm'),
                 file =  file_name)
}


#ggplot base plot
create_base_plot <- function(parameter_data, x_axis_var, y_axis_var, grouping_var, fill_var) {

  base_plot <-  ggplot(data = parameter_data, aes(x = !!sym(x_axis_var), y = !!sym(y_axis_var),
                                                  group = !!sym(grouping_var),
                                                  linetype = !!sym(grouping_var),
                                                  shape = !!sym(grouping_var),
                                                  fill = !!sym(fill_var)))
  return(base_plot)

}

#create base plot
create_base_plot_unfiltered <- function(parameter_data, x_axis_var, y_axis_var, grouping_var,
                                        grouping_var2) {

  base_plot <-  ggplot(data = parameter_data, aes(x = !!sym(x_axis_var), y = !!sym(y_axis_var),
                                                  group = !!sym(grouping_var),
                                                  linetype = !!sym(grouping_var),
                                                  shape = !!sym(grouping_var),
                                                  fill = !!sym(grouping_var2)))
  return(base_plot)

}



#primary aesthetic specifications (points, lines, error bars, hline)
create_data_visualizations_unfiltered <- function(dodge_position, point_size, line_size,
                                lower_ci, upper_ci, error_bar_width,
                                h_line_alpha, h_line_size, grouping_var2, linesizes) {

  primary_plot <- list(
    geom_hline(mapping = aes(yintercept = pop_value), color = 'gray', alpha = 0.43, size = linesizes),
    geom_hline(mapping = aes(yintercept = pop_value), color = 'blue', size = 5, alpha = 0.8),
    geom_errorbar(mapping  = aes(ymin = lower_ci, ymax = upper_ci, color = precision_status),
                  width = error_bar_width, position = dodge_position, size = 3.5),
    geom_point(position = dodge_position, size = point_size),
    geom_line(size = line_size,  position = dodge_position))


  return(primary_plot)
}

create_data_visualizations <- function(dodge_position, point_size, line_size,
                                       lower_ci, upper_ci, error_bar_width,
                                       h_line_alpha, h_line_size) {

  primary_plot <- list(
    geom_hline(mapping = aes(yintercept = pop_value), color = 'gray', alpha = h_line_alpha, size = h_line_size),
    #geom_rect(mapping = aes(ymin = pop_value - 0.1*pop_value, ymax = pop_value + 0.1*pop_value, xmin = -Inf, xmax = Inf)),
    geom_point(position = dodge_position, size = point_size),
    geom_line(size = line_size,  position = dodge_position),
    geom_errorbar(mapping  = aes(ymin = lower_ci, ymax = upper_ci),
                  width = error_bar_width, position = dodge_position, size = 2.5)
  )

  return(primary_plot)
}


#creates legend
create_legend <- function(shape_legend_title, fill_legend_title, x_axis_name) {

  #legend details + x-axis name
  legend_details <- list(
    scale_shape_manual(name = shape_legend_title, values=c(22,21,24,23),
                       guide  = guide_legend(override.aes = list(fill = c("black")))),


    scale_linetype_manual(name = shape_legend_title, values = rev(c('dotted', 'dashed', 'longdash', 'solid'))),

    scale_fill_manual(name = 'Is Biased?',
                     values = c('black', 'white'),
                     labels = c('No', 'Yes'), drop = FALSE), #set drop =FALSE s that unused levels are included

    #order = 1))
    scale_color_manual(name = 'Is Precise?',
                      breaks = c("0", "1"),
                      values = c('black', '#8cb9e3'),
                       labels = c('Yes', 'No'), drop = FALSE,
                       guide = guide_legend(order = 2)),

    guides(shape = guide_legend(order = 1,  override.aes = list(fill = "black")),
           linetype = guide_legend(order = 1),
           fill = guide_legend(order = 2, override.aes = list(shape = 22)),
           color = guide_legend(order = 3)),
    #guides(col = guide_legend(reverse = TRUE)),
    #fill = guide_legend(override.aes = list(shape = 22, fill = c('black', 'white'))),
    labs(x = x_axis_name))

  return(legend_details)
}

create_legend_unfiltered <- function(shape_legend_title, x_axis_name) {

  #legend details + x-axis name
  legend_details <- list(
    scale_shape_manual(name = shape_legend_title, values=c(22,21,24,23)),
    scale_linetype_manual(name = shape_legend_title, values = rev(c('dotted', 'dashed', 'longdash', 'solid'))),
    guides(shape = guide_legend(override.aes = list(fill = "black"), order = 1)),
    #scale_x_discrete(name = x_axis_name))
    labs(x = x_axis_name))

  return(legend_details)
}

#create facets for day-based parameters
create_days_param_facets<- function(facet_var, num_rows, num_cols, y_axis_name,
                                    beta_lower, beta_upper, beta_ticks) {

  #used to set range of other day-based parameters
  beta_range <- beta_upper - beta_lower

  day_facet_details <- facet_wrap_custom( ~ get(facet_var), scales = "free", ncol = num_cols, nrow = num_rows ,
                                          dir = 'h', labeller = label_parsed,
                                          scale_overrides = list(scale_override(1,
                                                                                scale_y_continuous(name =  y_axis_name,
                                                                                                   breaks = seq(from = beta_lower, to = beta_upper, by = beta_ticks),
                                                                                                   limits = c(beta_lower, beta_upper))),
                                                                 scale_override(2, scale_y_continuous(name =  y_axis_name,
                                                                                                      breaks = seq(from = 0, to = 55, by = 5),
                                                                                                      limits = c(0, 55))),
                                                                 scale_override(3,scale_y_continuous(name =  y_axis_name,
                                                                                                     breaks = seq(from = 0, to = 55, by = 5),
                                                                                                     limits = c(0, 55))),
                                                                 scale_override(4, scale_y_continuous(name =  y_axis_name,
                                                                                                      breaks = seq(from = 0, to = 55, by = 5),
                                                                                                      limits = c(0, 55)))))

  return(day_facet_details)

}

#create facets for likert-based parameters
create_likert_param_facets <- function(facet_var, num_rows, num_cols, y_axis_name) {


  likert_facet_details <- facet_wrap_custom( ~ get(facet_var), scales = "free", ncol = num_cols, nrow = num_rows ,
                                             dir = 'h', labeller = label_parsed,
                                             scale_overrides = list(
                                               #theta_fixed
                                               scale_override(1,
                                                              scale_y_continuous(name =  y_axis_name,
                                                                                 breaks = seq(from = 2.9, to = 3.1, by = .05),
                                                                                 limits = c(2.9, 3.1))),
                                               #theta_random
                                               scale_override(3, scale_y_continuous(name =  y_axis_name,
                                                                                    breaks = seq(from = 0, to = 0.20, by = .05),
                                                                                    limits = c(0, 0.20))),

                                               #alpha_fixed
                                               scale_override(2,scale_y_continuous(name =  y_axis_name,
                                                                                   breaks = seq(from = 3.25, to = 3.45, by = .05),
                                                                                   limits = c(3.25, 3.45))),
                                               #alpha_random
                                               scale_override(4, scale_y_continuous(name =  y_axis_name,
                                                                                    breaks = seq(from = 0, to = 0.2, by =.05),
                                                                                    limits = c(0.0, 0.20))),
                                               #epsilon
                                               scale_override(5, scale_y_continuous(name =  y_axis_name,
                                                                                    breaks = seq(from = 0, to = 0.2, by = 0.05),
                                                                                    limits = c(0, 0.2)))))

  return(likert_facet_details)

}

#create facets for day-based parameters
create_filtered_unfiltered_param_facets <- function(facet_var, num_rows, num_cols, y_axis_name,
                                    beta_lower, beta_upper, beta_ticks) {

  day_facet_details <- facet_wrap_custom( ~ get(facet_var), scales = "free", ncol = num_cols, nrow = num_rows ,
                                          dir = 'h', labeller = label_parsed,
                                          scale_overrides = list(scale_override(1,
                                                                                scale_y_continuous(name =  y_axis_name,
                                                                                                   breaks = seq(from = beta_lower, to = beta_upper, by = beta_ticks),
                                                                                                   limits = c(beta_lower, beta_upper))),
                                                                 scale_override(2, scale_y_continuous(name =  y_axis_name,
                                                                                                      breaks = seq(from = 0, to = 40, by = 5),
                                                                                                      limits = c(0, 40)))))
  return(day_facet_details)
}


#thematic elements
create_thematic_elements <- function(legend_position, legend_direction, panel_spacing_y) {

  thematic_elements <- theme_classic(base_family = 'Helvetica') +

    theme(
      #panel details
      strip.background = element_rect(fill = "white", color = "white"),
      #original text size = 60, 150 for pre-results figures
      strip.text.x = element_text(face = 'bold', hjust = 0, size = 60, margin = unit(c(t = 0, r = 0, b = 1, l = 0), "cm")),

      #axis details
      axis.text = element_text(size = 60, color = 'black'),
      axis.title = element_text(size = 70),
      #axis.title.x.bottom = element_markdown(),
      axis.line = element_line(size = 2),
      axis.ticks.length.x = unit(x = 1, units = 'cm'),
      axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "cm")),
      axis.title.y = element_text(margin = unit(c(t = 0, r = 3, b = 0, l = 0), units = 'cm')),
      axis.ticks = element_line(size = 2, colour = 'black'),
      axis.ticks.length.y =  unit(x = 1, units = 'cm'),

      #legend details
      legend.text = element_text(size = 50),
      legend.margin = margin(unit(c(0, 0, 0, 10), "cm")),
      legend.title = element_text(size = 60),
      legend.key.size = unit(3, 'cm'),
      legend.position = legend_position,
      legend.direction = legend_direction,
      legend.box.background = element_rect(colour = 'black', size = 3),

      #panel details
      panel.spacing.y = unit(x = panel_spacing_y, units = 'cm'),
      panel.spacing.x = unit(x = 2, units = 'cm'))

  return(thematic_elements)
}



##functions needed to create customized facet plots
scale_override <- function(which, scale) {
  if(!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) {
    stop("which must be an integer of length 1")
  }

  if(is.null(scale$aesthetics) || !any(c("x", "y") %in% scale$aesthetics)) {
    stop("scale must be an x or y position scale")
  }

  structure(list(which = which, scale = scale), class = "scale_override")
}

CustomFacetWrap <- ggplot2::ggproto(
  "CustomFacetWrap", ggplot2::FacetWrap,
  init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
    # make the initial x, y scales list
    scales <- ggplot2::ggproto_parent(ggplot2::FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)

    if(is.null(params$scale_overrides)) return(scales)

    max_scale_x <- length(scales$x)
    max_scale_y <- length(scales$y)

    # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
    for(scale_override in params$scale_overrides) {
      which <- scale_override$which
      scale <- scale_override$scale

      if("x" %in% scale$aesthetics) {
        if(!is.null(scales$x)) {
          if(which < 0 || which > max_scale_x) stop("Invalid index of x scale: ", which)
          scales$x[[which]] <- scale$clone()
        }
      } else if("y" %in% scale$aesthetics) {
        if(!is.null(scales$y)) {
          if(which < 0 || which > max_scale_y) stop("Invalid index of y scale: ", which)
          scales$y[[which]] <- scale$clone()
        }
      } else {
        stop("Invalid scale")
      }
    }

    # return scales
    scales
  }
)

facet_wrap_custom <- function(..., scale_overrides = NULL) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)

  # sanitize scale overrides
  if(inherits(scale_overrides, "scale_override")) {
    scale_overrides <- list(scale_overrides)
  } else if(!is.list(scale_overrides) ||
            !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
    stop("scale_overrides must be a scale_override object or a list of scale_override objects")
  }

  facet_super$params$scale_overrides <- scale_overrides

  ggproto(NULL, CustomFacetWrap,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}



