#' Generate facetted plots
#'
#' @param data experiment data
#' @exp_condition experimental conditions
#' @return
generate_day_likert_facet_plot <- function(analytical_data, target_col = 'measurement_spacing', target_value, x_axis_var, x_axis_name, exp_num,
                                            beta_lower, beta_upper, ticks) {

  #if exp_num contains 1, center beta estimates
  if (str_detect(string = exp_num, pattern = '1')){

    #extract beta_fixed_data
    beta_rows <- str_detect(string = analytical_data$days$parameter, pattern = 'beta\\[fixed\\]')
    beta_fixed_data <- analytical_data$days[beta_rows, ]

    analytical_data$days$estimate[beta_rows] <- beta_fixed_data$estimate - as.numeric(as.character(beta_fixed_data$pop_value))
  }

  #filter for rows that match measurement spacing/time structuredness condition
  day_parameter_data <- analytical_data$days %>% filter(!!sym(target_col) == target_value)
  likert_parameter_data <- analytical_data$likert %>% filter(!!sym(target_col) == target_value)


  #file names for PDFs
  days_file_name = paste(exp_num, 'plot_days_', tolower(target_value), '.pdf', sep = '')
  likert_file_name = paste(exp_num, 'plot_likert_', tolower(target_value),'.pdf', sep = '')

  #setup variable for dodging
  dodge_position <- position_dodge(width = 0.8)

  #generate day facet plot
  generate_day_parameter_facet_plot(parameter_data = day_parameter_data, file_name = days_file_name,
                                    dodge_position = dodge_position,
                                    beta_lower = beta_lower, beta_upper = beta_upper, ticks = ticks,
                                    #parameters that change across Experiments 1-3
                                    x_axis_var = x_axis_var, x_axis_name = x_axis_name,
                                    exp_num = exp_num)


  #generate likert facet plot
  generate_likert_parameter_facet_plot(parameter_data = likert_parameter_data, file_name = likert_file_name,
                                dodge_position = dodge_position,
                                #parameters that change across Experiments 1-3
                                x_axis_var = x_axis_var, x_axis_name = x_axis_name)
}


generate_day_parameter_facet_plot <- function(parameter_data, num_rows = 2, num_cols = 2,
                                              legend_position = c(0.50, .50), legend_direction = 'horizontal',
                                              file_name, dodge_width = 0.8, h_line_alpha = 0.8, h_line_size = 15,
                                              point_size = 15, line_size = 2, error_bar_width = 0.8,
                                              error_bar_size = 2.5,
                                              y_axis_var = 'estimate',  y_axis_name = 'Estimate (days)',
                                              grouping_var = 'number_measurements',
                                              facet_var = 'parameter',
                                              fill_var = 'conv_fail',
                                              fill_legend_title = 'Convergence \nSuccess',
                                              shape_legend_title = 'Number of \nMeasurements',
                                              panel_spacing_y = 12, dodge_position,
                                              #parameters that change across Experiments 1-3
                                              x_axis_var = 'midpoint', x_axis_name = 'Midpoint location (days)',
                                              beta_lower, beta_upper, ticks, exp_num) {

  #place beta_fixed pop values at 0
  if (str_detect(string = exp_num, pattern = '1')) {
    beta_fixed_rows <- str_detect(string = parameter_data$parameter, pattern = 'beta\\[fixed\\]')
    parameter_data$pop_value[beta_fixed_rows] <- 0
  }

  #create base plot
  base_plot <- create_base_plot(parameter_data = parameter_data, x_axis_var = x_axis_var, y_axis_var = y_axis_var,
                                grouping_var = grouping_var, fill_var = fill_var)

  #primary aesthetic specifications (points, lines, error bars, hline)
  plot_visualizations <- create_data_visualizations(dodge_position = dodge_position,
                                                    point_size = point_size, line_size =  line_size,
                                                    lower_ci = lower_ci, upper_ci = upper_ci,
                                                    error_bar_width = error_bar_width, h_line_alpha = h_line_alpha,
                                                    h_line_size = h_line_size)

  #create legend
  legend_details <- create_legend(shape_legend_title = shape_legend_title,
                                  fill_legend_title = fill_legend_title, x_axis_name =  x_axis_name)

  #facets
  facet_details <- create_days_param_facets(facet_var = facet_var, num_rows = num_rows, num_cols = num_cols,
                                            y_axis_name = y_axis_name, beta_lower = beta_lower, beta_upper = beta_upper, ticks = ticks)

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
                                          y_axis_var = 'estimate',  y_axis_name = 'Estimate (days)',
                                          grouping_var = 'number_measurements',
                                          facet_var = 'parameter',
                                          fill_var = 'conv_fail',
                                          fill_legend_title = 'Convergence \nSuccess',
                                          shape_legend_title = 'Number of \nMeasurements',
                                          panel_spacing_y = 4, dodge_position,
                                          #arguments that need to be changed to Likert facets
                                          legend_position = c(0.70, 0.15), legend_direction = 'vertical', num_rows = 3,
                                          #parameters that change across Experiments 1-3
                                          x_axis_var = 'midpoint', x_axis_name = 'Midpoint location (days)') {

  #create base plot
  base_plot <- create_base_plot(parameter_data = parameter_data, x_axis_var = x_axis_var, y_axis_var = y_axis_var,
                                grouping_var = grouping_var, fill_var = fill_var)

  #primary aesthetic specifications (points, lines, error bars, hline)
  plot_visualizations <- create_data_visualizations(dodge_position = dodge_position,
                                                    point_size = point_size, line_size =  line_size,
                                                    lower_ci = lower_ci, upper_ci = upper_ci,
                                                    error_bar_width = error_bar_width, h_line_alpha = h_line_alpha,
                                                    h_line_size = h_line_size)

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

#ggplot base plot
create_base_plot <- function(parameter_data, x_axis_var, y_axis_var, grouping_var, fill_var) {

  base_plot <-  ggplot(data = parameter_data, aes(x = !!sym(x_axis_var), y = !!sym(y_axis_var),
                                                  group = !!sym(grouping_var),
                                                  linetype = !!sym(grouping_var),
                                                  shape = !!sym(grouping_var),
                                                  fill = !!sym(fill_var)))

  return(base_plot)

}

#primary aesthetic specifications (points, lines, error bars, hline)
create_data_visualizations <- function(dodge_position, point_size, line_size,
                                lower_ci, upper_ci, error_bar_width,
                                h_line_alpha, h_line_size) {

  primary_plot <- list(
    geom_hline(mapping = aes(yintercept = pop_value), color = 'gray', alpha = h_line_alpha, size = h_line_size),
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
    scale_shape_manual(name = shape_legend_title, values=c(22,21,24,23)),
    scale_linetype_manual(name = shape_legend_title, values = rev(c('dotted', 'dashed', 'longdash', 'solid'))),
    scale_fill_manual(name = fill_legend_title,
                    values = c('black', 'white'),
                    labels = c('Above 90%', 'Below 90%'), drop = FALSE), #set drop =FALSE so that unused levels are included
    guides(fill = guide_legend(override.aes = list(shape = 22, fill = c('black', 'white'))),
         shape = guide_legend(override.aes = list(fill = "black"))),
    scale_x_discrete(name = x_axis_name))


  return(legend_details)
}

#create facets for day-based parameters
create_days_param_facets<- function(facet_var, num_rows, num_cols, y_axis_name,

                                                                  beta_lower, beta_upper, ticks) {

  day_facet_details <- facet_wrap_custom( ~ get(facet_var), scales = "free", ncol = num_cols, nrow = num_rows ,
                                          dir = 'h', labeller = label_parsed,
                                          scale_overrides = list(scale_override(1,
                                                                                scale_y_continuous(name =  y_axis_name,
                                                                                                   breaks = seq(from = beta_lower, to = beta_upper, by = ticks),
                                                                                                   limits = c(beta_lower, beta_upper))),
                                                                 scale_override(2, scale_y_continuous(name =  y_axis_name,
                                                                                                      breaks = seq(from = 0, to = 40, by = 5),
                                                                                                      limits = c(0, 40))),
                                                                 scale_override(3,scale_y_continuous(name =  y_axis_name,
                                                                                                     breaks = seq(from = 0, to = 40, by = 5),
                                                                                                     limits = c(0, 40))),
                                                                 scale_override(4, scale_y_continuous(name =  y_axis_name,
                                                                                                      breaks = seq(from = 0, to = 40, by = 5),
                                                                                                      limits = c(0, 40)))))

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
                                               scale_override(2, scale_y_continuous(name =  y_axis_name,
                                                                                    breaks = seq(from = 0, to = 0.20, by = .05),
                                                                                    limits = c(0, 0.20))),

                                               #alpha_fixed
                                               scale_override(3,scale_y_continuous(name =  y_axis_name,
                                                                                   breaks = seq(from = 3.2, to = 3.4, by = .05),
                                                                                   limits = c(3.2, 3.4))),
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

#thematic elements
create_thematic_elements <- function(legend_position, legend_direction, panel_spacing_y) {

  thematic_elements <- theme_classic(base_family = 'Helvetica') +

    theme(
      #panel details
      strip.background = element_rect(fill = "white", color = "white"),
      strip.text.x = element_text(face = 'bold', hjust = 0, size = 65, margin = unit(c(0, 0, 1, 0), "cm")),

      #axis details
      axis.text = element_text(size = 60, color = 'black'),
      axis.title = element_text(size = 70),
      axis.title.x.bottom = element_markdown(),
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



