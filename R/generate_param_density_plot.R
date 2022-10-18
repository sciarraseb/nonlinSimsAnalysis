

generate_param_density_plot <- function(raw_exp_data, param_summary_data,
                                        spacing, num_measurements, sample_size) {

  file_name <- paste('Figures/beta_fixed_', spacing,'_', num_measurements, '_', sample_size, ".pdf", sep = '')

  #remove parameter estimates from non-converged models; convert var to SD
  cleaned_data <- convert_raw_var_to_sd(raw_data = raw_exp_data)

  #convert IV columns to factors with same level order and level names
  cleaned_data <- recode_factor_vars(cleaned_data = cleaned_data, param_summary_data = param_summary_data)

  #extract condition data for parameter and individual model data
  param_ind_data_ls <- extract_cond_data_param_and_ind(cleaned_data = cleaned_data, param_summary_data = param_summary_data,
                                                       spacing = spacing, num_measurements = num_measurements, sample_size = sample_size)

  base_density_plot <- ggplot(data = param_ind_data_ls$ind_data, mapping = aes(x = beta_fixed)) +
    geom_density(size = 1) +
    coord_flip()

  plot_building_elements <- compute_param_hist_elements(base_density_plot = base_density_plot,
                                                        param_ind_data_ls = param_ind_data_ls)

  param_density <- generate_param_histogram(base_density_plot = base_density_plot,
                           plot_building_elements = plot_building_elements,
                           param_ind_data_ls = param_ind_data_ls)

  ggsave(filename = file_name, plot = param_density, width = 9, height = 6)
}

generate_param_histogram <- function(base_density_plot, plot_building_elements, param_ind_data_ls) {

  param_density <- base_density_plot +
    scale_x_continuous(name = 'Value of Parameter Estimate (ays)',
                       limits = c(150, 210), breaks = seq(from = 150, to = 210, by = 20)) +
    scale_y_continuous(name = 'Density (Proportion of Estimates)')  +

    #shaded filling
    geom_area(data = plot_building_elements$density_df, mapping = aes(x = x, y = y), fill="grey") +
    #error bar
    geom_errorbarh(data = param_ind_data_ls$param_data, inherit.aes = F,
                   mapping = aes(xmin = lower_ci, xmax = upper_ci, y = plot_building_elements$max_density_value+0.002, height = 0.01), size = 1) +

    #vertical dashed lines for error bars
    geom_segment(x = plot_building_elements$lower_ci, xend = plot_building_elements$lower_ci,
                 y = 0, yend = plot_building_elements$max_density_value, linetype = 2, size = 0.5) +
    geom_segment(x = plot_building_elements$upper_ci, xend = plot_building_elements$upper_ci,
                 y = 0, yend = plot_building_elements$max_density_value, linetype = 2, size = 0.5) +
    #plot aesthetics

    theme_classic() +
    theme(axis.text = element_text(size = 20, color = 'black'),
          axis.title = element_text(size = 26, color = 'black'))

  return(param_density)

  #ggsave(filename = file_name, plot = param_density, width = 9, height = 6)
}

compute_param_hist_elements <- function(base_density_plot, param_ind_data_ls) {

  density_data <- ggplot_build(plot = base_density_plot)

  lower_ci <- param_ind_data_ls$param_data$lower_ci
  upper_ci <-param_ind_data_ls$param_data$upper_ci

  #setup variables
  density_lower_x <- min(which(density_data$data[[1]]$x >= lower_ci))
  density_upper_x <- max(which(density_data$data[[1]]$x <= upper_ci))

  density_df <- data.frame('x' = density_data$data[[1]]$x[density_lower_x:density_upper_x],
                           'y' = density_data$data[[1]]$y[density_lower_x:density_upper_x])


  max_density_value <- max(density_data$data[[1]]$y)

  return(list('lower_ci' = lower_ci, 'upper_ci' = upper_ci,
              #'density_lower_x' = density_lower_x, 'density_upper_x' = density_upper_x,
              'max_density_value' = max_density_value,
              'density_df' = density_df))
}

extract_cond_data_param_and_ind <- function(cleaned_data, param_summary_data,
                                            spacing, num_measurements, sample_size){

  #condition_data target rows
  ind_target_rows <- cleaned_data$number_measurements == num_measurements & cleaned_data$sample_size == sample_size &
    cleaned_data$measurement_spacing == spacing
  ind_data <- cleaned_data[ind_target_rows, ]

  param_target_rows <- param_summary_data$number_measurements == num_measurements & param_summary_data$sample_size == sample_size &
    param_summary_data$measurement_spacing == spacing & param_summary_data$parameter == 'beta_fixed'
  param_data <- param_summary_data[param_target_rows, ]

  return(list('ind_data' = ind_data,
              'param_data' = param_data))
}

recode_factor_vars <- function(cleaned_data, param_summary_data) {

  cleaned_data$measurement_spacing <- fct_recode(.f = cleaned_data$measurement_spacing,
                                                 'Equal' = 'equal',
                                                 'Time increasing' = 'time_inc',
                                                 'Time decreasing' = 'time_dec',
                                                 'Middle-and-extreme' = 'mid_ext')
  cleaned_data$number_measurements <- factor(cleaned_data$number_measurements, levels = levels(param_summary_data$number_measurements))
  cleaned_data$sample_size <- factor(cleaned_data$sample_size, levels = levels(param_summary_data$sample_size))

  return(cleaned_data)
}
