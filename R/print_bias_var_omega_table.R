#' Computes summary data for each experimental conition/cell.
#'
#' @param param_summary_data parameter summary data (created from compute_parameter_summary)
#' @return Returns a data table.
#' @export
print_bias_var_omega_table <- function(exp_data, target_col, target_value,
                                       ind_vars = c('number_measurements', 'midpoint'), ind_var_acronyms,
                                       caption = '$\\upomega^2$ Values for Manipulated Variables With Equal Spacing',
                                       footnote = 'NM = number of measurements, M = population value set for $\\\\upbeta_{fixed}$,
                                       NM x M = interaction between number of measurements and population value set for $\\\\upbeta_{fixed}$',
                                       parameter_labels = c('$\\upbeta_{fixed}$ (Figure \\ref{fig:exp1_plot_equal}A)',
                                                            '$\\upbeta_{random}$ (Figure \\ref{fig:exp1_plot_equal}B)',
                                                            '$\\upgamma_{random}$ (Figure \\ref{fig:exp1_plot_equal}C)',
                                                            '$\\upgamma_{random}$ (Figure \\ref{fig:exp1_plot_equal}D)'))
  {

  bias_var_table <- assemble_bias_var_omega_table(exp_data = exp_data, ind_vars = ind_vars,
                                                  target_col = target_col, target_value = target_value,
                                                  parameter_labels = parameter_labels)


  #generate kable table
  kable_table <- kbl(x = bias_var_table, format = 'latex',
       col.names = c('Parameter', rep(c(ind_var_acronyms), times = 1)),
       longtable = T, booktabs = T, centering = T, escape = F,
       linesep = c('', '', '', ''), align = c('l', rep('c', times = ncol(bias_var_table) - 1)),
       caption = caption) %>%
  column_spec(column = 1, width = '6cm') %>%
  #header
  add_header_above(header = c(' ' = 1, 'Effect' = ncol(bias_var_table) - 1)) %>%
  #footnotes
  footnote(escape = F, threeparttable = T, general_title = '',
           general = footnote) %>%
  #table position
  kable_styling(position = 'left')

  return(kable_table)
}

assemble_bias_var_omega_table <- function(exp_data,
                                          ind_vars = c('number_measurements', 'midpoint'),
                                          target_col, target_value,
                                          parameter_labels) {


 ##generate omega-squared values for bias (based on deviations from population values)
 #pop_deviation <- bind_rows(.x = pmap(.l = list(param = c('beta_fixed', 'beta_rand', 'gamma_fixed', 'gamma_rand'),
 #                                               target_col = target_col,
 #                                               target_value = target_value,
 #                                               dv_var = 'pop_deviation'),
 #                                     .f = compute_day_param_omega_squared,
 #                                     exp_data = exp_data,
 #                                     ind_vars = ind_vars))

  #generate omega-squared values for variability (based on median absolute deviations)
  med_abs_deviation <- bind_rows(.x = pmap(.l = list(param = c('beta_fixed', 'beta_rand', 'gamma_fixed', 'gamma_rand'),
                                                     target_col = target_col,
                                                     target_value = target_value,
                                                     dv_var = 'med_abs_deviation'),
                                           .f = compute_day_param_omega_squared,
                                           exp_data = exp_data,
                                           ind_vars = ind_vars))
  #join data sets
  #bias_var_omega_table <-  left_join(x = pop_deviation, y = med_abs_deviation, by = 'Parameter')

  #update with latex code
  med_abs_deviation$parameter <- parameter_labels

  return(med_abs_deviation)
}


compute_day_param_omega_squared <- function(exp_data,
                                            ind_vars = c('number_measurements', 'midpoint'), param,
                                            target_col, target_value, dv_var = 'med_abs_deviation') {

  long_param_data <- generate_long_param_data(exp_data = exp_data, target_col = target_col, target_value = target_value)
  analytical_data <- compute_var_bias_cols(long_param_data = long_param_data, ind_vars = ind_vars)
  #compute regression
  lm_output <- compute_regression(param = param, analytical_data = analytical_data, dv_var = dv_var, ind_vars = ind_vars)

  constant_terms_list <- compute_constant_terms(lm_output = lm_output, analytical_data = analytical_data,
                                                param = param, ind_vars = ind_vars)
  #setup variables
  num_rows <- nrow(lm_output) - 1

  param_partial_omega <- compute_ind_omega_squared(row_num = 1, lm_output = lm_output, constant_terms_list = constant_terms_list)

  #format partial omega wide
  param_partial_omega$parameter <- param
  param_partial_omega_wide <- param_partial_omega %>% pivot_wider(names_from = 'effect',values_from = 'partial_omega')


  #omega_squared_values <- lapply(X = 1:num_rows, FUN = compute_ind_omega_squared, constant_terms_list = constant_terms_list,
  #                               lm_output = lm_output)

  #omega_output <- c(param, omega_squared_values)

  #format output to be table ready
  #table_ready_output <- format_regression_output(omega_output = omega_output)

  return(param_partial_omega_wide)
}

format_regression_output <- function(omega_output) {

  param_and_effect_names <- unlist(lapply(X = omega_output,FUN =  `[[`, 1))

  #setup variables
  num_effects <- length(param_and_effect_names) - 1
  length_output <- length(param_and_effect_names)

  #extract omega-squared values
  omega_squared_values <- unlist(lapply(X = omega_output[2:(num_effects + 1)],FUN =  `[[`, 2))

  omega_squared_df <- data.frame('Parameter' = rep(param_and_effect_names[1], times = num_effects),
                                 'effect' = param_and_effect_names[2:length_output],
                                 'omega_squared' = format(round(as.numeric(omega_squared_values), digits = 2), nsmall = 2))

  table_ready_output <- omega_squared_df %>%
    pivot_wider(values_from = omega_squared, names_from = effect)

  return(table_ready_output)
}


compute_constant_terms <- function(lm_output, analytical_data, param, ind_vars){

  mean_cell_size <- analytical_data %>%
    filter(parameter == param) %>%
    group_by(across(.cols = c(ind_vars))) %>%
    summarize(cell_size = n()) %>%
    pull(cell_size) %>%
    mean()

  residuals_row <- which(rownames(lm_output) == 'Residuals')
  MS_effects <- lm_output$Mean.Sq[1:(residuals_row - 1)]
  df_effects <- lm_output$Df[1:(residuals_row - 1)]
  MSE <-  lm_output$Mean.Sq[residuals_row]
  #cell size x df_effect_a x df_effect_b for fixed effects
  ##identify row names that do not include a colon (:) so that only main effects degrees of freedom are taken
  target_rows <- subset(lm_output, !grepl(":|Residuals", rownames(lm_output)))
  denominator <- prod(target_rows$Df)*mean_cell_size

  return(list('cell_size' = mean_cell_size,
              'residuals_row' = residuals_row,
              'MS_effects' = MS_effects,
              'df_effects' = df_effects,
              'MSE' = MSE,
              'denominator' = denominator))
}

compute_constant_terms_orig <- function(lm_output, analytical_data, param, ind_vars){

  mean_cell_size <- analytical_data %>%
    filter(parameter == param) %>%
    group_by(across(.cols = c(ind_vars))) %>%
    summarize(cell_size = n()) %>%
    pull(cell_size) %>%
    mean()

  residuals_row <- which(rownames(lm_output) == 'Residuals')
  MSE <-  lm_output$Mean.Sq[residuals_row]
  #cell size x df_effect_a x df_effect_b
  denominator <- prod(lm_output$Df[1:(residuals_row - 2)] + 1)*mean_cell_size

  return(list('cell_size' = mean_cell_size,
              'residuals_row' = residuals_row,
              'MSE' = MSE,
              'denominator' = denominator))
}

compute_ind_omega_squared <- function(row_num, lm_output, constant_terms_list) {

  #terms that differ across each effect
  sigma_effect <- constant_terms_list$df_effects*(constant_terms_list$MS_effects - constant_terms_list$MSE)/constant_terms_list$denominator

  partial_omega_squared <- sigma_effect/(sigma_effect + constant_terms_list$MSE)

  partial_omega_squared_rounded <- format(round(as.numeric(partial_omega_squared), digits = 2), nsmall = 2)

  #extract effect name
  effect_name <- rownames(lm_output)[row_num]

  return(data.frame('effect' = rownames(lm_output)[1:(nrow(lm_output)- 1)],
                    'partial_omega' = partial_omega_squared_rounded))
}

compute_regression <- function(param, analytical_data, dv_var = 'med_abs_deviation', ind_vars) {

  #filter data for specific parameter
  analytical_data <- analytical_data %>%
    filter(parameter == param)

  interaction_term <- str_c(ind_vars, collapse = '*')
  model <- as.formula(paste(dv_var, interaction_term, sep = '~'))

  var_lm_model <- aov(data = analytical_data, formula = model)
  lm_output <- data.frame(anova(var_lm_model))

  return(lm_output)
}

compute_var_bias_cols <- function(long_param_data, ind_vars = c('number_measurements', 'midpoint')){

  #compute median absolute deviation for each parameter in each condition for each parameter; needed for computing effect of IVs on variability
  median_values <- long_param_data %>%
    group_by(across(.cols = c(parameter, ind_vars))) %>%
    summarize(med_value = median(estimate))

  #left join median values, matching on number_measurements and midpoint
  analytical_data <- long_param_data %>%
    left_join(x = median_values, by = c('parameter', ind_vars))

  #compute column of median absolute deviations and deviation from
  analytical_data$med_abs_deviation <- abs(analytical_data$estimate - analytical_data$med_value)
  analytical_data$pop_deviation <- analytical_data$estimate - analytical_data$pop_value

  return(analytical_data)
}

generate_long_param_data <- function(exp_data, target_col, target_value) {

  #extract data for measurement spacing condition for day-unit parameters
  if(is.na(target_col)) {
    exp_data_filtered <- exp_data %>%
     # filter(!!sym(target_col) == target_value) %>%
      select(c(locate_ivs(exp_data), midpoint),  matches('beta|gamma'))
  }

  else{
    exp_data_filtered <- exp_data %>%
      filter(!!sym(target_col) == target_value) %>%
      select(c(locate_ivs(exp_data), midpoint),  matches('beta|gamma'))
  }


  #create placeholder dataframe with population values; needed to left_join() in next pipe
  pop_values <- data.frame('gamma_fixed' = 20,
                           'beta_rand' = 10,
                           'gamma_rand' = 4) %>%
    pivot_longer(cols = 1:3, names_to = 'parameter', values_to = 'pop_value')

  #populate data with population values and pivot to longer
  all_other_param_long <- exp_data_filtered %>%
    select(-'beta_fixed') %>%
    #place parameter estimates in one column
    pivot_longer(cols = contains(c('beta_rand', 'gamma')), names_to = 'parameter', values_to = 'estimate') %>%
    left_join(pop_values, by = 'parameter')

  #populate data with population values of beta_fixed and pivot to longer
  beta_fixed_long <- exp_data_filtered %>%
    select(-contains(c('beta_rand', 'gamma'))) %>%
    mutate(pop_value = midpoint) %>%
    pivot_longer(cols = 'beta_fixed', names_to = 'parameter', values_to = 'estimate')

  complete_data <- rbind(all_other_param_long, beta_fixed_long)
  #tidy code must be used because simply using as.numeric() causes ordering problems
  complete_data <- complete_data %>% mutate(pop_value = as.numeric(pop_value))

  return(complete_data)
}




