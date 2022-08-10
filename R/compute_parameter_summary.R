#' Computes summary statistics for simulated experiment data.
#'
#' @param data experiment data
#' @exp_condition experimental conditions
#' @return
compute_parameter_summary <- function(data, exp_num) {

  data <- data %>% filter (code == 0)

  #if exp_num is 1, center beta_fixed data on zero
  if (exp_num == 1) {

    pop_value_data <- add_pop_values_beta(data = data)
  }

  else {
    pop_value_data <- add_pop_value_col(data = data)
  }

  #order levels of parameters and measurement_spacing
  ordered_data <- order_param_spacing_levels(data = pop_value_data)

  summary_data <- compute_statistics(data = ordered_data)

  #add conv_fail categorical variable and percentage error categorical variable (i.e, within or outside of 10% margin)
  summary_data$conv_fail <-  factor(ifelse(summary_data$num_removed_values < .1*summary_data$num_converged_values, yes =  0, no = 1))
  #summary_data$bias_status <-  factor(ifelse(summary_data$perc_error > 10, yes =  1, no = 0))


  #set levels for time structuredness in exp 3
  if (exp_num == 3){
    summary_data$time_structuredness <- factor(summary_data$time_structuredness,
                                               levels = c('time_structured', 'fast_response', 'slow_response'),
                                               labels = c('Time structured', 'Time unstructured (fast response)', 'Time unstructured (slow response)'))
  }

  return(summary_data)
}


compute_statistics <- function(data) {

    summary_stats_table <- data %>%
      #compute statistics for each parameter for each experimental variable
      group_by(parameter, .dots = locate_ivs(data)) %>%
      summarize(
       #bias = (mean(pop_value) - mean(estimate, na.rm = T))/sd(estimate, na.rm = T),
       num_removed_values = sum(is.na(estimate)),
       sd_estimate = sd(estimate),
      # var_estimate = var(estimate),


       #lower and upper CI for estimates
       lower_ci = compute_middle_95_estimate(param_data = estimate)[1],
       upper_ci = compute_middle_95_estimate(param_data = estimate)[2],
       lower_ci_90 = compute_middle_90_estimate(param_data = estimate)[1],
       upper_ci_90 = compute_middle_90_estimate(param_data = estimate)[2],

       ##lower and upper CI for percentage change
       #lower_ci_perc = compute_middle_95_perc_change(param_data = estimate, pop_value = pop_value)[1],
       #upper_ci_perc = compute_middle_95_perc_change(param_data = estimate, pop_value = pop_value)[2],

       estimate = mean(estimate, na.rm = T),
       num_converged_values = sum(code == 0),
       pop_value = mean(pop_value))
    #more ennoying errors from tidyverse
    #summary_stats_table$perc_error <- abs(((summary_stats_table$pop_value - summary_stats_table$estimate)/summary_stats_table$pop_value)*100)

      ##not returning this table at the moment
      #estimate_summary <- intermediate_table_2 %>%
      #  pivot_wider(names_from = 'parameter',
      #              values_from = c('bias', 'num_removed_values',  'lower_ci', 'upper_ci', 'sd_estimate', 'estimate', 'num_converged_values', 'perc_error', 'pop_value'))

  return(summary_stats_table)
}

order_param_spacing_levels <- function(data) {

  ordered_data <- data %>%
    mutate('parameter' = factor(parameter, levels = c("theta_fixed", "theta_rand",
                                                      "alpha_fixed", "alpha_rand",
                                                      "beta_fixed", "beta_rand",
                                                      "gamma_fixed", "gamma_rand",
                                                      "epsilon")),
           'measurement_spacing' = factor(measurement_spacing, levels = c('equal', 'time_inc', 'time_dec', 'mid_ext'),
                                          labels = c('Equal spacing',
                                                     'Time-interval increasing',
                                                     'Time-interval decreasing',
                                                     'Middle-and-extreme spacing')))

  return(ordered_data)
}


add_pop_value_col <- function(data=data) {

  pop_values <- data.frame('theta_fixed' = 3,
                          'alpha_fixed' = 3.32,
                          'beta_fixed' = 180,
                          'gamma_fixed' = 20,

                          'theta_rand' = 0.05,
                          'alpha_rand' = 0.05,
                          'beta_rand' = 10,
                          'gamma_rand' = 4,

                          'epsilon' = 0.05) %>%
   pivot_longer(cols = 1:9, names_to = 'parameter', values_to = 'pop_value')

  #intermediate table needed because pipes tend to break down in functions
  pop_values_long <- data %>%
   filter(code == 0) %>%
   #place parameter estimates in one column
   pivot_longer(cols = contains(c('theta', 'alpha', 'beta', 'gamma', 'epsilon')),
                names_to = 'parameter', values_to = 'estimate') %>%
   left_join(pop_values, by = 'parameter')

  return(pop_values_long)
}

add_pop_values_beta <- function(data) {

  #create data frame for beta_fixed and change the population values to equal the midpoint value
  beta_fixed_data <- data %>%
    filter(code == 0) %>%
    #place parameter estimates in one column
    pivot_longer(cols = contains(c('theta', 'alpha', 'beta', 'gamma', 'epsilon')),
                 names_to = 'parameter', values_to = 'estimate') %>%
    filter(parameter == 'beta_fixed') %>%
    mutate(pop_value = midpoint)

  #center beta_fixed estimates on zero

  #create data.frame for all other parameters
  pop_values <- data.frame('theta_fixed' = 3,
                           'alpha_fixed' = 3.32,
                           'gamma_fixed' = 20,

                           'theta_rand' = 0.05,
                           'alpha_rand' = 0.05,
                           'beta_rand' = 10,
                           'gamma_rand' = 4,

                           'epsilon' = 0.05) %>%
    pivot_longer(cols = 1:8, names_to = 'parameter', values_to = 'pop_value')

  #append pop_value column for all other parameters
  all_other_param_data <- data %>%
    filter(code == 0) %>%
    #place parameter estimates in one column
    pivot_longer(cols = contains(c('theta', 'alpha', 'beta', 'gamma', 'epsilon')),
                 names_to = 'parameter', values_to = 'estimate') %>%
    filter(parameter != 'beta_fixed') %>%
    left_join(pop_values, by = 'parameter')


  #merge
  combined_data <- rbind(all_other_param_data, beta_fixed_data)
  combined_data$pop_value <- as.numeric(combined_data$pop_value)

  return(combined_data)
}


#compute middle 95% of a parameter's estimates
compute_middle_95_estimate <- function(param_data) {

  #order data; remove NA values by setting na.last = NA
  parameter_values_ordered <- param_data[order(param_data, na.last = NA)]

  #extract appropriate percentiles
  perc_95_estimate <- quantile(parameter_values_ordered, probs = c(.025, .975), type = 7)

  return(perc_95_estimate)
}

compute_middle_80_estimate <- function(param_data) {

  #order data; remove NA values by setting na.last = NA
  parameter_values_ordered <- param_data[order(param_data, na.last = NA)]

  #extract appropriate percentiles
  perc_80_estimate <- quantile(parameter_values_ordered, probs = c(.1, .9), type = 7)

  return(perc_80_estimate)
}

compute_middle_90_estimate <- function(param_data) {

  #order data; remove NA values by setting na.last = NA
  parameter_values_ordered <- param_data[order(param_data, na.last = NA)]

  #extract appropriate percentiles
  perc_90_estimate <- quantile(parameter_values_ordered, probs = c(.05, .95), type = 7)

  return(perc_90_estimate)
}

#computes middle 95% of percentage bias values
compute_middle_95_perc_change <- function(param_data, pop_value) {

  #order data; remove NA values by setting na.last = NA
  perc_bias_values <- (param_data - pop_value)*100

  perc_bias_values_ordered <- perc_bias_values[order(perc_bias_values, na.last = NA)]

  #extract appropriate percentiles
  perc_95_CI <- quantile(perc_bias_values_ordered, probs = c(.025, .975))

  return(perc_95_CI)
}

