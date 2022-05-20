

#generates data sets likert- and day-based parameters
generate_likert_days_data_sets <- function(summary_data, spacing, exp_num) {

  #convert vars to SDs
  summary_data <- convert_var_to_sd(param_summary_data = summary_data)

  #compute necessary conversions for beta-fixed (if necessary) + appends labels
  analytical_data <- append_parameter_labels(summary_data = summary_data)

  #extract data for specific measurement spacing condition for Likert-scale parameters
  likert_data_rows <- str_detect(string = analytical_data$parameter, pattern =  'theta|alpha|epsilon')
  likert_data <- analytical_data[likert_data_rows, ]

  #extract data for specific measurement spacing condition for day-scale parameters
  days_data_rows <- str_detect(string = analytical_data$parameter, pattern =  'beta|gamma')
  days_data <- analytical_data[days_data_rows, ]

  return(list('likert' = likert_data,
              'days' = days_data))

}

#generate analytical version of data
convert_summary_data_to_analytical <- function(summary_data, exp_num) {

  if(str_detect(string = exp_num, pattern = '1')) {

    #center beta_fixed data
    summary_data <- center_beta_fixed_data(summary_data = summary_data)

    #append parameter labels
    summary_data <- append_parameter_labels(summary_data = summary_data)
  }

  else {

    summary_data <- append_parameter_labels(summary_data = summary_data)

  }

  return(summary_data)

}

#centers beta-fixed data (required for experiment 1)
center_beta_fixed_data <- function(summary_data) {

  #code needed for modifications to beta; because midpoint is manipulated, the pop_value column is set to zero (centered value)
  beta_fixed_data <- summary_data %>%
    filter(parameter == 'beta_fixed')%>%
    mutate(estimate = pop_value - estimate,
           lower_ci = pop_value - lower_ci,
           upper_ci = pop_value - upper_ci,
           lower_ci_90 = pop_value - lower_ci_90,
           upper_ci_90 = pop_value - upper_ci_90,
           pop_value = 0)

  #overwrite beta_fixed data
  summary_data[summary_data$parameter == 'beta_fixed', ] <- beta_fixed_data

  return(summary_data)
}

#adds labels to parameter factor values
append_parameter_labels <- function(summary_data) {

  #convert parameter to a factor and provide panel names in labels
  summary_data$parameter <- factor(summary_data$parameter,
                                   levels =  c("theta_fixed", "alpha_fixed",
                                               "theta_rand", "alpha_rand",
                                               "beta_fixed", "gamma_fixed",
                                               "beta_rand","gamma_rand",
                                               "epsilon"),
                                   labels = c(bquote(expr = 'bold(E:~theta[fixed]~(Baseline))'),
                                              bquote(expr = 'bold(F:~alpha[fixed]~(Maximal~elevation))'),
                                              bquote(expr = 'bold(G:~theta[random]~(Baseline))'),
                                              bquote(expr = 'bold(H:~alpha[random]~(Maximal~elevation))'),
                                              bquote(expr = 'bold(A:~beta[fixed]~(`Days-to-halfway`~elevation))'),
                                              bquote(expr = bold(C:~gamma[fixed]~(`Triquarter-halfway`~'delta'))),
                                              bquote(expr = 'bold(B:~beta[random]~(`Days-to-halfway`~elevation))'),
                                              bquote(expr = bold(D:~gamma[random]~(`Triquarter-halfway`~'delta'))),
                                              bquote(expr = 'bold(I:~epsilon~(Error))')))

                                  #labels = c(bquote(expr = 'bold(theta[fixed])'),
                                  #          bquote(expr = 'bold(theta[random])'),
                                  #          bquote(expr = 'bold(alpha[fixed])'),
                                  #          bquote(expr = 'bold(alpha[random])'),
                                  #          bquote(expr = 'bold(beta[fixed])'),
                                  #          bquote(expr = 'bold(beta[random])'),
                                  #          bquote(expr = 'bold(gamma[fixed])'),
                                  #          bquote(expr = 'bold(gamma[random])'),
                                  #          bquote(expr = 'bold(epsilon~(error))')))

  return(summary_data)
}

convert_var_to_sd <- function(param_summary_data) {

  #identify rows with random-effects parameter data
  rand_effects_rows <- str_detect(string = param_summary_data$parameter, pattern = 'rand|epsilon')

  param_summary_data[rand_effects_rows, ] <- param_summary_data[rand_effects_rows, ] %>%
    mutate(upper_ci = sqrt(upper_ci),
           lower_ci = sqrt(lower_ci),

           upper_ci_90 = sqrt(upper_ci_90),
           lower_ci_90 = sqrt(lower_ci_90),

           estimate = sqrt(estimate),
           perc_error = ((pop_value - mean(estimate, na.rm = T))/pop_value)*100,
           sd_estimate = sd(estimate))

  return(param_summary_data)
}

