

#generates data sets likert- and day-based parameters
generate_likert_days_data_sets <- function(summary_data, spacing, exp_num) {

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
           pop_value = 0)

  #overwrite beta_fixed data
  summary_data[summary_data$parameter == 'beta_fixed', ] <- beta_fixed_data

  return(summary_data)
}

#adds labels to parameter factor values
append_parameter_labels <- function(summary_data) {

  #convert parameter to a factor and provide panel names in labels
  summary_data$parameter <- factor(summary_data$parameter,
                                   levels =  c("theta_fixed", "theta_rand",
                                               "alpha_fixed", "alpha_rand",
                                               "beta_fixed", "beta_rand",
                                               "gamma_fixed", "gamma_rand",
                                               "epsilon"),
                                   labels = c(bquote(expr = 'bold(A:~theta[fixed]~(Lower~plateau))'),
                                              bquote(expr = 'bold(B:~theta[random]~(Lower~plateau))'),
                                              bquote(expr = 'bold(C:~alpha[fixed]~(Upper~plateau))'),
                                              bquote(expr = 'bold(D:~alpha[random]~(Upper~plateau))'),
                                              bquote(expr = 'bold(F:~beta[fixed]~(Midpoint))'),
                                              bquote(expr = 'bold(G:~beta[random]~(Midpoint))'),
                                              bquote(expr = 'bold(H:~gamma[fixed]~(Satiation~point))'),
                                              bquote(expr = 'bold(I:~gamma[random]~(Satiation~point))'),
                                              bquote(expr = 'bold(E:~epsilon~(Error))')))

  return(summary_data)
}
