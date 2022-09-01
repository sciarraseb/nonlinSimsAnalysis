

#generates data sets likert- and day-based parameters
generate_likert_days_data_sets <- function(summary_data, spacing, exp_num) {

  #compute necessary conversions for beta-fixed (if necessary) + appends labels
  analytical_data <- convert_summary_data_to_analytical(summary_data, exp_num)

  #compute percentage error and bias status (i.e, > 10% error)
  bias_list <- compute_bias_status(analytical_data = analytical_data, exp_num = exp_num)
  analytical_data$perc_error <- bias_list$perc_error
  analytical_data$bias_status <- bias_list$bias_status

  #compute column for margin of error
  analytical_data$ci_status <- compute_ci_status(analytical_data = analytical_data, exp_num = exp_num)

  #center beta_fixed data
  analytical_data <- center_beta_fixed_data(summary_data = analytical_data)

  #extract data for specific measurement spacing condition for Likert-scale parameters
  likert_data_rows <- str_detect(string = analytical_data$parameter, pattern =  'theta|alpha|epsilon')
  likert_data <- analytical_data[likert_data_rows, ]

  #extract data for specific measurement spacing condition for day-scale parameters
  days_data_rows <- str_detect(string = analytical_data$parameter, pattern =  'beta|gamma')
  days_data <- analytical_data[days_data_rows, ]

  #compute ci lengths for days and likert data sets
  likert_data$errorbar_length <- likert_data$upper_ci - likert_data$lower_ci
  days_data$errorbar_length <- days_data$upper_ci - days_data$lower_ci


  return(list('likert' = likert_data,
              'days' = days_data))

}

compute_bias_status <- function(analytical_data, exp_num) {

  if(str_detect(string = exp_num, pattern = '1')) {

    #compute percentage error with respect to pop value of 180 temporarily replace all beta_fixed pop values with 180
    beta_fixed_rows <- which(analytical_data$parameter == 'bold(A:~beta[fixed]~(`Days-to-Halfway`~Elevation))')
    all_other_rows <- which(analytical_data$parameter != 'bold(A:~beta[fixed]~(`Days-to-Halfway`~Elevation))')

    #for beta_fixed rows, compute percentage error relative to a population value of 180
    perc_error <- rep(NA, nrow(analytical_data))
    bias_status <- rep(NA, nrow(analytical_data))

    perc_error[beta_fixed_rows] <- (abs(analytical_data$estimate[beta_fixed_rows] - analytical_data$pop_value[beta_fixed_rows])/180)*100 #percentage error
    #bias_status[beta_fixed_rows] <- factor(ifelse( perc_error[beta_fixed_rows] > 10, yes = 1, no = 0)) #bias status

    perc_error[all_other_rows] <- (abs(analytical_data$estimate[all_other_rows] - analytical_data$pop_value[all_other_rows])
                                   /analytical_data$pop_value[all_other_rows])*100 #percentage error
    bias_status <- factor(ifelse(perc_error > 10, yes = 1, no = 0)) #bias status
  }



  else {
    perc_error <- abs((analytical_data$estimate - analytical_data$pop_value)/analytical_data$pop_value)*100 #percentage error
    bias_status <- factor(ifelse(perc_error > 10, yes = 1, no = 0)) #bias status  }
  }

  return(list('perc_error' = perc_error,
              'bias_status' = bias_status))

}

compute_ci_status <- function(analytical_data, exp_num) {

  if(str_detect(string = exp_num, pattern = '1')) {

    #temporarily replace all beta_fixed pop values with 180
    beta_fixed_rows <- which(analytical_data$parameter == 'bold(A:~beta[fixed]~(`Days-to-Halfway`~Elevation))')
    analytical_data$pop_value[beta_fixed_rows] <- 180
    ci_status <- factor(ifelse(abs(analytical_data$upper_ci - analytical_data$lower_ci) > 2*.10*analytical_data$pop_value, yes = 1, no = 0))
  }
  else {
    ci_status <- factor(ifelse(analytical_data$upper_ci - analytical_data$lower_ci > 2*.10*analytical_data$pop_value, yes = 1, no = 0))
  }

  return(ci_status)

}

#generate analytical version of data
convert_summary_data_to_analytical <- function(summary_data, exp_num) {

  if(str_detect(string = exp_num, pattern = '1')) {

    #center beta_fixed data
    #summary_data <- center_beta_fixed_data(summary_data = summary_data)

    #append parameter labels
    summary_data <- append_parameter_labels(summary_data = summary_data)

    #convert vars to sds for random-effect parameters and epsilon
    analytical_data <- convert_var_to_sd(param_summary_data = summary_data)

  }

  else {

    summary_data <- append_parameter_labels(summary_data = summary_data)
    analytical_data <- convert_var_to_sd(param_summary_data = summary_data)

  }

  return(analytical_data)

}

#centers beta-fixed data (required for experiment 1)
center_beta_fixed_data <- function(summary_data) {

  #code needed for modifications to beta; because midpoint is manipulated, the pop_value column is set to zero (centered value)
  beta_fixed_data <- summary_data %>%
    filter(parameter == 'beta_fixed')%>%
    mutate(estimate = pop_value - estimate, #centers all beta_fixed estimates on zero
           lower_ci = pop_value - lower_ci,
           upper_ci = pop_value - upper_ci,
           lower_ci_90 = pop_value - lower_ci_90,
           upper_ci_90 = pop_value - upper_ci_90,
           pop_value = 0) #needed so that pop_value on plot is zero

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
                                              bquote(expr = 'bold(F:~alpha[fixed]~(Maximal~Elevation))'),
                                              bquote(expr = 'bold(G:~theta[random]~(Baseline))'),
                                              bquote(expr = 'bold(H:~alpha[random]~(Maximal~Elevation))'),
                                              bquote(expr = 'bold(A:~beta[fixed]~(`Days-to-Halfway`~Elevation))'),
                                              bquote(expr = bold(B:~gamma[fixed]~(`Triquarter-Halfway`~'Delta'))),
                                              bquote(expr = 'bold(C:~beta[random]~(`Days-to-Halfway`~Elevation))'),
                                              bquote(expr = bold(D:~gamma[random]~(`Triquarter-Halfway`~'Delta'))),
                                              bquote(expr = 'bold(I~epsilon~(Error))'))) #~~(Error))')))

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

           estimate = sqrt(estimate), #convert to standard deviation units
           sd_estimate = sd(estimate))

  return(param_summary_data)
}

