

compute_condition_summary <- function(param_summary_data, facet_var, ind_vars) {

  #converts random effect units from variance to SD, then computes mean
  #param_summary_data <-  convert_var_to_sd(param_summary_data = param_summary_data)

  #convert appropriate condition column to expressions (needed to facet titles)
  target_col <- which(x = names(param_summary_data) == facet_var)
  labels <- generate_labels(facet_var = facet_var)

  param_summary_data[[target_col]] <- factor(x =  param_summary_data[[target_col]],
                                               levels = levels(param_summary_data[[target_col]]),
                                               labels = labels)

  #compute mean percentage error and lower and upper bounds of middle 95% of estimates
  condition_summary <- param_summary_data %>%
    #add facet labels to condition levels
    filter(str_detect(parameter, 'beta|gamma')) %>%   #filter for day-unit parameters
    group_by_at(ind_vars) %>%
    summarize(mean_perc_error = mean(perc_error),
              #mean_lower_ci = mean(lower_ci_perc),
              #mean_upper_ci = mean(upper_ci_perc),
              mean_convergence = mean(num_converged_values)/1000,
              mean_sd = sqrt(mean(sd_estimate^2)))

  #add conv_fail variable; 900 comes from .9*cell size
  condition_summary$conv_fail <-  factor(ifelse(condition_summary$mean_convergence > 900, yes =  0, no = 1))

  return(condition_summary)
}

generate_labels <- function(facet_var) {

  if (facet_var == 'measurement_spacing') {
    #facet panel titles for exp 1 & 2
    labels <-  c(bquote(expr = 'bold(A:~Equal)'),
                 bquote(expr = 'bold(B:~`Time-Interval`~Increasing)'),
                 bquote(expr = 'bold(C:~`Time-Interval`~Decreasing)'),
                 bquote(expr = 'bold(D:~`Middle-and-Extreme`)'))
  }

  else if (facet_var == 'time_structuredness') {
    #facet panel titles for exp 3
    labels <-  c(bquote(expr = 'bold(A:~Time~~Structured)'),
                 bquote(expr = bold(atop("B: Time Unstructured (Fast", paste("Response)")))),
                 bquote(expr =  bold(atop("C: Time Unstructured (Slow", paste("Response)")))))
  }

  return(labels)
}
