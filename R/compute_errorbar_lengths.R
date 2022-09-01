compute_errorbar_lengths <- function(exp_analytical_days,
                                     iv_level, num_measurements, sample_size) {

  #find column that contains measurement_spacing or time structuredness level
  target_col <- locate_iv_level_column(exp_analytical_days = exp_analytical_days, iv_level = iv_level)

  #filter by number_measurements & sample_size if sample_size was manipulated (i.e., is found in column names)
  errorbar_param_summary <- determine_errorbar_length_comp(exp_analytical_days = exp_analytical_days,
                                                           num_measurements = num_measurements,
                                                           sample_size = sample_size,
                                                           target_col = target_col,
                                                           iv_level = iv_level)

  #round to two decimal places
  errorbar_param_summary$errorbar_length <- round(errorbar_param_summary$errorbar_length, digits = 2)

  return(errorbar_param_summary)
}


determine_errorbar_length_comp <- function(exp_analytical_days, num_measurements, sample_size, target_col, iv_level) {

  #understand which level contains the iv_level tag
  target_col_num <-  which(names(exp_analytical_days) == target_col)
  target_var_levels <- which(str_detect(string = levels(exp_analytical_days[[target_col_num]]), pattern = iv_level))
  target_level_name <- levels(exp_analytical_days[[target_col_num]])[target_var_levels]

  #filter by number_measurements & sample_size if sample_size was manipulated (i.e., is found in column names)
  if ('sample_size' %in% names(exp_analytical_days)) {

    errorbar_param_summary <- exp_analytical_days %>%
      filter(!!sym(target_col) == target_level_name, number_measurements == num_measurements) %>%
      group_by(parameter, sample_size) %>%
      summarize(errorbar_length = mean(errorbar_length))

    #annoying extra filtering because tidyverse code breaks in functions
    errorbar_param_summary <- errorbar_param_summary[errorbar_param_summary$sample_size == sample_size, ]

  }
  else {
    errorbar_param_summary <- exp_analytical_days %>%
      filter(!!sym(target_col) == target_level_name, number_measurements == num_measurements) %>%
      group_by(parameter) %>%
      summarize(errorbar_length = mean(errorbar_length))
  }

  return(errorbar_param_summary)
}

locate_iv_level_column <- function(exp_analytical_days, iv_level) {

  #put all levels of columns in a list
  level_names <- lapply(X = exp_analytical_days, FUN = levels)

  #find which list element contains the iv_level
  search_results <- (lapply(X = level_names, FUN = str_detect, pattern = iv_level))

  #find the list name of the list element that contains iv_level
  target_col <- names(which(lapply(X = search_results, FUN = function(x) {TRUE %in% x}) == T))

  return(target_col)
}


