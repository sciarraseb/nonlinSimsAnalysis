
convert_var_to_sd <- function(param_summary_data) {

  #identify rows with random-effects parameter data
  rand_effects_rows <- str_detect(string = param_summary_data$parameter, pattern = 'rand|epsilon')

  param_summary_data[rand_effects_rows, ] <- param_summary_data[rand_effects_rows, ] %>%
    mutate(upper_ci = sqrt(upper_ci),
           lower_ci = sqrt(lower_ci),
           estimate = sqrt(estimate),
           perc_error = ((pop_value - mean(estimate, na.rm = T))/pop_value)*100,
           sd_estimate = sd(estimate))

  return(param_summary_data)
}


convert_raw_var_to_sd <- function(raw_data) {

  raw_data <- raw_data %>% filter (code == 0)

  #identify rows with random-effects parameter data
  rand_effects_cols <- str_detect(string = names(raw_data), pattern = 'rand|epsilon')

  raw_data[ ,rand_effects_cols] <- sqrt(raw_data[ ,rand_effects_cols])

  return(raw_data)
}
