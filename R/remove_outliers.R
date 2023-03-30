#' Removes outliers from each generated data set.
#'
#' @param data experiment data
#' @param exp_conditions experimental conditions
#' @export
remove_outliers <- function(data, exp_conditions = locate_ivs(data)) {

  #convert  columns that contain IVs to factors
  exp_cond_names <- exp_conditions
  data <- data %>% mutate_at(.vars = exp_cond_names, factor)

  level_names_ls <- lapply(X = exp_cond_names, FUN = identify_level_names, data = data)

  #create a list of all experimental conditions
  exp_cond_dt <- data.table(expand.grid(level_names_ls))
  names(exp_cond_dt) <- exp_cond_names  #names of data.frame
  exp_cond_list <- split(x = exp_cond_dt, f = seq(nrow(exp_cond_dt)))

  cond_data_ls <- mclapply(X = exp_cond_list,
                           FUN = extract_condition_data,
                           exp_cond_names = names(exp_cond_list$`1`),  #take names from any of the list element data.frames
                           data = data, mc.cores = 3)

  #filter each data set by the median absolute deviation values for each set of parameters
  ##compute median absolute deviation values for each condition
  cond_data_filtered <- mclapply(X = 1:length(cond_data_ls),
                                 FUN = filter_estimates,
                                 cond_data_ls = cond_data_ls, mc.cores = 3)

  #collapse list to data table
  cond_data_filtered <- do.call(rbind, cond_data_filtered)

  return(cond_data_filtered)
}

#filters estimates for each parameter such that they lie within 4 median absolute deviations of the median
filter_estimates <- function (index, cond_data_ls) {

  cond_data <- cond_data_ls[[index]]

  filtered_data <- cond_data %>%
    pivot_longer(cols = contains(c('theta', 'alpha', 'beta', 'gamma', 'epsilon')),
                 names_to = 'parameter', values_to = 'estimate') %>%
    mutate(parameter = as.factor(parameter)) %>%
    group_by(parameter) %>%
    mutate(med_abs_value = compute_median_absolute_dev(data = estimate)) %>% #compute median absolute deviation value
    filter(estimate >= median(estimate) - 4*med_abs_value & estimate <= median(estimate) + 4*med_abs_value) %>%
    dplyr::select(-med_abs_value) %>%
    pivot_wider(names_from = 'parameter', values_from = 'estimate')

  return(filtered_data)
}


#extract data from each condition and place it inside a list
extract_condition_data <- function(exp_cond_values, exp_cond_names, data) {

  ##see link for more on filtering rows by character vector
  ##https://stackoverflow.com/questions/27197617/filter-data-frame-by-character-column-name-in-dplyr
  condition_data <- data %>%
    filter(code == 0,
           .[[exp_cond_names[1]]] == exp_cond_values[[1]],
           .[[exp_cond_names[2]]] == exp_cond_values[[2]],
           .[[exp_cond_names[3]]] == exp_cond_values[[3]])

  return(condition_data)
}

identify_level_names <- function(col_name, data) {

  col_index <- which(names(data) %in% col_name)

  #identify level names
  level_names <- levels(data[[col_index]])

  return(level_names)
}

compute_median_absolute_dev <- function(data) {

  median_data <- median(data)
  med_abs_dev <- median(abs(data - median_data)) * 1.4624

  return(med_abs_dev)
}

locate_ivs <- function(data) {

  #out of the first 5 columns, convert any dbl or chr column to factors; turns out to be all the columns
  data[ ,1:5] <- data[ ,1:5] %>%
    mutate_all(.funs = as.factor)

  #identify experimental factors (i.e., factors with > 1 level)
  iv_levels <- lapply(X = data[ ,1:5], levels)
  exp_factor_index <- which(lapply(iv_levels, length) > 1)

  #extract experimental factor names
  exp_factor_names <- names(exp_factor_index)

  return(exp_factor_names)
}

