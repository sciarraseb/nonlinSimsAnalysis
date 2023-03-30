#' Computes summary data for each experimental conition/cell.
#'
#' @param param_summary_data parameter summary data (created from compute_parameter_summary)
#' @export
#identify factor columns in data set
relevel_factors <- function(param_summary_data, factor_levels_df) {

  #target columns numbers
  target_cols <- unlist(lapply(X = param_summary_data, FUN = is.factor))

  #identify factor columns
  factor_col_data <- param_summary_data[ ,target_cols]

  updated_factor_col_data <- data.frame(lapply(X = names(factor_col_data), relevel_ind_factor,
                   param_summary_data = param_summary_data, factor_levels_df = factor_levels_df))

  names(updated_factor_col_data) <- names(factor_col_data)

  #overwrite factor columns of param_summary_data
  param_summary_data[ ,target_cols] <- updated_factor_col_data

  return(param_summary_data)
}



relevel_ind_factor <- function(col_name, param_summary_data, factor_levels_df) {

  #extract factor levels
  factor_levels <- factor_levels_df$level_value[factor_levels_df$col_name == col_name]

  param_summary_data[[col_name]] <- factor(param_summary_data[[col_name]], levels = factor_levels)

  return(param_summary_data[[col_name]])

}
