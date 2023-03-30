#' Computes summary data for each experimental conition/cell.
#'
#' @param param_summary_data parameter summary data (created from compute_parameter_summary)
#' @export
extract_factor_levels <- function(param_summary_data) {

  factor_col_data <- param_summary_data[ ,unlist(lapply(X = param_summary_data, FUN = is.factor))]
  factor_levels_ls <- lapply(X = factor_col_data, FUN = levels)

  factor_levels_df <- data.frame('col_name' =rep(x = names(factor_levels_ls), times = lapply(X = factor_levels_ls, FUN = length)),
                                 'level_value' = unlist(factor_levels_ls))

  return(factor_levels_df)
}
