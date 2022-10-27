#' Computes summary data for each experimental conition/cell.
#'
#' @param param_summary_data parameter summary data (created from compute_parameter_summary)
#' @export
#extracts values for the fixed- and random-effect estimates for one parameter and produces an APA table
print_param_table <- function(table_ready_data, parameter_name,
                              caption_name, col_header_name, IV_names, column_names, footnote
                              ) {

  names(table_ready_data) <- gsub(pattern = "_", replacement = " ", x = names(table_ready_data))

  #extract parameter data columns
  param_columns <- which(str_detect(string = names(table_ready_data), pattern = parameter_name))

  param_data <- table_ready_data %>%
    select(1, 2, param_columns)

  #setup variables
  num_repetitions <- length(col_header_name)
  header_width <- length(column_names)

  #header details
  header_details <- data.frame('col_name' =  c('', '', col_header_name),
                               'col_width' = c(1, 1, rep(header_width, times = num_repetitions)), check.names = F)

  #pop_value_details <- data.frame('col_name' =  c('', '', '3.00', '0.05', '3.32', '0.05'),
  #                             'col_width' = c(1, 1, rep(header_width, times = num_repetitions)), check.names = F)

  table <- kbl(x = param_data, format = 'latex', digits = 2,
      col.names = linebreak(x = c(IV_names, rep(column_names, times = num_repetitions))),
      longtable = T, booktabs = T, centering = T, escape = F,
      linesep = c('', '', '', '\\addlinespace'),
      caption = caption_name,
      align = c(rep('l', times = length(IV_names)),
                rep('c', times = header_width*num_repetitions))) %>%
    column_spec(column = c(1, 2), width = '3cm') %>%
    #add_header_above(header = pop_value_details, escape = F) %>%
    add_header_above(header = header_details, escape = F) %>%
    footnote(escape = F, threeparttable = T, general_title = '',
            general = footnote) %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
    kable_styling(position = 'left',repeat_header_continued = T) %>%
    landscape(margin = '2.54cm')

  return(table)
}

#generates table-ready data sets
create_table_data_sets <- function(param_summary_data, wide_var, first_col, second_col){

  pop_values <- list('theta_fixed' = 3.00,
                     'alpha_fixed' = 3.32,
                     'beta_fixed' = 180,
                     'gamma_fixed' = 20,
                     'theta_rand' = 0.05,
                     'alpha_rand' = 0.05,
                     'beta_rand' = 10,
                     'gamma_rand' = 4,
                     'epsilon' = 0.05)

  param_summary_data <- convert_var_to_sd(param_summary_data = param_summary_data)

  #extract all columns names before bias column
  bias_col_num <- which(names(param_summary_data) == 'num_removed_values')

  #make two copies of parameter estimate table (one is used as a reference and the other is modified in character format)
  param_est_data_char <- generate_parameter_est_data(param_summary_data = param_summary_data,
                                                    bias_col_num = bias_col_num,
                                                    wide_var = wide_var,
                                                    first_col = first_col, second_col = second_col)
  param_est_data_num <- param_est_data_char

  #error bar data
  errorbar_data <- generate_errorbar_data(param_summary_data = param_summary_data,
                                          bias_col_num = bias_col_num,
                                          wide_var = wide_var,
                                          first_col = first_col, second_col = second_col)


  parameter_est_data <- generate_latex_data(param_est_data_char, param_est_data_num, errorbar_data, pop_values)

 ##appends empty superscript square for biased estimates
 #parameter_est_data <- generate_bias_latex(parameter_est_data = parameter_est_data, pop_values = pop_values)

 ##shades cells in light blue if precision is low
 #parameter_est_data <- generate_errorbar_latex(parameter_est_data, errorbar_data, pop_values)


  return(list('estimate_table' = parameter_est_data))
}

generate_errorbar_data <- function(param_summary_data, bias_col_num, wide_var, first_col, second_col) {

  param_summary_data$errorbar_range <- param_summary_data$upper_ci - param_summary_data$lower_ci

  errorbar_data <- param_summary_data %>%
    select(1:(bias_col_num - 1), errorbar_range) %>%
    pivot_wider(values_from = errorbar_range, names_from = parameter, names_prefix = 'range_') %>%
    pivot_wider(values_from = contains('range_'), names_from = !!sym(wide_var), names_prefix = wide_var) %>%
    relocate(!!sym(first_col), .before = !!sym(second_col)) %>%
    arrange(!!sym(first_col), !!sym(second_col))

  return(errorbar_data)
}

generate_latex_data <- function(param_est_data_char, param_est_data_num, errorbar_data, pop_values) {

  #identify columns that contain parameter estimate information by finding column names with numbers in them
  parameter_estimate_index <- which(str_detect(string = names(param_est_data_char), pattern = '\\d'))

  for (col_number in parameter_estimate_index) {

    #identify relevant population value for parameter of interest
    pop_value_index <- str_detect(pattern = names(pop_values), string = names(param_est_data_char)[col_number])
    pop_value <- as.numeric(pop_values[pop_value_index])

    #notice use of [[]] for indexing
    #round column values to two decimal places and convert it to character format
    param_est_data_char[[col_number]] <- format(round(param_est_data_char[[col_number]], digits = 2), nsmall = 2)


    #modify cell contents for whether estimate is biased (add superscript square if this is the case)
    param_est_data_char[[col_number]] <- ifelse(test = param_est_data_num[[col_number]] - pop_value > 0.1*pop_value,
                                                yes = str_c(param_est_data_char[[col_number]], '$^{\\square}$', sep = ''),
                                                no = str_c(param_est_data_char[[col_number]]))


    #modify cell colour for error bar length
    param_est_data_char[[col_number]] <- cell_spec(x = param_est_data_char[[col_number]],
                                                   background = ifelse(test = abs(errorbar_data[[col_number]]) > .2*as.numeric(pop_values[pop_value_index]),
                                                                       yes = '#8cb9e3', no = '#ffffff'),
                                                   format = 'latex', escape = F)

  }

  return(param_est_data_char)

}

generate_parameter_est_data <- function(param_summary_data, bias_col_num, wide_var, first_col, second_col) {

  parameter_est_data <- param_summary_data %>%
    select(1:(bias_col_num - 1), estimate) %>%
    pivot_wider(values_from = estimate, names_from = parameter, names_prefix = 'est_') %>%
    pivot_wider(values_from = contains('est_'), names_from = !!sym(wide_var), names_prefix = wide_var) %>%
    relocate(!!sym(first_col), .before = !!sym(second_col)) %>%
    arrange(!!sym(first_col), !!sym(second_col))

  return(parameter_est_data)

}

extract_beta_fixed_pop_value <- function() {

  #extract numbers after midpoint until end of column name

  ##convert to numeric

}


##obfuscated function
generate_num_removed_data <- function(param_summary_data, bias_col_num, wide_var, first_col, second_col) {

  removed_value_table <- param_summary_data %>%
    select(1:(bias_col_num - 1),  num_removed_values) %>%
    pivot_wider(values_from = num_removed_values, names_from = parameter, names_prefix = 'removed_') %>%
    pivot_wider(values_from = contains('removed_'), names_from = !!sym(wide_var), names_prefix = wide_var) %>%
    relocate(!!sym(first_col), .before = !!sym(second_col)) %>%
    arrange(!!sym(first_col), !!sym(second_col))

  return(removed_value_table)
}

generate_num_removed_value_latex <- function(parameter_est_data) {

  #identify columns that contain parameter estimate information by finding column names with numbers in them
  parameter_estimate_index <- which(str_detect(string = names(parameter_est_data), pattern = '\\d'))

  for (col_number in parameter_estimate_index) {

    ##escape = FALSE so that latex code can be interpreted by compiler
    ##notice use of [[]] for indexing
    parameter_est_data[[col_number]] <- cell_spec(x = format(round(parameter_est_data[[col_number]], digits = 2), nsmall = 2),
                                               background = ifelse(test = abs(parameter_est_data[[col_number]]) > 100, yes = '#eeeeee', no = '#ffffff'),
                                               format = 'latex', escape = F)
    }

  return(parameter_est_data)
}

round_two_decimal_places <- function(parameter_est_data) {

  #identify columns that contain parameter estimate information
  parameter_estimate_index <- which(str_detect(string = names(parameter_est_data), pattern = '\\d'))

  for (col_number in parameter_estimate_index) {

    ##escape = FALSE so that latex code can be interpreted by compiler
    ##notice use of [[]] for indexing; sets all cells to have backgrounud colour of white
    parameter_est_data[[col_number]] <- cell_spec(x = format(round(parameter_est_data[[col_number]], digits = 2), nsmall = 2),
                                                  background = ifelse(test = abs(parameter_est_data[[col_number]]) == 'impossible_value', yes = '#eeeeee', no = '#ffffff'),
                                                  format = 'latex', escape = F)
  }

  return(parameter_est_data)
}


