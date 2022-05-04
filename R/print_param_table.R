
#extracts values for the fixed- and random-effect estimates for one parameter and produces an APA table
print_param_table <- function(table_ready_data, parameter_name,
                              caption_name, col_header_name, IV_names, column_names
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
    footnote(escape = F, threeparttable = T, general_title = '\\\\textit{Note.}\\\\hspace{-1pc}',
             general = 'Cells shaded in gray indicate instances where less than 90\\\\% of models converged.') %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
    kable_styling(position = 'left') %>%
    landscape(margin = '1cm')

  return(table)
}

#generates table-ready data sets
create_table_data_sets <- function(param_summary_data, wide_var, first_col, second_col){

  #extract all columns names before bias column
  bias_col_num <- which(names(param_summary_data) == 'num_removed_values')

  parameter_est_data <- generate_parameter_est_data(param_summary_data = param_summary_data,
                                                    bias_col_num = bias_col_num,
                                                    wide_var = wide_var,
                                                    first_col = first_col, second_col = second_col)

  removed_value_table <- generate_num_removed_data(param_summary_data = param_summary_data,
                                                                           bias_col_num = bias_col_num,
                                                                           wide_var = wide_var,
                                                                           first_col = first_col, second_col = second_col)

  #round parameter_est_data to two decimal places
  parameter_est_data <- round_two_decimal_places(parameter_est_data = parameter_est_data)

  #add latex information to param_estimate_table
  removed_value_table <- generate_num_removed_value_latex(parameter_est_data = removed_value_table)


  return(list('estimate_table' = parameter_est_data,
              'removed_value_table' = removed_value_table))
}

generate_num_removed_data <- function(param_summary_data, bias_col_num, wide_var, first_col, second_col) {

  removed_value_table <- param_summary_data %>%
    select(1:(bias_col_num - 1),  num_removed_values) %>%
    pivot_wider(values_from = num_removed_values, names_from = parameter, names_prefix = 'removed_') %>%
    pivot_wider(values_from = contains('removed_'), names_from = !!sym(wide_var), names_prefix = wide_var) %>%
    relocate(!!sym(first_col), .before = !!sym(second_col)) %>%
    arrange(!!sym(first_col), !!sym(second_col))

  return(removed_value_table)
}

generate_parameter_est_data <- function(param_summary_data, bias_col_num, wide_var, first_col, second_col) {

  parameter_est_data <- param_summary_data %>%
    select(1:(bias_col_num - 1), estimate) %>%
    pivot_wider(values_from = estimate, names_from = parameter, names_prefix = 'est_') %>%
    pivot_wider(values_from = contains('est_'), names_from = !!sym(wide_var), names_prefix = wide_var) %>%
    relocate(!!sym(first_col), .before = !!sym(second_col)) %>%
    arrange(!!sym(first_col), !!sym(second_col))

  }

generate_num_removed_value_latex <- function(parameter_est_data) {

  #identify columns that contain parameter estimate information
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


