
#extracts values for the fixed- and random-effect estimates for one parameter and produces an APA table
print_param_table <- function(table_ready_data, parameter_name,
                              caption_name, col_header_name, IV_names, column_names) {

  #extract parameter data columns
  param_columns <- which(str_detect(string = names(table_ready_data), pattern = parameter_name))

  param_data <- table_ready_data %>%
    select(1, 2, param_columns)

  #setup variables
  num_repetitions <- length(col_header_name)
  header_details <- data.frame(col_name =  c('', '', col_header_name),
                               col_width = c(1, 1, rep(6, times = num_repetitions)), check.names = F)

  table <- kbl(x = param_data, format = 'latex', digits = 2,
               col.names = c(IV_names, rep(column_names, times = num_repetitions)),
               longtable = T, booktabs = T, centering = T, escape = F,
               linesep = c('', '', '', '\\addlinespace'),
               caption = caption_name,
               align = 'l') %>%
    column_spec(column = c(1, 2), width = '3cm') %>%
    add_header_above(header = header_details, escape = F, line_sep = 6) %>%
    footnote(escape = F, threeparttable = T) %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
    kable_styling(position = 'left') %>%
    landscape(margin = '1cm')

  return(table)
}


#generates table-ready data sets
create_table_data_sets <- function(param_summary_data, wide_var, first_col, second_col){

  #extract all columns names before bias column
  bias_col_num <- which(names(param_summary_data) == 'bias')

  parameter_est_data <- generate_parameter_est_data(param_summary_data = param_summary_data,
                                                    bias_col_num = bias_col_num,
                                                    wide_var = wide_var,
                                                    first_col = first_col, second_col = second_col)

  #add latex information to param_estimate_table
  parameter_est_data <- generate_param_estimate_latex(parameter_est_data = parameter_est_data)


  removed_value_table <- generate_parameter_est_data(param_summary_data = param_summary_data,
                                                                           bias_col_num = bias_col_num,
                                                                           wide_var = wide_var,
                                                                           first_col = first_col, second_col = second_col)

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

generate_param_estimate_latex <- function(parameter_est_data) {

  #identify columns that contain parameter estimate information
  parameter_estimate_index <- which(str_detect(string = names(parameter_est_data), pattern = '\\d'))

  for (col_number in parameter_estimate_index) {

    ##escape = FALSE so that latex code can be interpreted by compiler
    ##notice use of [[]] for indexing
    parameter_est_data[[col_number]] <-  cell_spec(x = format(round(parameter_est_data[[col_number]], digits = 2), nsmall = 2),
                                               background = ifelse(test = abs(parameter_est_data[[col_number]]) > 100, yes = '#eeeeee', no = '#ffffff'),
                                               format = 'latex', escape = F)
    }

  return(parameter_est_data)
}


