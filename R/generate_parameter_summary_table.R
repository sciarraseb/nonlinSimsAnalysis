


generate_column_latex <- function(col_number, parameter_data, abs_cutoff = .40) {

  ##escape = FALSE so that latex code can be interpreted by compiler
  ##notice use of [[]] for indexing
  latex_out <-  cell_spec(x = format(round(parameter_data[[col_number]], digits = 2), nsmall = 2),
                          background = ifelse(test = abs(parameter_data[[col_number]]) > abs_cutoff, yes = '#eeeeee', no = '#ffffff'),
                          format = 'latex', escape = F)

  return(latex_out)
}

#replace each column with the corresponding latex code that specifies the cell shade (along with the bias value)
add_bias_information <- function(parameter_data){

  #find columns that begin with a number (sample size value)
  parameter_estimate_index <- which(str_detect(string = names(parameter_data), pattern = '^\\d'))

  for (column in parameter_estimate_index) {

    #overwite each column of bias values with latex code
    parameter_data[ ,column] <- generate_column_latex(col_number = column, parameter_data = parameter_data)
  }

  return(parameter_data)
}

print_table <- function(data, caption_name, col_header_name,IV_names,column_names) {

  num_repetitions <- length(col_header_name)

  header_details <- data.frame(col_name =  c('', '', col_header_name),
                               col_width = c(1, 1, rep(6, times = num_repetitions)), check.names = F)

  table <- kbl(x = data, format = 'latex', digits = 2,
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

create_data_tables <- function(exp_data) {

  #filter out values for each parameter under each condition
  exp_filtered <- remove_outliers(data = exp_data)

  #convert variance values to SD values for random effect parameters
  exp_cleaned <- convert_var_to_sd(param_summary_data = exp_filtered)

  #compute bias values
  summary_exp <- compute_estimate_summary(data = exp_cleaned)

  #extract bias columns
  reordered_columns <- unlist(lapply(X = c("bias_theta_fixed", "bias_theta_rand",
                                           "bias_alpha_fixed", "bias_alpha_rand",
                                           "bias_beta_fixed", "bias_beta_rand",
                                           "bias_gamma_fixed", "bias_gamma_rand",
                                           "bias_epsilon"), FUN = extract_parameter_columns, summary_exp
  ))

  #reorder columns so that all bias values for fixed values are in one column and all values for random effects are in another column
  #create table_ready data for bias values
  bias_values_table <-  summary_exp[ ,c(1:3, reordered_columns)] %>%
    pivot_wider(names_from = sample_size, values_from = contains('bias')) %>%
    mutate(measurement_spacing = factor(measurement_spacing, levels = c('equal', 'time_inc', 'time_dec', 'mid_ext'),
                                        labels = c('Equal', 'Time inceasing', 'Time decreasing', 'Middle-extreme'))) %>%
    arrange(measurement_spacing)

  table_data_removed <- summary_exp %>%
    select(c(1:3), contains('num_removed')) %>%
    pivot_wider(names_from = sample_size, values_from = contains('num_removed')) %>%
    mutate(measurement_spacing = factor(measurement_spacing, levels = c('equal', 'time_inc', 'time_dec', 'mid_ext'),
                                        labels = c('Equal', 'Time inceasing', 'Time decreasing', 'Middle-extreme'))) %>%
    arrange(measurement_spacing)

  return(list('bias_values' = bias_values_table,
              'removed_values' = table_data_removed))

}
