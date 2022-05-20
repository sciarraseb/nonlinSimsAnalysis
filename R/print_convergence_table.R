
print_conv_table <- function(table_ready_data,
                             caption_name, col_header_name, IV_names, column_names
) {

  #append latex information
  table_ready_data <- generate_conv_success_latex(conv_success_data = table_ready_data)

  names(table_ready_data) <- gsub(pattern = "_", replacement = " ", x = names(table_ready_data))

  #setup variables
  num_repetitions <- length(col_header_name)
  header_width <- length(column_names)

  #header details
  header_details <- data.frame('col_name' =  c('', '', col_header_name),
                               'col_width' = c(1, 1, rep(header_width, times = num_repetitions)), check.names = F)

  #pop_value_details <- data.frame('col_name' =  c('', '', '3.00', '0.05', '3.32', '0.05'),
  #                             'col_width' = c(1, 1, rep(header_width, times = num_repetitions)), check.names = F)

  table <- kbl(x = table_ready_data, format = 'latex', digits = 2,
               col.names = linebreak(x = c(IV_names, rep(column_names, times = num_repetitions))),
               longtable = T, booktabs = T, centering = T, escape = F,
               linesep = c('', '', '', '\\addlinespace'),
               caption = caption_name,
               align = c(rep('l', times = length(IV_names)),
                         rep('c', times = header_width*num_repetitions))) %>%
    column_spec(column = c(1, 2), width = '3cm') %>%
    column_spec(column = c(3:nrow(table_ready_data)), width = '1cm') %>%
    #add_header_above(header = pop_value_details, escape = F) %>%
    add_header_above(header = header_details, escape = F) %>%
    footnote(escape = F, threeparttable = T, general_title = '\\\\textit{Note.}\\\\hspace{-1.25pc}',
             general = 'Cells shaded in gray indicate conditions where less than 90\\\\% of models converged. \\\\phantom{ indicate conditions where less than 90\\\\% of models converged.}', footnote_as_chunk = F) %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
    kable_styling(position = 'left', bootstrap_options = )
    #landscape(margin = '1cm')

  return(table)
}


generate_conv_success_latex <- function(conv_success_data) {

  #identify columns that contain convergence success information
  conv_success_index <- which(str_detect(string = names(conv_success_data), pattern = '\\d'))

  for (col_number in conv_success_index) {

    ##escape = FALSE so that latex code can be interpreted by compiler
    ##notice use of [[]] for indexing
    conv_success_data[[col_number]] <- cell_spec(x = format(round(conv_success_data[[col_number]], digits = 2), nsmall = 2),
                                                  background = ifelse(test = abs(conv_success_data[[col_number]]) < .90, yes = '#eeeeee', no = '#ffffff'),
                                                  format = 'latex', escape = F)
  }

  return(conv_success_data)
}

generate_conv_success_data <- function(condition_data, target_value = 'mean_perc_error',
                                       first_col, second_col, wide_var, exp_num,
                                       recode_var) {

  conv_table <- create_base_conv_table(condition_data = condition_data,
                                       target_value = target_value,
                                       first_col = first_col,
                                       second_col = second_col,
                                       wide_var = wide_var)

  #recode target column column
  conv_table <- recode_target_col(conv_table = conv_table, exp_num = exp_num, recode_var = recode_var)

  return(conv_table)
}

recode_target_col <- function(conv_table, exp_num, recode_var) {

  #modify correct column
  if (exp_num != 3) {
    conv_table$measurement_spacing <-  factor(conv_table$measurement_spacing,
                                              levels = levels(conv_table$measurement_spacing),
                                              labels = c('Equal', 'Time-interval increasing', 'Time-interval decreasing', 'Middle-and-extreme'))


  }

  else {
    conv_table <- conv_table %>%
      mutate(time_structuredness = fct_recode(time_structuredness,
                                              'Time structured' = 'bold(A:~Time~~Structured)',
                                              'Time unstructured (fast response)' = 'bold(atop("B: Time Unstructured (Fast", paste("Response)")))',
                                              'Time unstructured (slow response)' = 'bold(atop("C: Time Unstructured (Slow", paste("Response)")))'))
  }

  return(conv_table)

}


create_base_conv_table <- function(condition_data, target_value,
                                   first_col, second_col, wide_var) {

  #select all columns before mean_perc_error
  target_col <- which(names(condition_data) == target_value)

  conv_table <- condition_data %>%
    select(1:(target_col - 1), mean_convergence) %>%
    pivot_wider(values_from = mean_convergence, names_from = !!sym(wide_var)) %>%
    relocate(!!sym(first_col), .before = !!sym(second_col)) %>%
    arrange(!!sym(first_col), !!sym(second_col))

  return(conv_table)
}
