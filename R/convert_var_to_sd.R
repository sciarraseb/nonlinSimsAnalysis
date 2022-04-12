
convert_var_to_sd <- function(data){

  data <- data %>%
    mutate_at(.vars = which(str_detect(string = names(data), pattern = c('rand|epsilon'))),
              .funs = sqrt) %>%
    filter(code == 0)

  return(data)
}
