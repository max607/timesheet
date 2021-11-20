# data.table, purrr, magrittr, ggplot2

time_total <- function(x) {
  x %>%
    .[, .(sum = sum(difftime(end, start, units = "hours")))] %>%
    as.numeric() %>%
    {paste0(floor(.), "h ", round((. - floor(.)) * 60), "min")}
}

time_month <- function(x, month, work_left = FALSE) {
  x %>%
    .[month(start) == month, .(sum = sum(difftime(end, start, units = "hours")))] %>%
    as.numeric() %>%
    {if (work_left) 39 - .
       else .} %>%
    {paste0(floor(.), "h ", round((. - floor(.)) * 60), "min")}
}
