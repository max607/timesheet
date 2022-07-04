#' Sum up all working hours.
#'
#' @param timesheet A correctly named data.table.
#'
#' @return A string with time in hours.
#' @export
#'
#' @examples
#' timesheet <- add_entry("2021-11-04 12:30:00", "2021-11-04 13:30:00", "Eating in office hours")
#' timesheet <- add_entry("2021-12-04 13:30:00", "2021-12-04 16:30:00", "Powernap", timesheet)
#' time_total(timesheet)
time_total <- function(timesheet) {
  timesheet %>%
    .[, .(sum = sum(difftime(end, start, units = "hours")))] %>%
    as.numeric() %>%
    {paste0(floor(.), "h ", round((. - floor(.)) * 60), "min")}
}

#' Sum up the working hours of month i.
#'
#' @param timesheet A correctly named data.table.
#' @param month Am integer between 1 and 12, defaults to current month.
#' @param year Am integer, defaults to current year.
#' @param work_left Calculate remaining hours instead.
#' @param hours_per_month How many hours are worked per month.
#'
#' @return A string with time in hours.
#' @export
#'
#' @examples
#' timesheet <- add_entry("2021-11-04 12:30:00", "2021-11-04 13:30:00", "Eating in office hours")
#' timesheet <- add_entry("2021-12-04 13:30:00", "2021-12-04 16:30:00", "Powernap", timesheet)
#' time_month(timesheet, month = 11)
#' time_month(timesheet, month = 12)
#' time_month(timesheet, month = 12, work_left = TRUE)
time_month <- function(timesheet, month = data.table::month(Sys.time()), year = data.table::year(Sys.time()),
                       work_left = FALSE, hours_per_month = 39) {
  timesheet %>%
    .[month(start) == month & year(start) == year, .(sum = sum(difftime(end, start, units = "hours")))] %>%
    {if(nrow(.) != 0) as.numeric(.) else 0} %>%
    {if (work_left) hours_per_month - . else .} %>%
    {paste0(floor(.), "h ", round((. - floor(.)) * 60), "min")}
}
