#' Function for adding unusual time formats.
#'
#' @param ls_time List of times in "4h 45min" format.
#'
#' @return A sum in "4h 45min" format.
#' @export
#'
#' @examples
#' timesheet <- add_entry("2021-11-04 12:30:00", "2021-12-04 13:30:00", "Eating in office hours")
#' timesheet <- add_entry("2021-12-04 13:30:00", "2021-12-04 16:30:00", "Powernap", timesheet)
#' lapply(11:12, time_month, timesheet = timesheet, work_left = TRUE) %>% time_add()
time_add <- function(ls_time) {
  gsub("[[:alpha:]]+", "", ls_time) %>%
    strsplit(" ") %>% unlist() %>% as.numeric() %>%
    map_at(seq_len(length(.) / 2) * 2, ~ .x / 60) %>%
    do.call(what = sum) %>%
    {paste0(floor(.), "h ", round((. - floor(.)) * 60), "min")}
}

#' Function for reading in a timesheet. (Save timesheet as csv.)
#'
#' @param path Path to timesheet.
#'
#' @return timesheet data.table
#' @export
#'
#' @examples
#' \dontrun{
#' timesheet <- read_timesheet("timesheet.csv")
#' utils::write.csv(timesheet, "timesheet.csv")
#' }
read_timesheet <- function(path) {
  utils::read.csv(path) %>%
    as.data.table() %>%
    {.[, c("start", "end", "X") := .(as.POSIXct(start), as.POSIXct(end), NULL)][]}
}

#' Adding an entry to or creating a timesheet
#'
#' @param timesheet A correctly named data.table.
#' @param start Start of work.
#' @param end End of work.
#' @param work What was done?
#'
#' @return A timesheet.
#' @export
#'
#' @examples
#' timesheet <- add_entry("2021-12-04 12:30:00", "2021-12-04 13:30:00", "Eating in office hours")
#' timesheet <- add_entry("2021-12-04 13:30:00", "2021-12-04 16:30:00", "Powernap", timesheet)
add_entry <- function(start, end, work, timesheet = NULL) {
  if (is.null(timesheet)) data.table(start = as.POSIXct(start), end = as.POSIXct(end), work = work)
    else rbind(timesheet, list(start = as.POSIXct(start), end = as.POSIXct(end), work = work))
}

# format_day_i <- function(x, i) {
#   difftime_hm <- function(time1, time2) {
#     map2_chr(time1, time2, function(time1, time2){
#       difftime(time1, time2, units = "hours") %>%
#         as.numeric() %>%
#         {paste0(floor(.), "h ", round((. - floor(.)) * 60), "min")}
#     })
#   }
#
#   x <- x[i,]
#   x[, .(date = as.Date(start),
#         start = format(start, "%H:%M"),
#         end = format(as.POSIXct(end + 1800, origin = "1970-01-01"), "%H:%M"),
#         lunch_break = "30 min",
#         hours = difftime_hm(end, start))]
# }
