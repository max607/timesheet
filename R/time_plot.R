overview <- function(x) {
  # data for plotting
  x <- copy(x)
  x[, date := as.Date(start)]
  x[, c("time_start", "time_end") :=  {format(.SD, "%H:%M") %>% paste("2021-01-01", .) %>%
      as.POSIXct() %>% {list(.[seq_len(length(.)/2)], .[seq_len(length(.)/2) + length(.)/2])}},
    .SDcols = c("start", "end")]

  # data for labels
  seconds <- apply(x, 1, function(session) difftime(session[[2]], session[[1]], units = "secs"))
  positions <- map2_dbl(x[, time_start], seconds/2, `+`) %>% as.POSIXct(origin = "1970-01-01")
  dt_label <- data.table(x_coord = x[, date], y_coord = positions, label = seconds)
  dt_label[, label := paste0(round(label / 3600, 2), "h")]

  # melt
  x %>% .[, session := seq_len(.N)] %>%
    melt(measure.vars = c("time_start", "time_end")) %>%

    # plot
    ggplot(aes(x = date, y = value)) +
    geom_point() +
    geom_line(aes(group = session)) +
    geom_text(aes(x = x_coord, y = y_coord, label = label), data = dt_label, hjust = 0) +

    # axes
    labs(x = "Tag", y = "Zeit", title = "Arbeitsstunden")
}
