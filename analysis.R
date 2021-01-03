suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(tidyr)
  library(zoo)
})

Moving7dAvg <- function(values) {
  return(zoo::rollmean(values, k = 7, align = "right", fill = NA))
}

IsWeekend <- function(dates) {
  return(lubridate::wday(dates) %% 7 <= 1)
}

kColorLightBlue <- "#cfe2f3"
kColorDarkBlue <- "#0b5394"
kColorLightGray <- "#cccccc"
kPlotSize = 16

PlotMetric <- function(data, metric) {
  metric <- enquo(metric)
  data_plot <- data %>%
    dplyr::mutate(
      Moving7dAvg = Moving7dAvg(!!metric),
      IsWeekend = IsWeekend(Date))
  ggplot(data_plot, aes(x = Date, y = !!metric)) +
    theme_bw(base_size = kPlotSize) +
    geom_col(aes(fill = IsWeekend), show.legend = FALSE) +
    scale_fill_manual(values = c(kColorLightBlue, kColorLightGray)) +
    geom_line(aes(y = Moving7dAvg), color = kColorDarkBlue, size = 1)
}
