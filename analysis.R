# Functions for basic exploratory analysis of Fitbit sensor data.
#
# Example session:
#   source("lib.R")
#   source("analysis.R")
#   data <- LoadFitbitData("/path/to/MyFitbitData/FirstnameLastname/")
#   head(data)
#   PlotMetrics(data)
#   PlotMetrics(data %>% dplyr::select(date, VeryActiveMinutes))
#   PlotWeeklyMetrics(data)
#   PlotWeekdayMetrics(data)
#   PlotTimeAtRest(data)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(tidyr)
  library(zoo)
})

kPlotSize = 16
kColorLightBlue <- "#9fc5e8"
kColorDarkBlue <- "#0b5394"
kColorLightGray <- "#cccccc"

Moving7dAvg <- function(values) {
  return(zoo::rollmean(values, k = 7, align = "right", fill = NA))
}

IsWeekend <- function(dates) {
  return(lubridate::wday(dates) %% 7 <= 1)
}

DateToWeekday <- function(dates) {
  kOrderedWeekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  return(factor(weekdays(dates, abbreviate = TRUE), levels = kOrderedWeekdays))
}

se <- function(x, na.rm = FALSE) {
  assert_that(is.flag(na.rm))
  if (isTRUE(na.rm)) {
    x <- x[!is.na(x)]
  }
  return(sd(x) / sqrt(length(x)))
}

PrepDataForPlotMetrics <- function(data) {
  assert_that(is.data.frame(data))
  assert_that("date" %in% names(data))
  assert_that(sum(purrr::map_lgl(data, is.numeric)) > 0)
  data_metrics <- data %>%
    dplyr::select(date, where(is.numeric)) %>%
    tidyr::pivot_longer(!date, "metric") %>%
    dplyr::group_by(metric) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(moving_7d_avg = Moving7dAvg(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(is_weekend = IsWeekend(date))
  return(data_metrics)
}

PlotMetrics <- function(data) {
  data_metrics <- PrepDataForPlotMetrics(data)
  assert_that(is.data.frame(data_metrics))
  assert_that(all(c("date", "metric", "value") %in% names(data_metrics)))
  ggplot(data_metrics, aes(date, value)) +
    theme_bw(base_size = kPlotSize) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    facet_wrap(~metric, scales = "free") +
    geom_col(aes(fill = is_weekend), show.legend = FALSE) +
    scale_fill_manual(values = c(kColorLightBlue, kColorLightGray)) +
    geom_line(aes(y = moving_7d_avg), color = kColorDarkBlue, size = 1)
}

PrepDataForPlotWeeklyMetrics <- function(data) {
  assert_that(is.data.frame(data))
  assert_that("date" %in% names(data))
  assert_that(sum(purrr::map_lgl(data, is.numeric)) > 0)
  data_metrics <- data %>%
    dplyr::select(date, where(is.numeric)) %>%
    dplyr::mutate(
      date = lubridate::floor_date(date, "weeks", week_start = 1)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(across(everything(), sum), .groups = "drop_last") %>%
    tidyr::pivot_longer(!date, names_to = "metric", values_to = "sum")
  return(data_metrics)
}

PlotWeeklyMetrics <- function(data) {
  data_metrics <- PrepDataForPlotWeeklyMetrics(data)
  assert_that(is.data.frame(data_metrics))
  assert_that(all(c("date", "metric", "sum") %in% names(data_metrics)))
  ggplot(data_metrics, aes(date, sum)) +
    theme_bw(base_size = kPlotSize) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    facet_wrap(~metric, scales = "free") +
    geom_col(fill = kColorLightBlue, show.legend = FALSE) +
    geom_smooth(method = "loess", colour = kColorDarkBlue)
}

PrepDataForPlotWeekdayMetrics <- function(data) {
  assert_that(is.data.frame(data))
  assert_that("date" %in% names(data))
  assert_that(sum(purrr::map_lgl(data, is.numeric)) > 0)
  stat_funs <- list(
    mean = ~mean(.x, na.rm = TRUE),
    se = ~se(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    lower95 = ~quantile(.x, 0.025, na.rm = TRUE),
    upper95 = ~quantile(.x, 0.975, na.rm = TRUE)
  )
  data_metrics <- data %>%
    dplyr::select(date, where(is.numeric)) %>%
    dplyr::mutate(date = DateToWeekday(date)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(across(everything(), stat_funs), .groups = "drop_last") %>%
    tidyr::pivot_longer(!date, c("metric", "statistic"), names_sep = "_") %>%
    tidyr::pivot_wider(names_from = "statistic") %>%
    dplyr::mutate(is_weekend = date %in% c("Sat", "Sun"))
  return(data_metrics)
}

PlotWeekdayMetrics <- function(data) {
  data_metrics <- PrepDataForPlotWeekdayMetrics(data)
  assert_that(is.data.frame(data_metrics))
  assert_that(all(c("date", "metric", "mean", "se") %in% names(data_metrics)))
  ggplot(data_metrics, aes(date, mean)) +
    theme_bw(base_size = kPlotSize) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    facet_wrap(~metric, scales = "free") +
    geom_col(aes(fill = is_weekend), show.legend = FALSE) +
    geom_errorbar(aes(ymin = mean - 2 * se, ymax = mean + 2 * se),
                  width = 0.5) +
    scale_fill_manual(values = c(kColorLightBlue, kColorLightGray))
}

PrepDataForPlotTimeAtRest <- function(data) {
  assert_that(is.data.frame(data))
  assert_that(all(c("date", "StartTime", "EndTime") %in% names(data)))
  data_metrics <- data
  kFixedYear <- lubridate::year(min(data_metrics$date))
  for (var in c("StartTime", "EndTime")) {
    data_metrics$same_day <-
      lubridate::day(data_metrics[[var]]) == lubridate::day(data_metrics$date)
    lubridate::day(data_metrics[[var]])[data_metrics$same_day] <- 1
    lubridate::day(data_metrics[[var]])[!data_metrics$same_day] <- 2
    lubridate::month(data_metrics[[var]]) <- 1
    lubridate::year(data_metrics[[var]]) <- kFixedYear
  }
  data_metrics <- data_metrics %>%
    dplyr::select(date, StartTime, EndTime) %>%
    tidyr::pivot_longer(c(StartTime, EndTime), names_to = "metric",
                        values_to = "time") %>%
    dplyr::mutate(metric = factor(metric, levels = c("StartTime", "EndTime")))
  return(data_metrics)
}

PlotTimeAtRest <- function(data, histogram_bins = 50) {
  data_metrics <- PrepDataForPlotTimeAtRest(data)
  assert_that(is.data.frame(data_metrics))
  assert_that(all(c("date", "metric", "time") %in% names(data_metrics)))
  ggplot(data_metrics, aes(time)) +
    theme_bw(base_size = kPlotSize) +
    facet_grid(metric ~ .) +
    geom_histogram(bins = histogram_bins, fill = kColorLightBlue,
                   color = kColorDarkBlue) +
    ylab("days")
}
