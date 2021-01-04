# Functions for basic exploratory analysis of Fitbit sensor data.
#
# Example session:
#   source("lib.R")
#   source("analysis.R")
#   data <- LoadFitbitData("/path/to/MyFitbitData/FirstnameLastname/")
#   head(data)
#   PlotMetrics(data)
#   PlotMetrics(data %>% dplyr::select(date, VeryActiveMinutes))
#   PlotAvgMetricsPerWeekday(data)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(tidyr)
  library(zoo)
})

kPlotSize = 16
kColorLightBlue <- "#cfe2f3"
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

PrepDataForPlotAvgMetricsPerWeekday <- function(data) {
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

PlotAvgMetricsPerWeekday <- function(data) {
  data_metrics <- PrepDataForPlotAvgMetricsPerWeekday(data)
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
