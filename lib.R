# Library functions for importing and parsing Fitbit sensor data.
# Author: khbrodersen@gmail.com.

suppressPackageStartupMessages({
  library(assertthat)
  library(dplyr)
  library(jsonlite)
  library(lubridate)
  library(purrr)
})

PascalCase <- function(x) {
  assert_that(is.character(x))
  return(gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", x, perl = TRUE))
}

DropNA <- function(x) {
  assert_that(is.vector(x))
  return(x[!is.na(x)])
}

LoadActiveMinutesLogs <- function(absolute_file_pattern) {
  assert_that(is.string(absolute_file_pattern))
  path <- dirname(absolute_file_pattern)
  file_pattern <- basename(absolute_file_pattern)
  files <- list.files(path, file_pattern, full.names = TRUE)
  assert_that(length(files) > 0, msg = paste0("no log files found in ", path))
  logs <- purrr::map_dfr(files, function(absolute_filename) {
    message(basename(absolute_filename))
    jsonlite::fromJSON(absolute_filename)
  })
  assert_that(nrow(logs) > 0)
  return(logs)
}

LoadActivityLogs <- function(input_path) {
  assert_that(is.string(input_path))
  kFilePatterns <- list(
    "SedentaryMinutes" = "^sedentary_minutes-.*\\.json$",
    "LightlyActiveMinutes" = "^lightly_active_minutes-.*\\.json$",
    "ModeratelyActiveMinutes" = "^moderately_active_minutes-.*\\.json$",
    "VeryActiveMinutes" = "^very_active_minutes-.*\\.json$")
  kAbsoluteFilePatterns <-
    purrr::map(kFilePatterns, ~file.path(input_path, "Physical Activity", .))
  activity_logs <- purrr::map(kAbsoluteFilePatterns, LoadActiveMinutesLogs)
  assert_that(length(activity_logs) > 0)
  return(activity_logs)
}

ProcessActivityLogs <- function(activity_logs) {
  assert_that(is.list(activity_logs), length(activity_logs) > 0)
  assert_that(all(purrr::map_lgl(activity_logs, is.data.frame)))
  for (i in seq_along(names(activity_logs))) {
    names(activity_logs[[i]])[2] <- names(activity_logs)[i]
  }
  activity_data <- activity_logs %>%
    purrr::reduce(full_join, by = "dateTime") %>%
    dplyr::mutate(
      Date = as.Date(lubridate::parse_date_time(dateTime, "%m/%d/%y H:M:S")),
      SedentaryMinutes = as.numeric(SedentaryMinutes),
      LightlyActiveMinutes = as.numeric(LightlyActiveMinutes),
      ModeratelyActiveMinutes = as.numeric(ModeratelyActiveMinutes),
      VeryActiveMinutes = as.numeric(VeryActiveMinutes)) %>%
    dplyr::select(Date, everything(), -dateTime) %>%
    dplyr::arrange(Date)
  assert_that(nrow(activity_data) > 0)
  assert_that(!any(duplicated(activity_data$Date)))
  return(activity_data)
}
