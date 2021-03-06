PascalCase <- function(x) {
  assert_that(is.character(x))
  return(gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", x, perl = TRUE))
}

LoadJsonLogFiles <- function(files) {
  assert_that(is.vector(files), is.character(files), length(files) > 0)
  logs <- purrr::map_dfr(files, function(absolute_filename) {
    message(basename(absolute_filename))
    jsonlite::fromJSON(absolute_filename)
  })
  return(logs)
}

LoadActiveMinutesLogs <- function(absolute_file_pattern) {
  assert_that(is.string(absolute_file_pattern))
  path <- dirname(absolute_file_pattern)
  file_pattern <- basename(absolute_file_pattern)
  files <- list.files(path, file_pattern, full.names = TRUE)
  assert_that(length(files) > 0, msg = paste("no log files found in", path))
  logs <- LoadJsonLogFiles(files)
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
    purrr::reduce(dplyr::full_join, by = "dateTime") %>%
    dplyr::mutate(
      date = as.Date(lubridate::parse_date_time(dateTime, "%m/%d/%y H:M:S")),
      SedentaryMinutes = as.numeric(SedentaryMinutes),
      LightlyActiveMinutes = as.numeric(LightlyActiveMinutes),
      ModeratelyActiveMinutes = as.numeric(ModeratelyActiveMinutes),
      VeryActiveMinutes = as.numeric(VeryActiveMinutes)) %>%
    dplyr::select(date, everything(), -dateTime) %>%
    dplyr::arrange(date)
  assert_that(nrow(activity_data) > 0)
  assert_that(!any(duplicated(activity_data$date)))
  return(activity_data)
}

LoadSleepLogs <- function(input_path) {
  assert_that(is.string(input_path))
  kPath <- file.path(input_path, "Sleep")
  kFilePattern <- "^sleep-.*\\.json$"
  files <- list.files(kPath, kFilePattern, full.names = TRUE)
  assert_that(length(files) > 0, msg = paste("no log files found in", kPath))
  sleep_logs <- LoadJsonLogFiles(files)
  assert_that(nrow(sleep_logs) > 0)
  return(sleep_logs)
}

ProcessSleepLogs <- function(sleep_logs) {
  assert_that(is.data.frame(sleep_logs), nrow(sleep_logs) > 0)
  sleep_data <- sleep_logs %>%
    stats::setNames(PascalCase(names(.))) %>%
    dplyr::mutate(
      StartTime = lubridate::as_datetime(StartTime),
      EndTime = lubridate::as_datetime(EndTime)) %>%
    dplyr::mutate(
      date = as.Date(DateOfSleep) - 1,
      DeepMinutes = Levels$summary$deep$minutes,
      RemMinutes = Levels$summary$rem$minutes,
      LightMinutes = Levels$summary$light$minutes,
      WakeMinutes = Levels$summary$wake$minutes) %>%
    dplyr::filter(MainSleep) %>%
    dplyr::select(date, StartTime, EndTime,
                  MinutesAsleep, MinutesAwake, TimeInBed, Efficiency,
                  DeepMinutes, RemMinutes, LightMinutes, WakeMinutes)
  sleep_data <- sleep_data[!duplicated(sleep_data), ]
  sleep_data <- sleep_data %>%
    dplyr::arrange(date)
  assert_that(nrow(sleep_data) > 0)
  assert_that(!any(duplicated(sleep_data$date)))
  return(sleep_data)
}

#' @title Load a Fitbit archive from disk
#'
#' @description Load a Fitbit archive, as downloaded from fitbit.com, Settings,
#' Data Export, Request Data. The downloaded zip file must be extracted into a
#' folder. See below for an example.
#'
#' @param input_path String containing the path of the extracted Fitbit archive.
#' The string should end on a 'FirstnameLastname' directory.
#'
#' @return A data frame keyed on `date` and one column per metric.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- LoadFitbitData("/path/to/MyFitbitData/FirstnameLastname/")
#' head(data)
#' PlotMetrics(data)
#' PlotMetrics(data %>% dplyr::select(date, VeryActiveMinutes))
#' PlotWeeklyMetrics(data)
#' PlotWeekdayMetrics(data)
#' PlotTimeAtRest(data)
#' PlotMinutesAwakeVsStartHour(data)
#' }
LoadFitbitData <- function(input_path) {
  activity_logs <- LoadActivityLogs(input_path)
  activity_data <- ProcessActivityLogs(activity_logs)
  sleep_logs <- LoadSleepLogs(input_path)
  sleep_data <- ProcessSleepLogs(sleep_logs)
  dates <- sort(unique(c(activity_data$date, sleep_data$date)))
  data <- data.frame(date = dates) %>%
    dplyr::full_join(activity_data, by = "date") %>%
    dplyr::full_join(sleep_data, by = "date")
  return(data)
}
