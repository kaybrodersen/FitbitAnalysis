# FitbitAnalysis

<!-- badges: start -->
[![R-CMD-check](https://github.com/kaybrodersen/FitbitAnalysis/workflows/R-CMD-check/badge.svg)](https://github.com/kaybrodersen/FitbitAnalysis/actions)
<!-- badges: end -->

Fitbit devices collect large amounts of sensor data. This package provides a few basic functions to help you start exploring your data in R.

## Step 1: Installing the package

```
# install.packages("devtools")
devtools::install_github("kaybrodersen/FitbitAnalysis")
```

## Step 2: Downloading your Fitbit data

1. Navigate to fitbit.com.
1. Log in and navigate to Profile > [Data Export](https://www.fitbit.com/settings/data/export).
1. Select 'Request Data'.
1. Download your archive when ready.

## Step 3: Exploring your data in R

```
library(FitbitAnalysis)
data <- LoadFitbitData("/path/to/MyFitbitData/FirstnameLastname/")

head(data)

PlotMetrics(data)
PlotMetrics(data %>% dplyr::select(date, VeryActiveMinutes))
PlotWeeklyMetrics(data)
PlotWeekdayMetrics(data)
PlotTimeAtRest(data)
PlotMinutesAwakeVsStartHour(data)
```
