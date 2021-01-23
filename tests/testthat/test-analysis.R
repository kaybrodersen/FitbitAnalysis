test_that("Moving7dAvg works on standard input", {
  # Use `devtools::load_all()` when adding tests for non-exported functions.
  expect_equal(Moving7dAvg(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)),
               c(NA, NA, NA, NA, NA, NA, 4, 5, 6, 7))
})

test_that("Moving7dAvg works on input that is shorter than a week", {
  expect_equal(Moving7dAvg(NULL), NULL)
  expect_equal(Moving7dAvg(1), NA)
  expect_equal(Moving7dAvg(c(1, 2, 3)), c(NA, NA, NA))
})

test_that("IsWeekend works on standard input", {
  expect_equal(IsWeekend(as.Date("2021-01-02")), TRUE)
  expect_equal(IsWeekend(as.Date("2021-01-03")), TRUE)
  expect_equal(IsWeekend(as.Date("2021-01-04")), FALSE)
})

test_that("DateToWeekday works on standard input", {
  weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  expect_equal(DateToWeekday(as.Date("2021-01-01")),
               factor("Fri", levels = weekdays))
})
