library(checkmate)
library(lubridate)

get_last_week <- function(date = Sys.Date()) {
  assert_date(date)

  lubridate::interval(
    start = floor_date(date, unit = "week") - period(1, "week"),
    end = floor_date(date, unit = "week") - days(1)
  )
}

library(checkmate)
library(lubridate)

get_last_month <- function(date = Sys.Date()) {
  assert_date(date)

  lubridate::interval(
    start = floor_date(date, unit = "month") - period(1, "month"),
    end = floor_date(date, unit = "month") - period(1, "day")
  )
}
