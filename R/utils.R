# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck

get_last_week <- function(date = Sys.Date()) {
  prettycheck:::assert_date(date)

  lubridate::interval(
    start =
      lubridate::floor_date(date, unit = "week") -
      lubridate::period(1, "week"),
    end = lubridate::floor_date(date, unit = "week") - lubridate::days(1)
  )
}

# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck

get_last_month <- function(date = Sys.Date()) {
  prettycheck:::assert_date(date)

  lubridate::interval(
    start =
      lubridate::floor_date(date, unit = "month") -
      lubridate::period(1, "month"), # nolint
    end = lubridate::floor_date(date, unit = "month") -
      lubridate::period(1, "day") # nolint
  )
}

# library(dplyr)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck

sum_interval <- function(data, int) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_interval(int)
  prettycheck:::assert_subset(c("date", "count"), names(data))

  data |>
    dplyr::filter(
      dplyr::between(
        date,
        lubridate::int_start(int),
        lubridate::int_end(int)
      )
    ) |>
    dplyr::pull(count) |> # nolint
    sum(na.rm = TRUE)
}

# library(dplyr)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(tsibble)

aggregate_tsibble_index <- function(
  data,
  unit,
  fun = \(x) sum(x, na.rm = TRUE),
  week_start = 1
  ) { # nolint
  unit_choices <- c("day", "week", "month", "quarter", "year")
  unit_choices <- c(unit_choices, paste0(unit_choices, "s"))

  prettycheck:::assert_tibble(data, min.rows = 2, min.cols = 2)
  prettycheck:::assert_subset(c("date", "count"), names(data))
  prettycheck:::assert_function(fun, null.ok = TRUE)
  prettycheck:::assert_choice(week_start, c(1, 7))

  index_var <- tsibble::index_var(data)
  index <- data[[index_var]]

  # Workaround to avoid problems with dplyr::select()
  data <- data |> dplyr::rename(.index = tsibble::index_var(data))

  if (grepl("^day*", unit)) {
    group <- lubridate::floor_date(index, "days") |> as.Date()
  } else if (grepl("^week*", unit)) {
    group <- tsibble::yearweek(index, week_start = week_start)
  } else if (grepl("^month*", unit)) {
    group <- tsibble::yearmonth(index)
  } else if (grepl("^quarter*", unit)) {
    group <- tsibble::yearquarter(index)
  } else if (grepl("^year*", unit)) {
    group <- lubridate::year(index)
  } else {
    group <- lubridate::floor_date(index, unit)
  }

  data |>
    tsibble::index_by(.index2 = group) |>
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        .fns = fun
      )
    ) |>
    dplyr::rename_with(~ gsub("^.index2$", index_var, .x))
}
