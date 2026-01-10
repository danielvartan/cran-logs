library(checkmate)
library(dplyr)
library(lubridate)
library(prettycheck) # github.com/danielvartan/prettycheck
library(tsibble)

aggregate_tsibble_index <- function(
  data,
  unit,
  fun = \(x) sum(x, na.rm = TRUE),
  week_start = 1
) {
  # nolint
  unit_choices <- c("day", "week", "month", "quarter", "year")
  unit_choices <- c(unit_choices, paste0(unit_choices, "s"))

  assert_tibble(data, min.rows = 2, min.cols = 2)
  assert_subset(c("date", "count"), names(data))
  assert_function(fun, null.ok = TRUE)
  assert_choice(week_start, c(1, 7))

  index_var <- index_var(data)
  index <- data[[index_var]]

  # Workaround to avoid problems with select()
  data <- data |> rename(.index = index_var(data))

  if (grepl("^day*", unit)) {
    group <- floor_date(index, "days") |> as.Date()
  } else if (grepl("^week*", unit)) {
    group <- yearweek(index, week_start = week_start)
  } else if (grepl("^month*", unit)) {
    group <- yearmonth(index)
  } else if (grepl("^quarter*", unit)) {
    group <- yearquarter(index)
  } else if (grepl("^year*", unit)) {
    group <- year(index)
  } else {
    group <- floor_date(index, unit)
  }

  data |>
    index_by(.index2 = group) |>
    summarise(
      across(
        .cols = where(is.numeric),
        .fns = fun
      )
    ) |>
    rename_with(~ gsub("^.index2$", index_var, .x))
}
