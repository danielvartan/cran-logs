library(prettycheck) # github.com/danielvartan/prettycheck

library(checkmate)
library(dplyr)
library(lubridate)
library(prettycheck) # github.com/danielvartan/prettycheck

sum_interval <- function(data, int) {
  assert_tibble(data)
  assert_interval(int)
  assert_subset(c("date", "count"), names(data))

  data |>
    filter(
      between(
        date,
        int_start(int),
        int_end(int)
      )
    ) |>
    pull(count) |>
    sum(na.rm = TRUE)
}
