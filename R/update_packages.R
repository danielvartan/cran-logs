# library(dplyr)
# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)
# library(utils))

update_packages <- function() {
  prettycheck::assert_internet()

  packages_file <- here::here("www", "available-packages.txt")

  utils::available.packages() |>
    dplyr::as_tibble() |>
    dplyr::pull(Package) |>
    readr::write_lines(packages_file)

  app_file <- here::here("app.R")

  app_file |>
    readr::read_lines() |>
    stringr::str_replace_all(
      pattern = "Package update: \\d{4}-\\d{2}-\\d{2}",
      replacement = paste0(
        "Package update: ",
        as.character(Sys.Date() |> format("%Y-%m-%d"))
      )
    ) |>
    readr::write_lines(app_file)

  invisible()
}

update_packages()
