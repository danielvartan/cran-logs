# Prepare System Requirements -----
#
# Prints the Ubuntu packages that the R packages in `renv.lock` need in order to
# install and load. Posit Package Manager serves Linux binaries built against
# system libraries, so a missing one only surfaces when the package is loaded,
# not when it is installed.
#
# Run it after changing `renv.lock` and paste the result into the `apt-get
# install` layer of the `Dockerfile`:
#
#   Rscript R/prepare-system-requirements.R

library(jsonlite)
library(purrr)

# Set Parameters -----

distribution <- "ubuntu"
release <- "24.04" # Matches the `rocker/r-ver` base image.

# `pandoc` is only needed to render documents with `rmarkdown`, which the
# dashboard never does, and it is a large addition to the image.
excluded <- "pandoc"

# `renv` downloads packages with the command line tool, which is not part of
# any package requirement.
included <- "curl"

# Query Posit Package Manager -----

packages <- read_json("renv.lock") |> pluck("Packages") |> names()

query <- paste0(
  "https://packagemanager.posit.co/__api__/repos/cran/sysreqs",
  "?all=false",
  "&distribution=",
  distribution,
  "&release=",
  release,
  "&",
  paste0("pkgname=", packages, collapse = "&")
)

requirements <-
  query |>
  fromJSON(simplifyVector = FALSE) |>
  pluck("requirements") |>
  map(\(x) x |> pluck("requirements", "packages") |> unlist()) |>
  unlist(use.names = FALSE) |>
  setdiff(excluded) |>
  union(included) |>
  unique() |>
  sort()

# Print Requirements -----

cat("R packages in the lockfile:", length(packages), "\n")
cat("System packages required:", length(requirements), "\n\n")
cat(paste0("    ", requirements, " \\"), sep = "\n")
