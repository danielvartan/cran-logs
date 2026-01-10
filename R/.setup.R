# Load libraries -----

library(ggplot2)
library(ragg)
library(shiny)
library(thematic)

# Set general options -----

options(
  scipen = 10,
  digits = 5,
  shiny.maxRequestSize = 100 * 1024^2,
  shiny.useragg = TRUE
)

# Set CRAN mirror -----

if (
  getOption("repos")["CRAN"] == "@CRAN@" ||
    is.na(getOption("repos")["CRAN"])
) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# Set variables -----

set.seed(2025)

color_primary <- "#0559BE"

# Set `ggplot2` theme -----

theme_set(
  theme_bw(
    base_size = 14
  )
)

thematic_shiny(
  font = "auto"
)
