# Load Packages -----

library(ggplot2)
library(ragg)
library(shiny)
library(thematic)

# Set Options -----

options(
  scipen = 10,
  digits = 5,
  shiny.maxRequestSize = 100 * 1024^2,
  shiny.useragg = TRUE
)

# Set CRAN Mirror -----

if (
  getOption("repos")["CRAN"] == "@CRAN@" ||
    is.na(getOption("repos")["CRAN"])
) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# Set Variables -----

color_primary <- "#0559BE"

# Set `ggplot2` Theme -----

theme_set(
  theme_bw(
    base_size = 14
  )
)

thematic_shiny(
  font = "auto"
)
