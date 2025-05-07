# Load libraries -----

# library(bslib)
library(ggplot2)
library(httpgd) # github.com/nx10/httpgd
library(magrittr)
library(ragg)
library(rlang)
# library(shiny)
library(vscDebugger) # github.com/ManuelHentschel/vscDebugger
# library(thematic)

# Set general options -----

options(
  scipen = 10,
  digits = 5,
  shiny.maxRequestSize = 100 * 1024^2,
  shiny.useragg = TRUE
)

# Set variables -----

set.seed(2025)

color_primary <- "#0559BE"

# Set `ggplot2` theme -----

ggplot2::theme_set(
  ggplot2::theme_bw(
    base_size = 14
  )
)

thematic::thematic_shiny(
  font = "auto"
)
