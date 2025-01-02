# Load packages ----

# library(bsicons)
# library(bslib)
# library(dplyr)
library(ggplot2)
# library(glue)
# library(here)
library(magrittr)
# library(openssl)
# library(plotly)
# library(ragg)
library(rlang)
# library(shiny)
# library(shinybusy)
# library(shinyjs)
# library(showtext)
# library(sysfonts)
# library(thematic)
# library(tsibble)

# Source scripts ----

source(here::here("R", "_setup.R"))
source(here::here("R", "utils.R"))

# Set UI ----

ui <- bslib::page_fillable(
  ## Set header -----

  title = "CRAN Package Download Statistics",
  lang = "en",
  padding = c("0.5rem", "1.5rem", "2rem", "1.5rem"),

  theme = bslib::bs_theme(
    bg = "white",
    fg = "black",
    primary = color_primary,
    base_font = bslib::font_google("Noto Sans") # "Pacifico"
    # code_font = bslib::font_google("Noto Sans Mono")
  ),

  shiny::tags$head(
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),

  shiny::includeHTML(here::here("www", "github-corners.html")),

  ## Add busy indicator -----

  shinybusy::add_busy_bar(
    timeout = 1000,
    color = "#F5C827",
    centered = TRUE,
    height = "10px"
  ),

  ## Set body -----

  bslib::layout_columns(
    col_widths = c(-1, 10, -1),

    bslib::layout_column_wrap(
      shiny::tags$div(
        shiny::textInput(
          inputId = "package",
          label = "Package",
          value = "mctq",
          width = "100%"
        ),
        shiny::tags$p(
          shiny::tags$span(
            shiny::textOutput("package_feedback", inline = TRUE),
            class = "assertion"
          )
        )
      ),
      shiny::tags$div(
        shiny::tags$p(
          bsicons::bs_icon(name = "info-circle"),
          shiny::tags$b(
            shiny::tags$a(
              "Adjusted",
              href = "https://github.com/tylermorganwall/adjustedcranlogs",
              target = "_blank",
              style = "text-decoration: none;"
            )
          ),
          " CRAN Downloads.",
          shiny::tags$br(),
          bsicons::bs_icon(name = "info-circle"),
          "Depending on the selected period, the process may take ",
          "some time."
        ),
        class = "mt-4"
      ),
      max_height = "5rem",
      class = "align-bottom"
    ),

    bslib::layout_column_wrap(
      bslib::value_box(
        title = "Last Day",
        value = shiny::textOutput("last_day"),
        # showcase = bsicons::bs_icon("graph-up"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Last Week",
        value = shiny::textOutput("last_week"),
        # showcase = bsicons::bs_icon("graph-up"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Last Month",
        value = shiny::textOutput("last_month"),
        # showcase = bsicons::bs_icon("graph-up"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Grand Total",
        value = shiny::textOutput("total"),
        # showcase = bsicons::bs_icon("graph-up"),
        theme = "primary"
      )
    ),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::dateRangeInput(
          inputId = "date_range",
          label = "Date Range",
          start = as.Date("2020-01-01"),
          end = Sys.Date(),
          min = as.Date("1993-08-01"),
          max = Sys.Date(),
          width = "100%"
        ),
        shiny::selectInput(
          inputId = "view",
          label = "View",
          choices = c(
            "Daily" = "daily",
            "Weekly" = "weekly",
            "Monthly" = "monthly",
            "Yearly" = "yearly"
          ),
          selected = "weekly",
          width = "100%"
        ),
        shiny::checkboxInput(
          inputId = "cumulative",
          label = "Cumulative?",
          value = FALSE
        ),
        shiny::tags$div(
          shiny::tags$p(
            bsicons::bs_icon(name = "clipboard-data"),
            "Mean: ",
            shiny::tags$span(
              shiny::textOutput("mean", inline = TRUE),
              class = "sidepanel-stats"
            )
          ),
          shiny::tags$p(
            bsicons::bs_icon(name = "clipboard-data"),
            "Std. Deviation: ",
            shiny::tags$span(
              shiny::textOutput("sd", inline = TRUE),
              class = "sidepanel-stats"
            )
          ),
          shiny::tags$p(
            bsicons::bs_icon(name = "clipboard-data"),
            "Median: ",
            shiny::tags$span(
              shiny::textOutput("median", inline = TRUE),
              class = "sidepanel-stats"
            )
          )
        ),
        width = 300
      ),
      bslib::navset_card_tab(
        bslib::nav_panel(
          title = "Plot",
          # shiny::plotOutput("plot")
          plotly::plotlyOutput("plot")
        ),
        bslib::nav_panel(
          title = "Data",
          DT::DTOutput("data")
        )
      )
    )
  )
)

# Set server ----

server <- function(input, output, session) { #nolint
  ## Get and set package URL query -----

  shiny::observe({
    query <- parseQueryString(session$clientData$url_search)

    if (!is.null(query[["package"]])) {
      shiny::updateTextInput(
        session,
        inputId = "package",
        value = query[["package"]]
      )
    }
  })

  # Check `package` input -----

  ## Last update: 2025-01-02
  #
  # available_packages <-
  #   utils::available.packages() |>
  #   dplyr::as_tibble() |>
  #   dplyr::pull(Package)

  available_packages <-
    here::here("www", "available-packages.txt") |>
    readLines()

  is_valid_package <- shiny::reactive({
    if (is.null(input$package)) {
      FALSE
    } else {
      input$package %in% available_packages
    }
  }) |>
    shiny::bindEvent(input$package)

  output$package_feedback <- shiny::reactive({
    if (isTRUE(is_valid_package())) {
      shiny::validate(
        shiny::need(input$package, "Package not found.")
      )
    }
  })

  ## Get package data -----

  data <- shiny::reactive({
    start_date <- as.Date("2020-01-01")
    end_date <- Sys.Date()

    if (isTRUE(is_valid_package())) {
      adjustedcranlogs::adj_cran_downloads(
        packages = input$package |> trimws(),
        from = start_date,
        to = end_date
      ) |>
        dplyr::select(date, adjusted_downloads, adjusted_total_downloads) |>
        dplyr::rename(
          count = adjusted_downloads,
          cum_sum = adjusted_total_downloads
        ) |>
        dplyr::select(date, count, cum_sum) |>
        dplyr::slice(seq(which(.data$count > 0)[1], dplyr::n())) |>
        tsibble::as_tsibble(index = date)
    } else {
      tsibble::tibble(
        date = as.Date(character()),
        count = numeric(),
        cum_sum = numeric()
      ) |>
        tsibble::as_tsibble(index = date)
    }
  })

  ## Filter data by date range -----

  filtered_data <- shiny::reactive({
    data() |>
    dplyr::filter(
      dplyr::between(
        date,
        input$date_range[1] |> as.Date(),
        input$date_range[2] |> as.Date()
      )
    )
  })

  ## Compute value box statistics -----

  output$total <- shiny::reactive({
    filtered_data() |>
      dplyr::pull(count) |>
      sum(na.rm = TRUE)
  }) |>
    shiny::bindEvent({is_valid_package() == TRUE})

  output$last_month <- shiny::reactive({
    filtered_data() |>
      sum_interval(get_last_month(Sys.Date()))
  })

  output$last_week <- shiny::reactive({
    filtered_data() |>
      sum_interval(get_last_week(Sys.Date()))
  })

  output$last_day <- shiny::reactive({
    filtered_data() |>
      dplyr::slice(nrow(filtered_data()) - 1) |>
      dplyr::pull(count) |>
      sum(na.rm = TRUE)
  })

  ## Compute sidebar plot and statistics -----

  .data <- shiny::reactive({
    if (input$view == "daily") {
      out <- filtered_data()
    } else if (input$view == "weekly") {
      out <- filtered_data() |> aggregate_tsibble_index("week")
    } else if (input$view == "monthly") {
      out <- filtered_data() |> aggregate_tsibble_index("month")
    } else if (input$view == "yearly") {
      out <- filtered_data() |> aggregate_tsibble_index("year")
    }

    out |> dplyr::mutate(cum_sum = cumsum(count))
  })

  output$mean <- shiny::reactive({
    mean(.data()$count, na.rm = TRUE) |> round(digits = 2)
  })

  output$sd <- shiny::reactive({
    stats::sd(.data()$count, na.rm = TRUE) |> round(digits = 2)
  })

  output$median <- shiny::reactive({
    stats::median(.data()$count, na.rm = TRUE) |> round(digits = 2)
  })

  plot <- shiny::reactive({
    if (input$cumulative |> as.logical() |> isFALSE()) {
      out <-
        .data() |>
        ggplot2::ggplot(ggplot2::aes(x = date, y = count))
    } else {
      out <-
        .data() |>
        ggplot2::ggplot(ggplot2::aes(x = date, y = cum_sum))
    }

    if (input$view == "yearly") {
      out <-
        out +
        ggplot2::geom_col(linewidth = 0, fill = color_primary) +
        ggplot2::labs(x = "Year", y = "Downloads")
    } else if (input$view == "monthly") {
      out <-
        out +
        ggplot2::geom_line(
          color = color_primary,
          linewidth = 1
        ) +
        tsibble::scale_x_yearmonth() +
        ggplot2::labs(x = "Date", y = "Downloads")
    } else if (input$view == "weekly") {
      out <-
        out +
        ggplot2::geom_line(
          color = color_primary,
          linewidth = 1
        ) +
        tsibble::scale_x_yearweek() +
        ggplot2::labs(x = "Date", y = "Downloads")
    } else if (input$view == "daily") {
      out <-
        out +
        ggplot2::geom_line(
          color = color_primary,
          linewidth = 1
        ) +
        ggplot2::labs(x = "Date", y = "Downloads")
    }

    out |> plotly::ggplotly()
  })

  output$plot <-
    plotly::renderPlotly({plot()})

  output$data <- DT::renderDT({
    .data() |>
      dplyr::as_tibble() |>
      dplyr::mutate(date = as.character(date))
  })
}

# Compile app ----

shiny::shinyApp(ui = ui, server = server)
