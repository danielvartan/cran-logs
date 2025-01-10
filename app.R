# Load packages ----

# library(bsicons)
# library(bslib)
# library(dplyr)
library(ggplot2)
# library(here)
library(lubritime) # github.com/danielvartan/lubritime
library(magrittr)
# library(plotly)
# library(ragg)
library(rlang)
library(shiny)
# library(shinyvalidate)
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
    base_font = bslib::font_google("Noto Sans")
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

  shiny::useBusyIndicators(spinners = FALSE, pulse = TRUE),
  shiny::busyIndicatorOptions(pulse_height = "10px"),

  ## Set body -----

  bslib::layout_columns(
    col_widths = c(-1, 10, -1),

    bslib::layout_column_wrap(
      shiny::tags$div(
        shiny::textInput(
          inputId = "package",
          label = "Package",
          width = "100%"
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
        theme = "primary"
      ),
      bslib::value_box(
        title = "Last Week",
        value = shiny::textOutput("last_week"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Last Month",
        value = shiny::textOutput("last_month"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Grand Total",
        value = shiny::textOutput("total"),
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
        shiny::tags$div(
          shiny::checkboxInput(
            inputId = "columns",
            label = "Columns?",
            value = FALSE
          ),
          shiny::checkboxInput(
            inputId = "cumulative",
            label = "Cumulative?",
            value = FALSE
          )
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
          ),
          id = "sidepanel-stats-div"
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

server <- function(input, output, session) { # nolint
  ## Get and set package URI query -----

  shiny::observe({
    query <- parseQueryString(session$clientData$url_search)

    if (!is.null(query[["package"]])) {
      shiny::updateTextInput(
        session,
        inputId = "package",
        value = query[["package"]]
      )
    } else {
      shiny::updateTextInput(
        session,
        inputId = "package",
        value = "mctq"
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

  iv <- shinyvalidate::InputValidator$new()

  iv$add_rule("package", shinyvalidate::sv_required())

  iv$add_rule(
    "package",
    shinyvalidate::sv_in_set(
      set = available_packages,
      message_fmt = "Package not found."
    )
  )

  iv$enable()

  ## Get download data -----

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
        dplyr::summarise(
          dplyr::across(
            .cols = dplyr::where(is.numeric),
            .fns = ~ mean(.x, na.rm = TRUE)
          ),
          .by = "date"
        ) |>
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
    if (isTRUE(is_valid_package())) {
      filtered_data() |>
        dplyr::pull(count) |>
        sum(na.rm = TRUE)
    } else {
      as.numeric(NA)
    }
  })

  output$last_month <- shiny::reactive({
    if (isTRUE(is_valid_package())) {
      filtered_data() |> sum_interval(lubritime::get_last_month(Sys.Date()))
    } else {
      as.numeric(NA)
    }
  })

  output$last_week <- shiny::reactive({
    if (isTRUE(is_valid_package())) {
      filtered_data() |> sum_interval(lubritime::get_last_week(Sys.Date()))
    } else {
      as.numeric(NA)
    }
  })

  output$last_day <- shiny::reactive({
    if (isTRUE(is_valid_package())) {
      filtered_data() |>
        dplyr::slice(nrow(filtered_data()) - 1) |>
        dplyr::pull(count) |>
        sum(na.rm = TRUE)
    } else {
      as.numeric(NA)
    }
  })

  ## Compute sidebar plot and statistics -----

  .data <- shiny::reactive({
    if (isTRUE(is_valid_package())) {
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
    } else {
      filtered_data()
    }
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
    if (isTRUE(is_valid_package())) {
      if (input$cumulative |> as.logical() |> isTRUE()) {
        out <-
          .data() |>
          ggplot2::ggplot(ggplot2::aes(x = date, y = cum_sum))
      } else {
        out <-
          .data() |>
          ggplot2::ggplot(ggplot2::aes(x = date, y = count))
      }

      if (input$columns |> as.logical() |> isTRUE()) {
        out <-
          out +
          ggplot2::geom_col(fill = color_primary, linewidth = 0)
      } else {
        out <-
          out +
          ggplot2::geom_line(color = color_primary, linewidth = 1)
      }

      if (input$view == "yearly") {
        NULL
      } else if (input$view == "monthly") {
        out <- out + tsibble::scale_x_yearmonth()
      } else if (input$view == "weekly") {
        out <- out + tsibble::scale_x_yearweek()
      } else if (input$view == "daily") {
        NULL
      }

      out <- out + ggplot2::labs(x = "Year", y = "Downloads")

      out |> plotly::ggplotly()
    } else {
      ggplot2::ggplot() +
        ggplot2::geom_blank() +
        ggplot2::theme_void()
    }
  })

  output$plot <- plotly::renderPlotly({plot()}) # nolint

  output$data <- DT::renderDT(
    {
      .data() |>
        dplyr::as_tibble() |>
        dplyr::mutate(date = as.character(date))
    },
    extensions = "Buttons",
    options = list(
      dom = "Bfrtip",
      buttons = c("copy", "csv", "print")
    ),
    server = FALSE
  )
}

# Compile app ----

shiny::shinyApp(ui = ui, server = server)
