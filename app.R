# Load packages ----

# library(bsicons)
# library(bslib)
# library(dplyr)
library(ggplot2)
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

# library(dlstats)
# library(cranlogs)
# library(adjustedcranlogs)

# Source scripts ----

source(here::here("R", "_setup.R"))
source(here::here("R", "utils.R"))

# Set UI ----

ui <- bslib::page_fillable(

  ## Set header -----

  title = "Download Statistics for CRAN Packages",
  lang = "en",
  padding = "2rem",

  shinyFeedback::useShinyFeedback(),

  theme = bslib::bs_theme(
    bg = "white",
    fg = "black",
    primary = color_primary,
    base_font = bslib::font_google("Noto Sans") # "Pacifico"
    # code_font = bslib::font_google("Noto Sans Mono")
  ),

  shiny::tags$head(
    shiny::HTML(
      "
      <script
      async
      defer
      src='https://unpkg.com/github-corners@latest/dist/embed.min.js'
      data-href='https://github.com/danielvartan/cran-logs'
      data-target='_blank'
      ></script>
      "
    ),
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),

  ## Add busy indicator -----

  # shinybusy::add_busy_gif(
  #   src = "https://jeroen.github.io/images/banana.gif",
  #   timeout = 5000,
  #   position = "full-page",
  #   margins = c("2.5rem", "2.5rem"),
  #   overlay_color = "rgba(0, 0, 0, 0.5)",
  #   height = 70,
  #   width = 70
  # ),

  shinybusy::add_busy_spinner(
    spin = "fading-circle",
    color = color_primary,
    timeout = 5000,
    position = "full-page",
    margins = c("2.5rem", "2.5rem"),
    height = 50,
    width = 50
  ),

  # shinybusy::add_busy_bar(
  #   timeout = 1000,
  #   color = "#F5C827",
  #   centered = TRUE,
  #   height = "10px"
  # ),

  ## Set body -----

  bslib::layout_columns(
    col_widths = c(-1, 10, -1),

    bslib::layout_column_wrap(
      # shiny::selectizeInput(
      #   inputId = "package",
      #   label = "Package",
      #   selected = NULL,
      #   choices = NULL,
      #   width = "100%"
      # ),
      shiny::textInput(
        inputId = "package",
        label = "Package",
        value = "mctq",
        width = "100%"
      ),
      shiny::tags$div(
        shiny::tags$p(
          "*",
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
          "* Depending on the selected period, this process may take ",
          "some time."
        ),
        class = "mt-4"
      )
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
        shiny::uiOutput("date_range"),
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
            "Mean: ",
            shiny::tags$span(
              shiny::textOutput("mean", inline = TRUE),
              class = "sidepanel-stats"
            )
          ),
          shiny::tags$p(
            "Std. Deviation: ",
            shiny::tags$span(
              shiny::textOutput("sd", inline = TRUE),
              class = "sidepanel-stats"
            )
          ),
          shiny::tags$p(
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
  ## Set package choices -----

  # Last update: 2025-01-02
  #
  # available_packages <-
  #   utils::available.packages() |>
  #   dplyr::as_tibble() |>
  #   dplyr::pull(Package)

  # available_packages <-
  #   here::here("www", "available-packages.txt") |>
  #   readLines()

  # shiny::updateSelectizeInput(
  #   session,
  #   "package",
  #   choices = available_packages,
  #   selected = "mctq",
  #   server = TRUE,
  #   options = list(maxOptions = length(available_packages))
  # )

  ## Get and set package URL query -----

  # shiny::observe({
  #   query <- parseQueryString(session$clientData$url_search)
  #
  #   if (!is.null(query[["package"]])) {
  #     shiny::updateTextInput(
  #       session,
  #       inputId = "package",
  #       value = query[["package"]]
  #     )
  #   }
  # })

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

  shiny::observe({
    if (isFALSE(is_valid_package())) {
      shinyFeedback::feedbackDanger(
        inputId = "package",
        show = TRUE,
        text = "Package not found."
      )
    } else {
      shinyFeedback::hideFeedback(inputId = "package")
    }
  }) |>
    shiny::bindEvent(input$package, ignoreInit = TRUE)

  ## Get package data -----

  data <- shiny::reactive({
    start_date <- as.Date("2020-01-01")
    end_date <- Sys.Date()

    # cols = c(date, count, package)
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
  })

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
