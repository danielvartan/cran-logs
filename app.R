# Load packages ----

library(adjustedcranlogs)
library(bsicons)
library(bslib)
library(cranlogs)
library(dplyr)
library(DT)
library(ggplot2)
library(here)
library(lubritime) # github.com/danielvartan/lubritime
library(magrittr)
library(plotly)
library(ragg)
library(rlang)
library(shiny)
library(shinyvalidate)
library(stats)
library(tsibble)

# Source scripts ----

here("R", ".setup.R") |> source()
here("R", "utils.R") |> source()

# Set UI ----

ui <- page_fillable(
  ## Set header -----

  title = "CRAN Package Download Statistics",
  lang = "en",
  padding = c("0.5rem", "1.5rem", "2rem", "1.5rem"),

  theme = bs_theme(
    bg = "white",
    fg = "black",
    primary = color_primary,
    base_font = font_google("Noto Sans")
    # code_font = font_google("Noto Sans Mono")
  ),

  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),

  here("www", "github-corners.html") |> includeHTML(),

  ## Add busy indicator -----

  useBusyIndicators(spinners = FALSE, pulse = TRUE),
  busyIndicatorOptions(pulse_height = "10px"),

  ## Set body -----

  layout_columns(
    col_widths = c(-1, 10, -1),

    layout_column_wrap(
      tags$div(
        textInput(
          inputId = "package",
          label = "Package",
          width = "100%"
        )
      ),
      tags$div(
        tags$p(
          bs_icon(name = "info-circle"),
          tags$b(
            tags$a(
              "Adjusted",
              href = "https://github.com/tylermorganwall/adjustedcranlogs",
              target = "_blank",
              style = "text-decoration: none;"
            )
          ),
          " CRAN Downloads.",
          tags$br(),
          bs_icon(name = "info-circle"),
          "Depending on the selected period, the process may take ",
          "some time."
        ),
        class = "mt-4"
      ),
      max_height = "5rem",
      class = "align-bottom"
    ),

    layout_column_wrap(
      value_box(
        title = "Last Day",
        value = textOutput("last_day"),
        theme = "primary"
      ),
      value_box(
        title = "Last Week",
        value = textOutput("last_week"),
        theme = "primary"
      ),
      value_box(
        title = "Last Month",
        value = textOutput("last_month"),
        theme = "primary"
      ),
      value_box(
        title = "Grand Total",
        value = textOutput("total"),
        theme = "primary"
      )
    ),

    layout_sidebar(
      sidebar = sidebar(
        dateRangeInput(
          inputId = "date_range",
          label = "Date Range",
          start = as.Date("2020-01-01"),
          end = Sys.Date(),
          min = as.Date("1993-08-01"),
          max = Sys.Date(),
          width = "100%"
        ),
        selectInput(
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
        tags$div(
          checkboxInput(
            inputId = "columns",
            label = "Columns?",
            value = FALSE
          ),
          checkboxInput(
            inputId = "cumulative",
            label = "Cumulative?",
            value = FALSE
          )
        ),
        tags$div(
          tags$p(
            bs_icon(name = "clipboard-data"),
            "Mean: ",
            tags$span(
              textOutput("mean", inline = TRUE),
              class = "sidepanel-stats"
            )
          ),
          tags$p(
            bs_icon(name = "clipboard-data"),
            "Std. Deviation: ",
            tags$span(
              textOutput("sd", inline = TRUE),
              class = "sidepanel-stats"
            )
          ),
          tags$p(
            bs_icon(name = "clipboard-data"),
            "Median: ",
            tags$span(
              textOutput("median", inline = TRUE),
              class = "sidepanel-stats"
            )
          ),
          id = "sidepanel-stats-div"
        ),
        width = 300
      ),
      navset_card_tab(
        nav_panel(
          title = "Plot",
          # plotOutput("plot")
          plotlyOutput("plot")
        ),
        nav_panel(
          title = "Data",
          DTOutput("data")
        )
      )
    )
  )
)

# Set server ----

server <- function(input, output, session) {
  ## Get and Set Package URI Query -----

  observe({
    query <- parseQueryString(session$clientData$url_search)

    if (!is.null(query[["package"]])) {
      updateTextInput(
        session,
        inputId = "package",
        value = query[["package"]]
      )
    } else {
      updateTextInput(
        session,
        inputId = "package",
        value = "mctq"
      )
    }
  })

  # Update Available Packages -----

  available_packages <-
    available.packages() |>
    as_tibble() |>
    pull(Package)

  # Check `package` Input -----

  is_valid_package <- reactive({
    if (is.null(input$package)) {
      FALSE
    } else {
      input$package %in% available_packages
    }
  }) |>
    bindEvent(input$package)

  iv <- InputValidator$new()

  iv$add_rule("package", sv_required())

  iv$add_rule(
    "package",
    sv_in_set(
      set = available_packages,
      message_fmt = "Package not found"
    )
  )

  iv$enable()

  ## Get Download Data -----

  data <- reactive({
    start_date <- as.Date("2020-01-01")
    end_date <- Sys.Date()

    if (isTRUE(is_valid_package())) {
      cran_logs <- try(
        adj_cran_downloads(
          packages = input$package |> trimws(),
          from = start_date,
          to = end_date
        ),
        silent = TRUE
      )

      if (inherits(cran_logs, "try-error")) {
        cran_logs <-
          cran_downloads(
            packages = input$package |> trimws(),
            from = start_date,
            to = end_date
          ) |>
          as_tibble() |>
          select(date, count) |>
          mutate(cum_sum = cumsum(count))
      } else {
        cran_logs <-
          cran_logs |>
          select(
            date,
            adjusted_downloads,
            adjusted_total_downloads
          ) |>
          rename(
            count = adjusted_downloads,
            cum_sum = adjusted_total_downloads
          )
      }

      cran_logs |>
        select(date, count, cum_sum) |>
        slice(seq(which(.data$count > 0)[1], n())) |>
        summarise(
          across(
            .cols = where(is.numeric),
            .fns = ~ mean(.x, na.rm = TRUE)
          ),
          .by = "date"
        ) |>
        as_tsibble(index = date)
    } else {
      tibble(
        date = as.Date(character()),
        count = numeric(),
        cum_sum = numeric()
      ) |>
        as_tsibble(index = date)
    }
  })

  ## Filter Data by Date Range -----

  filtered_data <- reactive({
    data() |>
      filter(
        between(
          date,
          input$date_range[1] |> as.Date(),
          input$date_range[2] |> as.Date()
        )
      )
  })

  ## Compute Value Box Statistics -----

  output$total <- reactive({
    if (isTRUE(is_valid_package())) {
      filtered_data() |>
        pull(count) |>
        sum(na.rm = TRUE)
    } else {
      as.numeric(NA)
    }
  })

  output$last_month <- reactive({
    if (isTRUE(is_valid_package())) {
      filtered_data() |> sum_interval(get_last_month(Sys.Date()))
    } else {
      as.numeric(NA)
    }
  })

  output$last_week <- reactive({
    if (isTRUE(is_valid_package())) {
      filtered_data() |> sum_interval(get_last_week(Sys.Date()))
    } else {
      as.numeric(NA)
    }
  })

  output$last_day <- reactive({
    if (isTRUE(is_valid_package())) {
      filtered_data() |>
        slice_tail(n = 1) |>
        pull(count) |>
        sum(na.rm = TRUE)
    } else {
      as.numeric(NA)
    }
  })

  ## Compute Sidebar Plot and Statistics -----

  .data <- reactive({
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

      out |> mutate(cum_sum = cumsum(count))
    } else {
      filtered_data()
    }
  })

  output$mean <- reactive({
    mean(.data()$count, na.rm = TRUE) |> round(digits = 2)
  })

  output$sd <- reactive({
    sd(.data()$count, na.rm = TRUE) |> round(digits = 2)
  })

  output$median <- reactive({
    median(.data()$count, na.rm = TRUE) |> round(digits = 2)
  })

  plot <- reactive({
    if (isTRUE(is_valid_package())) {
      if (input$cumulative |> as.logical() |> isTRUE()) {
        out <-
          .data() |>
          ggplot(aes(x = date, y = cum_sum))
      } else {
        out <-
          .data() |>
          ggplot(aes(x = date, y = count))
      }

      if (input$columns |> as.logical() |> isTRUE()) {
        out <-
          out +
          geom_col(fill = color_primary, linewidth = 0)
      } else {
        out <-
          out +
          geom_line(color = color_primary, linewidth = 1)
      }

      if (input$view == "yearly") {
        NULL
      } else if (input$view == "monthly") {
        out <- out + scale_x_yearmonth()
      } else if (input$view == "weekly") {
        out <- out + scale_x_yearweek()
      } else if (input$view == "daily") {
        NULL
      }

      out <- out + labs(x = "Year", y = "Downloads")

      out |> ggplotly()
    } else {
      ggplot() +
        geom_blank() +
        theme_void()
    }
  })

  output$plot <- renderPlotly({
    plot()
  }) # nolint

  output$data <- renderDT(
    {
      .data() |>
        as_tibble() |>
        mutate(date = as.character(date))
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

shinyApp(ui = ui, server = server)
