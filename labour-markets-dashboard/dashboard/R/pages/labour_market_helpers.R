# =============================================================================
# Labour Market Helpers
# =============================================================================
#
# Shared constants, dataset code mappings, and reusable UI/Server functions
# for labour market visualizations (Employment, Unemployment, etc.)
#
# This file provides the foundation for consistent labour market pages,
# evolved from the original simple pattern to support enhanced features:
#   - Stacked Area, Stacked Bar, Line Graph chart types
#   - Tabbed interface (Chart, Table, Download)
#   - Age group filtering and time period selection
#   - Roxygen documentation
#
# =============================================================================

library(ggplot2)
library(scales)
library(plotly)
library(DBI)

# Source required dependencies (if not already loaded by app.R)
if (!exists("ukhsa_card_tabs_assets")) {
  source("R/components/govuk_helpers.R")
}
if (!exists("govuk_stats_card")) {
  source("R/components/govuk_visuals.R")
}


# -----------------------------------------------------------------------------
# Constants: Age Group Choices (matching database values)
# -----------------------------------------------------------------------------

#' Age group choices for dropdowns and filters
#' @export
AGE_CHOICES <- c("Aged 16 and over", "Aged 16 to 64", "Aged 16 to 17", "Aged 18 to 24",
                 "Aged 25 to 34", "Aged 35 to 49", "Aged 50 to 64", "Aged 65 and over")

#' Age groups for stacked charts (excludes totals to avoid double-counting)
#' @export
AGE_STACK <- c("16-17", "18-24", "25-34", "35-49", "50-64", "65+")

#' Color palette for age groups (GOV.UK Analysis Function colours)
#' @export
AGE_COLOURS <- c(
  "16-17" = "#CF102D",
  "18-24" = "#00285F",
  "25-34" = "#004D44",
  "35-49" = "#4814A0",
  "50-64" = "#0063BE",
  "65+"   = "#E24912"
)


# -----------------------------------------------------------------------------
# Helper: Parse ONS Time Period Strings
# -----------------------------------------------------------------------------

#' Parse ONS time period strings to Date
#'
#' Converts time period strings like "Feb-Apr 1993" to Date objects.
#'
#' @param periods Character vector. Time periods in format "Mon-Mon YYYY".
#' @return Date vector.
#' @export
parse_ons_periods <- function(periods) {
  as.Date(sapply(periods, function(p) {
    parts <- strsplit(p, " ")[[1]]
    as.Date(paste0("01-", sub(".*-", "", parts[1]), "-", parts[2]), format = "%d-%b-%Y")
  }), origin = "1970-01-01")
}


# -----------------------------------------------------------------------------
# Helper: Query Data by Dataset Code
# -----------------------------------------------------------------------------

#' Query labour market data by dataset code
#'
#' @param conn Database connection.
#' @param code Character. The dataset_indentifier_code to query.
#' @param divide Numeric. Divisor for values (e.g., 1000 for thousands).
#' @return Data frame with time_period, value, date columns.
#' @export
query_data <- function(conn, code, divide = 1) {
  q <- sprintf('SELECT time_period, value FROM "ons"."labour_market__age_group"
                WHERE dataset_indentifier_code = \'%s\' ORDER BY time_period', code)
  df <- dbGetQuery(conn, q)
  df$date <- parse_ons_periods(df$time_period)
  df$value <- as.numeric(df$value) / divide
  df[order(df$date), ]
}


# -----------------------------------------------------------------------------
# Helper: Render Stacked Age Chart
# -----------------------------------------------------------------------------

#' Render stacked area chart by age group
#'
#' @param df Data frame with date, value, age_group columns.
#' @param selected_ages Character vector. Selected age groups to display.
#' @param y_label Character. Y-axis label.
#' @return Plotly chart object.
#' @export
render_stacked_age <- function(df, selected_ages, y_label) {
  p <- plot_ly()
  for (age in AGE_STACK[AGE_STACK %in% selected_ages]) {
    d <- df[df$age_group == age, ]
    d <- d[order(d$date), ]
    p <- p %>% add_trace(
      data = d, x = ~date, y = ~value,
      type = 'scatter', mode = 'lines',
      fill = 'tonexty', fillcolor = AGE_COLOURS[age],
      name = age, stackgroup = 'one',
      line = list(color = AGE_COLOURS[age], width = 0.5),
      hovertemplate = paste0(age, ": %{y:.0f}k<br>%{x}<extra></extra>")
    )
  }
  p %>% layout(
    xaxis = list(title = "", fixedrange = TRUE),
    yaxis = list(title = y_label, fixedrange = TRUE),
    hovermode = "x unified",
    legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center")
  ) %>% config(displayModeBar = FALSE)
}


# -----------------------------------------------------------------------------
# Reusable Labour Metric Card UI
# -----------------------------------------------------------------------------

#' Labour Metric Card Module UI
#'
#' Creates a reusable card UI for displaying labour market metrics by age group.
#' Supports chart type toggle (stacked area, stacked bar, line), tabbed interface
#' (Chart, Tabular data, Download), and consistent GOV.UK styling.
#'
#' @param id Character. The module namespace ID.
#' @param metric_name Character. Name of the metric (e.g., "Employment").
#' @param primary_colour Character. Hex colour for primary accent.
#' @param secondary_colour Character. Hex colour for secondary elements.
#'
#' @return A Shiny tagList containing the styled card with chart toggle and tabs.
#' @export
labour_metric_ui <- function(id, metric_name, primary_colour, secondary_colour) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    ukhsa_card_tabs_assets(),

    htmltools::tags$div(class = "lm-card-ukhsa",
      htmltools::tags$h2(class = "govuk-heading-m",
        paste(metric_name, "by Age Group")
      ),
      htmltools::tags$p(class = "govuk-hint",
        paste("View", tolower(metric_name), "data broken down by age group.")
      ),

      # Age group selection for stacked chart
      shiny::checkboxGroupInput(
        inputId = ns("stacked_age_select"),
        label = "Select Age Groups",
        choices = AGE_STACK,
        selected = AGE_STACK,
        inline = TRUE
      ),

      # Chart type toggle
      htmltools::tags$div(class = "govuk-form-group", style = "margin-bottom: 12px;",
        shiny::radioButtons(
          inputId = ns("chart_type"),
          label = "Chart Type",
          choices = c(
            "Stacked Area" = "area",
            "Stacked Bar" = "bar",
            "Line (Total)" = "line"
          ),
          selected = "area",
          inline = TRUE
        )
      ),

      # Tabs container
      htmltools::tags$div(class = "ukhsa-tabs",
        htmltools::tags$div(class = "ukhsa-tabs__list", role = "tablist",
          htmltools::tags$a(
            class = "ukhsa-tabs__tab",
            role = "tab", `aria-selected` = "true", tabindex = "0",
            `data-target` = ns("chart"), "Chart"
          ),
          htmltools::tags$a(
            class = "ukhsa-tabs__tab",
            role = "tab", `aria-selected` = "false", tabindex = "-1",
            `data-target` = ns("table"), "Tabular data"
          ),
          htmltools::tags$a(
            class = "ukhsa-tabs__tab",
            role = "tab", `aria-selected` = "false", tabindex = "-1",
            `data-target` = ns("download"), "Download"
          )
        ),

        # Panels
        htmltools::tags$div(
          id = ns("chart"), class = "ukhsa-tabs__panel", role = "tabpanel",
          plotly::plotlyOutput(ns("metric_chart"), height = "450px")
        ),
        htmltools::tags$div(
          id = ns("table"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          DT::dataTableOutput(ns("metric_table"))
        ),
        htmltools::tags$div(
          id = ns("download"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          htmltools::tags$p(class = "govuk-body", "Download the data in various formats:"),
          shiny::downloadButton(ns("download_csv"), "Download CSV",
            class = "govuk-button govuk-button--secondary"),
          shiny::downloadButton(ns("download_xlsx"), "Download Excel",
            class = "govuk-button govuk-button--secondary")
        )
      )
    )
  )
}


# -----------------------------------------------------------------------------
# Reusable Labour Metric Card Server
# -----------------------------------------------------------------------------

#' Labour Metric Card Module Server
#'
#' Server logic for the labour metric card. Queries data by dataset codes,
#' renders interactive charts (stacked area, stacked bar, or line graph),
#' a data table, and download handlers for CSV/Excel export.
#'
#' @param id Character. The module namespace ID (must match the UI).
#' @param metric_name Character. Name of the metric for labels.
#' @param age_codes Data frame. Mapping of age groups to dataset codes,
#'   with columns: age_group, level_code, rate_code.
#' @param stacked_codes Data frame. Age groups for stacked charts,
#'   with columns: age_group, code.
#' @param primary_colour Character. Hex colour for primary chart elements.
#' @param secondary_colour Character. Hex colour for secondary elements.
#' @param invert Logical. If TRUE, decreases are good (e.g., unemployment).
#'
#' @return NULL (called for side effects - renders chart, table, and downloads).
#' @export
labour_metric_server <- function(
    id,
    metric_name,
    age_codes,
    stacked_codes,
    primary_colour,
    secondary_colour,
    invert = FALSE
) {
  shiny::moduleServer(id, function(input, output, session) {

    # Get database connection from app
    conn <- APP_DB$pool

    # Fetch stacked age data based on selected age groups
    by_age <- reactive({
      req(input$stacked_age_select)
      sel <- stacked_codes[stacked_codes$age_group %in% input$stacked_age_select, ]
      codes <- paste0("'", sel$code, "'", collapse = ", ")

      q <- sprintf('SELECT time_period, dataset_indentifier_code, value
                    FROM "ons"."labour_market__age_group"
                    WHERE dataset_indentifier_code IN (%s)
                    ORDER BY time_period', codes)

      df <- tryCatch({
        dbGetQuery(conn, q)
      }, error = function(e) {
        message("Error fetching age data: ", e$message)
        return(data.frame())
      })

      if (nrow(df) == 0) return(df)

      df$date <- parse_ons_periods(df$time_period)
      df$value <- as.numeric(df$value) / 1000
      df$age_group <- setNames(sel$age_group, sel$code)[df$dataset_indentifier_code]
      df[order(df$date), ]
    })

    # Chart type from radio buttons
    selected_chart_type <- shiny::reactive({
      input$chart_type %||% "area"
    })

    # Render the appropriate chart
    output$metric_chart <- plotly::renderPlotly({
      d <- by_age()
      shiny::req(nrow(d) > 0)

      chart_choice <- selected_chart_type()

      if (chart_choice == "area") {
        # Use the render_stacked_age helper
        render_stacked_age(d, input$stacked_age_select, paste(metric_name, "(000s)"))

      } else if (chart_choice == "bar") {
        # Stacked Bar Chart
        p <- plot_ly()
        for (age in AGE_STACK[AGE_STACK %in% input$stacked_age_select]) {
          age_d <- d[d$age_group == age, ]
          p <- p %>% add_trace(
            data = age_d, x = ~time_period, y = ~value,
            type = 'bar', name = age,
            marker = list(color = AGE_COLOURS[age])
          )
        }
        p %>% layout(
          barmode = 'stack',
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = paste(metric_name, "(000s)")),
          legend = list(orientation = "h", y = -0.2)
        ) %>% config(displayModeBar = FALSE)

      } else if (chart_choice == "line") {
        # Line Graph - Total
        d_total <- d %>%
          dplyr::group_by(time_period, date) %>%
          dplyr::summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(date)

        plot_ly(d_total, x = ~date, y = ~total_value, type = 'scatter', mode = 'lines+markers',
                line = list(color = primary_colour, width = 2),
                marker = list(color = primary_colour, size = 6)) %>%
          layout(
            xaxis = list(title = ""),
            yaxis = list(title = paste("Total", metric_name, "(000s)")),
            hovermode = "x unified"
          ) %>% config(displayModeBar = FALSE)
      }
    })

    # Render data table
    output$metric_table <- DT::renderDataTable({
      d <- by_age()
      shiny::req(nrow(d) > 0)

      DT::datatable(
        d[, c("age_group", "time_period", "value")],
        options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE,
        colnames = c("Age Group", "Time Period", "Value (000s)")
      )
    })

    # Download handlers
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0(tolower(gsub(" ", "_", metric_name)), "_by_age_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(by_age(), file, row.names = FALSE)
      }
    )

    output$download_xlsx <- shiny::downloadHandler(
      filename = function() {
        paste0(tolower(gsub(" ", "_", metric_name)), "_by_age_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(by_age(), file)
        } else {
          write.csv(by_age(), file, row.names = FALSE)
        }
      }
    )

    # Initialize tabs
    session$onFlushed(function() {
      session$sendCustomMessage("ukhsa-tabs-init", list())
    }, once = FALSE)
  })
}
