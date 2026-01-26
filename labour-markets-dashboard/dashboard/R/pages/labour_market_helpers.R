# R/pages/labour_market_helpers.R
# =============================================================================
# Labour Market Helpers - evolved from original viq.R pattern
# =============================================================================
#
# Shared constants, dataset code mappings, and reusable UI/Server functions
# for labour market visualizations (Employment, Unemployment, etc.)
#
# Enhanced with: Roxygen docs, chart type toggle, tabs, table, downloads
# =============================================================================

library(ggplot2)
library(scales)
library(plotly)
library(DBI)
library(RPostgres)

# Source required dependencies (if not already loaded by app.R)
if (!exists("ukhsa_card_tabs_assets")) {
  source("R/components/govuk_helpers.R")
}


# -----------------------------------------------------------------------------
# Constants (matching database values exactly)
# -----------------------------------------------------------------------------

#' @export
AGE_CHOICES <- c("Aged 16 and over", "Aged 16 to 64", "Aged 16 to 17", "Aged 18 to 24",
                 "Aged 25 to 34", "Aged 35 to 49", "Aged 50 to 64", "Aged 65 and over")

#' @export
AGE_STACK <- c("16-17", "18-24", "25-34", "35-49", "50-64", "65+")

#' @export
AGE_COLOURS <- c("16-17" = "#CF102D", "18-24" = "#00285F", "25-34" = "#004D44",
                 "35-49" = "#4814A0", "50-64" = "#0063BE", "65+" = "#E24912")


# -----------------------------------------------------------------------------
# Helpers (from viq.R)
# -----------------------------------------------------------------------------

#' @export
parse_ons_periods <- function(periods) {
  as.Date(sapply(periods, function(p) {
    parts <- strsplit(p, " ")[[1]]
    as.Date(paste0("01-", sub(".*-", "", parts[1]), "-", parts[2]), format = "%d-%b-%Y")
  }), origin = "1970-01-01")
}

#' @export
query_data <- function(conn, code, divide = 1) {
  q <- sprintf('SELECT time_period, value FROM "ons"."labour_market__age_group"
                WHERE dataset_indentifier_code = \'%s\' ORDER BY time_period', code)
  df <- dbGetQuery(conn, q)
  df$date <- parse_ons_periods(df$time_period)
  df$value <- as.numeric(df$value) / divide
  df[order(df$date), ]
}

#' @export
render_stacked_age <- function(df, selected_ages, y_label) {
  p <- plot_ly()
  for (age in AGE_STACK[AGE_STACK %in% selected_ages]) {
    d <- df[df$age_group == age, ]
    d <- d[order(d$date), ]
    p <- p %>% add_trace(data = d, x = ~date, y = ~value, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', fillcolor = AGE_COLOURS[age], name = age, stackgroup = 'one',
                         line = list(color = AGE_COLOURS[age], width = 0.5),
                         hovertemplate = paste0(age, ": %{y:.0f}k<br>%{x}<extra></extra>"))
  }
  p %>% layout(xaxis = list(title = "", fixedrange = TRUE),
               yaxis = list(title = y_label, fixedrange = TRUE),
               hovermode = "x unified",
               legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center")) %>%
    config(displayModeBar = FALSE)
}


# -----------------------------------------------------------------------------
# Labour Metric UI (evolved from viq.R with enhancements)
# -----------------------------------------------------------------------------

#' Labour Metric Card Module UI
#'
#' @param id Character. The module namespace ID.
#' @param title Character. Name of the metric (e.g., "Employment").
#' @param level_colour Character. Hex colour for level charts.
#' @param rate_colour Character. Hex colour for rate charts.
#'
#' @return A Shiny tagList containing the card with chart toggle and tabs.
#' @export
labour_metric_ui <- function(id, title, level_colour, rate_colour) {
  ns <- NS(id)

  tagList(
    ukhsa_card_tabs_assets(),

    tags$div(class = "lm-card-ukhsa",
      tags$h2(class = "govuk-heading-m", paste(title, "by Age Group")),
      tags$p(class = "govuk-body", paste("Total", tolower(title), "broken down by age group over time")),

      # Age group checkboxes (from viq.R)
      tags$div(class = "govuk-form-group",
        checkboxGroupInput(ns("stacked_age_select"), "Select Age Groups", AGE_STACK, AGE_STACK, inline = TRUE)
      ),

      # Time period slider (enhancement)
      tags$div(class = "govuk-form-group",
        sliderInput(ns("date_range"), "Time Period",
                    min = as.Date("1992-01-01"), max = Sys.Date(),
                    value = c(as.Date("2010-01-01"), Sys.Date()),
                    width = "100%", timeFormat = "%Y")
      ),

      # Chart type toggle (enhancement)
      tags$div(class = "govuk-form-group",
        radioButtons(ns("chart_type"), "Chart Type",
          choices = c("Stacked Area" = "area", "Stacked Bar" = "bar", "Line (Total)" = "line"),
          selected = "area", inline = TRUE)
      ),

      # Tabs (enhancement)
      tags$div(class = "ukhsa-tabs",
        tags$div(class = "ukhsa-tabs__list", role = "tablist",
          tags$a(class = "ukhsa-tabs__tab", role = "tab", `aria-selected` = "true",
                 tabindex = "0", `data-target` = ns("chart"), "Chart"),
          tags$a(class = "ukhsa-tabs__tab", role = "tab", `aria-selected` = "false",
                 tabindex = "-1", `data-target` = ns("table"), "Tabular data"),
          tags$a(class = "ukhsa-tabs__tab", role = "tab", `aria-selected` = "false",
                 tabindex = "-1", `data-target` = ns("download"), "Download")
        ),

        tags$div(id = ns("chart"), class = "ukhsa-tabs__panel", role = "tabpanel",
          plotlyOutput(ns("stacked_age"), height = "450px")
        ),
        tags$div(id = ns("table"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          DT::dataTableOutput(ns("metric_table"))
        ),
        tags$div(id = ns("download"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          tags$p(class = "govuk-body", "Download the data in various formats:"),
          downloadButton(ns("download_csv"), "Download CSV", class = "govuk-button govuk-button--secondary"),
          downloadButton(ns("download_xlsx"), "Download Excel", class = "govuk-button govuk-button--secondary")
        )
      )
    )
  )
}


# -----------------------------------------------------------------------------
# Labour Metric Server (evolved from viq.R with enhancements)
# -----------------------------------------------------------------------------

#' Labour Metric Card Module Server
#'
#' @param id Character. The module namespace ID.
#' @param title Character. Name of the metric for labels.
#' @param age_codes Data frame. Mapping of age groups to dataset codes.
#' @param stacked_codes Data frame. Age groups for stacked charts.
#' @param level_colour Character. Hex colour for level charts.
#' @param rate_colour Character. Hex colour for rate charts.
#' @param invert Logical. If TRUE, decreases are good.
#'
#' @return NULL (called for side effects).
#' @export
labour_metric_server <- function(id, title, age_codes, stacked_codes, level_colour, rate_colour, invert = FALSE) {
  moduleServer(id, function(input, output, session) {

    # Database connection (exact pattern from viq.R)
    conn <- dbConnect(RPostgres::Postgres())
    onStop(function() dbDisconnect(conn))

    # Fetch all stacked age data (from viq.R)
    all_age_data <- reactive({
      req(input$stacked_age_select)
      sel <- stacked_codes[stacked_codes$age_group %in% input$stacked_age_select, ]
      codes <- paste0("'", sel$code, "'", collapse = ", ")
      df <- dbGetQuery(conn, sprintf('SELECT time_period, dataset_indentifier_code, value FROM "ons"."labour_market__age_group"
                                      WHERE dataset_indentifier_code IN (%s) ORDER BY time_period', codes))
      df$date <- parse_ons_periods(df$time_period)
      df$value <- as.numeric(df$value) / 1000
      df$age_group <- setNames(sel$age_group, sel$code)[df$dataset_indentifier_code]
      df[order(df$date), ]
    })

    # Filter by time period slider (enhancement)
    by_age <- reactive({
      d <- all_age_data()
      req(nrow(d) > 0, input$date_range)
      d[d$date >= input$date_range[1] & d$date <= input$date_range[2], ]
    })

    # Chart type selection
    chart_type <- reactive({ input$chart_type %||% "area" })

    # Render chart (enhanced with multiple types)
    output$stacked_age <- renderPlotly({
      d <- by_age()
      req(nrow(d) > 0)

      if (chart_type() == "area") {
        # Original stacked area from viq.R
        render_stacked_age(d, input$stacked_age_select, paste(title, "(000s)"))

      } else if (chart_type() == "bar") {
        # Stacked bar (enhancement)
        p <- plot_ly()
        for (age in AGE_STACK[AGE_STACK %in% input$stacked_age_select]) {
          age_d <- d[d$age_group == age, ]
          p <- p %>% add_trace(data = age_d, x = ~time_period, y = ~value,
                               type = 'bar', name = age, marker = list(color = AGE_COLOURS[age]))
        }
        p %>% layout(barmode = 'stack', xaxis = list(title = "", tickangle = 45),
                     yaxis = list(title = paste(title, "(000s)")),
                     legend = list(orientation = "h", y = -0.2)) %>%
          config(displayModeBar = FALSE)

      } else if (chart_type() == "line") {
        # Line total (enhancement)
        d_total <- d %>%
          dplyr::group_by(time_period, date) %>%
          dplyr::summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(date)

        plot_ly(d_total, x = ~date, y = ~total_value, type = 'scatter', mode = 'lines+markers',
                line = list(color = level_colour, width = 2),
                marker = list(color = level_colour, size = 6)) %>%
          layout(xaxis = list(title = ""),
                 yaxis = list(title = paste("Total", title, "(000s)")),
                 hovermode = "x unified") %>%
          config(displayModeBar = FALSE)
      }
    })

    # Data table (enhancement)
    output$metric_table <- DT::renderDataTable({
      d <- by_age()
      req(nrow(d) > 0)
      DT::datatable(d[, c("age_group", "time_period", "value")],
        options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE, colnames = c("Age Group", "Time Period", "Value (000s)"))
    })

    # Downloads (enhancement)
    output$download_csv <- downloadHandler(
      filename = function() paste0(tolower(gsub(" ", "_", title)), "_by_age_", Sys.Date(), ".csv"),
      content = function(file) write.csv(by_age(), file, row.names = FALSE)
    )

    output$download_xlsx <- downloadHandler(
      filename = function() paste0(tolower(gsub(" ", "_", title)), "_by_age_", Sys.Date(), ".xlsx"),
      content = function(file) {
        if (requireNamespace("writexl", quietly = TRUE)) writexl::write_xlsx(by_age(), file)
        else write.csv(by_age(), file, row.names = FALSE)
      }
    )

    # Initialize tabs
    session$onFlushed(function() {
      session$sendCustomMessage("ukhsa-tabs-init", list())
    }, once = FALSE)
  })
}
