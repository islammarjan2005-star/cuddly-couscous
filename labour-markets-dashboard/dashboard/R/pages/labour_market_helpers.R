# labour_market_helpers.R

library(ggplot2)
library(scales)
library(plotly)
library(DBI)
library(RPostgres)

# load dependencies
if (!exists("ukhsa_card_tabs_assets")) {
  source("R/components/govuk_helpers.R")
}


# constants
#' @export
AGE_CHOICES <- c("Aged 16 and over", "Aged 16 to 64", "Aged 16 to 17", "Aged 18 to 24",
                 "Aged 25 to 34", "Aged 35 to 49", "Aged 50 to 64", "Aged 65 and over")

#' @export
AGE_STACK <- c("16-17", "18-24", "25-34", "35-49", "50-64", "65+")

#' @export
AGE_COLOURS <- c("16-17" = "#CF102D", "18-24" = "#00285F", "25-34" = "#004D44",
                 "35-49" = "#4814A0", "50-64" = "#0063BE", "65+" = "#E24912")


# data helpers

#' Parse ONS time periods to Date objects
#' @param periods Character vector of ONS period strings (e.g., "Jan-Mar 2020")
#' @return Vector of Date objects
#' @export
parse_ons_periods <- function(periods) {
  as.Date(sapply(periods, function(p) {
    parts <- strsplit(p, " ")[[1]]
    as.Date(paste0("01-", sub(".*-", "", parts[1]), "-", parts[2]), format = "%d-%b-%Y")
  }), origin = "1970-01-01")
}

#' Query single dataset code from database
#' @param conn Database connection
#' @param code ONS dataset identifier code
#' @param divide Divisor for values (default 1)
#' @return Data frame with time_period, value, date columns
#' @export
query_data <- function(conn, code, divide = 1) {
  q <- sprintf('SELECT time_period, value FROM "ons"."labour_market__age_group"
                WHERE dataset_indentifier_code = \'%s\' ORDER BY time_period', code)
  df <- dbGetQuery(conn, q)
  df$date <- parse_ons_periods(df$time_period)
  df$value <- as.numeric(df$value) / divide
  df[order(df$date), ]
}


# chart helpers

#' Render stacked area chart
#' @keywords internal
render_area_chart <- function(d, selected_ages, x_config, y_config) {
  p <- plot_ly()
  for (age in AGE_STACK[AGE_STACK %in% selected_ages]) {
    age_d <- d[d$age_group == age, ]
    age_d <- age_d[order(age_d$date), ]
    p <- p %>% add_trace(
      data = age_d, x = ~date, y = ~value,
      type = 'scatter', mode = 'lines',
      fill = 'tonexty', fillcolor = AGE_COLOURS[age],
      name = age, stackgroup = 'one',
      line = list(color = AGE_COLOURS[age], width = 0.5),
      hovertemplate = paste0(age, ": %{y:.0f}k<br>%{x}<extra></extra>")
    )
  }
  p %>%
    layout(xaxis = x_config, yaxis = y_config,
           hovermode = "x unified",
           legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center")) %>%
    config(displayModeBar = FALSE)
}

#' Render stacked bar chart
#' @keywords internal
render_bar_chart <- function(d, selected_ages, x_config, y_config) {
  p <- plot_ly()
  for (age in AGE_STACK[AGE_STACK %in% selected_ages]) {
    age_d <- d[d$age_group == age, ]
    p <- p %>% add_trace(
      data = age_d, x = ~time_period, y = ~value,
      type = 'bar', name = age,
      marker = list(color = AGE_COLOURS[age])
    )
  }
  bar_x_config <- x_config
  bar_x_config$tickangle <- 45
  p %>%
    layout(barmode = 'stack', xaxis = bar_x_config, yaxis = y_config,
           legend = list(orientation = "h", y = -0.2)) %>%
    config(displayModeBar = FALSE)
}

#' Render line chart (total)
#' @keywords internal
render_line_chart <- function(d, x_config, y_config, line_colour) {
  d_total <- d %>%
    dplyr::group_by(time_period, date) %>%
    dplyr::summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(date)

  plot_ly(d_total, x = ~date, y = ~total_value,
          type = 'scatter', mode = 'lines+markers',
          line = list(color = line_colour, width = 2),
          marker = list(color = line_colour, size = 6)) %>%
    layout(xaxis = x_config, yaxis = y_config, hovermode = "x unified") %>%
    config(displayModeBar = FALSE)
}


# module ui

#' Labour Metric Card Module UI
#'
#' @param id Character. The module namespace ID.
#' @param title Character. Name of the metric (e.g., "Employment").
#' @param subtitle Character. Optional custom description. Defaults to auto-generated.
#' @param level_colour Character. Hex colour for level charts.
#' @param rate_colour Character. Hex colour for rate charts.
#'
#' @return A Shiny tagList containing the card with chart toggle and tabs.
#' @export
labour_metric_ui <- function(id, title, subtitle = NULL, level_colour, rate_colour) {
  ns <- NS(id)

  tagList(
    ukhsa_card_tabs_assets(),

    tags$h2(class = "govuk-heading-m", paste(title, "by Age Group")),
    tags$p(class = "govuk-body", subtitle %||% paste("Total", tolower(title), "broken down by age group over time")),

    # time period filter
    tags$fieldset(class = "govuk-fieldset", style = "border: 1px solid #b1b4b6; padding: 15px; margin-bottom: 20px;",
      tags$legend(class = "govuk-fieldset__legend govuk-fieldset__legend--s",
        tags$span(class = "govuk-fieldset__heading", "Time Period")
      ),
      sliderInput(ns("date_range"), NULL,
                  min = as.Date("1992-01-01"), max = Sys.Date(),
                  value = c(as.Date("2010-01-01"), Sys.Date()),
                  width = "100%", timeFormat = "%Y")
    ),

    # age group filter
    tags$fieldset(class = "govuk-fieldset", style = "border: 1px solid #b1b4b6; padding: 15px; margin-bottom: 20px;",
      tags$legend(class = "govuk-fieldset__legend govuk-fieldset__legend--s",
        tags$span(class = "govuk-fieldset__heading", "Select Age Groups")
      ),
      checkboxGroupInput(ns("stacked_age_select"), NULL, AGE_STACK, AGE_STACK, inline = TRUE)
    ),

    # chart card
    tags$div(class = "lm-card-ukhsa",
      tags$div(class = "ukhsa-tabs",
        tags$div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          tags$div(class = "ukhsa-tabs__list", role = "tablist", style = "margin-bottom: 0;",
            tags$a(class = "ukhsa-tabs__tab", role = "tab", `aria-selected` = "true",
                   tabindex = "0", `data-target` = ns("chart"), "Chart"),
            tags$a(class = "ukhsa-tabs__tab", role = "tab", `aria-selected` = "false",
                   tabindex = "-1", `data-target` = ns("table"), "Tabular data"),
            tags$a(class = "ukhsa-tabs__tab", role = "tab", `aria-selected` = "false",
                   tabindex = "-1", `data-target` = ns("download"), "Download")
          ),
          tags$div(class = "govuk-body-s", style = "margin: 0; display: flex; align-items: center; gap: 10px;",
            tags$span(style = "font-weight: 600;", "View:"),
            radioButtons(ns("chart_type"), NULL,
              choices = c("Area" = "area", "Bar" = "bar", "Line" = "line"),
              selected = "area", inline = TRUE)
          )
        ),

        # tab panels
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


# module server

#' Labour Metric Card Module Server
#'
#' @param id Character. The module namespace ID.
#' @param title Character. Name of the metric for labels.
#' @param age_codes Data frame. Mapping of age groups to dataset codes.
#' @param stacked_codes Data frame. Age groups for stacked charts.
#' @param level_colour Character. Hex colour for level charts.
#' @param rate_colour Character. Hex colour for rate charts.
#' @param y_label Character. Optional custom y-axis label. Defaults to "Title (000s)".
#' @param x_label Character. Optional custom x-axis label. Defaults to "".
#' @param y_min Numeric. Optional minimum y-axis value. NULL for auto.
#' @param y_max Numeric. Optional maximum y-axis value. NULL for auto.
#' @param invert Logical. If TRUE, decreases are good.
#'
#' @return NULL (called for side effects).
#' @export
labour_metric_server <- function(id, title, age_codes, stacked_codes, level_colour, rate_colour,
                                  y_label = NULL, x_label = "", y_min = NULL, y_max = NULL, invert = FALSE) {
  moduleServer(id, function(input, output, session) {

    # db connection
    conn <- dbConnect(RPostgres::Postgres())
    onStop(function() dbDisconnect(conn))

    # fetch data
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

    # filter by date
    by_age <- reactive({
      d <- all_age_data()
      req(nrow(d) > 0, input$date_range)
      d[d$date >= input$date_range[1] & d$date <= input$date_range[2], ]
    })

    # chart type
    chart_type <- reactive({ input$chart_type %||% "area" })

    # axis config
    y_axis_label <- y_label %||% paste(title, "(000s)")
    y_axis_config <- list(title = y_axis_label, fixedrange = TRUE)
    if (!is.null(y_min)) y_axis_config$range <- c(y_min, y_max %||% NA)
    if (!is.null(y_max) && is.null(y_min)) y_axis_config$range <- c(NA, y_max)
    x_axis_config <- list(title = x_label, fixedrange = TRUE)

    # render chart
    output$stacked_age <- renderPlotly({
      d <- by_age()
      req(nrow(d) > 0)

      switch(chart_type(),
        "area" = render_area_chart(d, input$stacked_age_select, x_axis_config, y_axis_config),
        "bar"  = render_bar_chart(d, input$stacked_age_select, x_axis_config, y_axis_config),
        "line" = {
          line_y_config <- y_axis_config
          line_y_config$title <- y_label %||% paste("Total", title, "(000s)")
          render_line_chart(d, x_axis_config, line_y_config, level_colour)
        }
      )
    })

    # data table
    output$metric_table <- DT::renderDataTable({
      d <- by_age()
      req(nrow(d) > 0)
      DT::datatable(d[, c("age_group", "time_period", "value")],
        options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE, colnames = c("Age Group", "Time Period", "Value (000s)"))
    })

    # csv download
    output$download_csv <- downloadHandler(
      filename = function() paste0(tolower(gsub(" ", "_", title)), "_by_age_", Sys.Date(), ".csv"),
      content = function(file) write.csv(by_age(), file, row.names = FALSE)
    )

    # excel download
    output$download_xlsx <- downloadHandler(
      filename = function() paste0(tolower(gsub(" ", "_", title)), "_by_age_", Sys.Date(), ".xlsx"),
      content = function(file) {
        if (requireNamespace("writexl", quietly = TRUE)) writexl::write_xlsx(by_age(), file)
        else write.csv(by_age(), file, row.names = FALSE)
      }
    )

    # init tabs
    session$onFlushed(function() {
      session$sendCustomMessage("ukhsa-tabs-init", list())
    }, once = FALSE)
  })
}
