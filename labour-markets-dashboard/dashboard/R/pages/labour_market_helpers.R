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


# -----------------------------------------------------------------------------
# Constants: Age Group Choices
# -----------------------------------------------------------------------------

#' Age group choices for dropdowns and filters
#' @export
AGE_CHOICES <- c(

"All aged 16 & over",
"Age 16-17",
"Age 18-24",
"Age 25-34",
"Age 35-49",
"Age 50-64",
"Age 65+",
"All aged 16-64"
)

#' Age groups for stacked charts (excludes totals to avoid double-counting)
#' @export
AGE_STACK <- c(
"Age 16-17",
"Age 18-24",
"Age 25-34",
"Age 35-49",
"Age 50-64",
"Age 65+"
)

#' Color palette for age groups (GOV.UK Analysis Function colours)
#' @export
AGE_COLOURS <- c(
"Age 16-17" = "#12436D",
"Age 18-24" = "#28A197",
"Age 25-34" = "#801650",
"Age 35-49" = "#F46A25",
"Age 50-64" = "#3D3D3D",
"Age 65+"
= "#A285D1"
)


# -----------------------------------------------------------------------------
# Helper: Parse Time Period String to Date
# -----------------------------------------------------------------------------
# Note: parse_time_period() is defined in lfs_scripts.R (loaded by app.R)
# If not available, use this fallback:

if (!exists("parse_time_period")) {
  #' Parse ONS time period string to Date
  #'
  #' Converts time period strings like "Feb-Apr 1993" to a Date object.
  #'
  #' @param period Character. Time period string in format "Mon-Mon YYYY".
  #' @return Date object, or NA if parsing fails.
  #' @export
  parse_time_period <- function(period) {
    tryCatch({
      parts <- strsplit(period, " ")[[1]]
      year <- as.numeric(parts[length(parts)])
      month_range <- parts[1]
      first_month <- strsplit(month_range, "-")[[1]][1]
      month_num <- match(first_month, c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
      if (is.na(month_num)) month_num <- 1
      as.Date(paste(year, month_num, "01", sep = "-"))
    }, error = function(e) as.Date(NA))
  }
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
#' This is the evolution of the original simple `labour_metric_ui()` pattern,
#' now with enhanced visualization options.
#'
#' @param id Character. The module namespace ID.
#' @param metric_name Character. Name of the metric (e.g., "Employment").
#' @param primary_colour Character. Hex colour for primary accent.
#' @param secondary_colour Character. Hex colour for secondary elements.
#'
#' @return A Shiny tagList containing the styled card with chart toggle and tabs.
#' @export
#'
#' @examples
#' # Original simple pattern (still works):
#' labour_metric_ui(ns("employment"), "Employment", "#1d70b8", "#00703c")
#'
#' # The UI now includes enhanced features automatically
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

      # Chart type toggle (radio buttons for switching view)
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
        # Tab buttons
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
          plotly::plotlyOutput(ns("metric_chart"), height = "400px")
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
#' Server logic for the labour metric card. Renders interactive charts
#' (stacked area, stacked bar, or line graph), a data table, and download
#' handlers for CSV/Excel export.
#'
#' This is the evolution of the original simple `labour_metric_server()` pattern,
#' now with enhanced visualization options and data handling.
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
#' @param data Reactive. A reactive expression returning filtered data frame.
#'
#' @return NULL (called for side effects - renders chart, table, and downloads).
#' @export
#'
#' @examples
#' # Original simple pattern (evolved):
#' labour_metric_server(
#'   id = "employment",
#'   metric_name = "Employment",
#'   age_codes = employment_age_codes,
#'   stacked_codes = stacked_employment_codes,
#'   primary_colour = "#1d70b8",
#'   secondary_colour = "#00703c",
#'   invert = FALSE,
#'   data = filtered_data
#' )
labour_metric_server <- function(
    id,
    metric_name,
    age_codes,
    stacked_codes,
    primary_colour,
    secondary_colour,
    invert = FALSE,
    data
) {
  shiny::moduleServer(id, function(input, output, session) {

    # Chart type from radio buttons
    selected_chart_type <- shiny::reactive({
      input$chart_type %||% "area"
    })

    # Render the appropriate chart based on selection
    output$metric_chart <- plotly::renderPlotly({
      d <- data()
      shiny::req(nrow(d) > 0)

      chart_choice <- selected_chart_type()

      # Parse time periods for proper ordering
      d$parsed_date <- sapply(d$time_period, parse_time_period)
      d$parsed_date <- as.Date(d$parsed_date, origin = "1970-01-01")
      d <- d[order(d$parsed_date, d$age_group), ]

      if (chart_choice == "area") {
        # Stacked Area Chart
        p <- ggplot2::ggplot(d, ggplot2::aes(x = parsed_date, y = value, fill = age_group)) +
          ggplot2::geom_area(alpha = 0.8, position = "stack") +
          ggplot2::scale_fill_manual(values = AGE_COLOURS, na.value = "#888888") +
          ggplot2::theme_minimal() +
          ggplot2::labs(x = NULL, y = "Value (000s)", fill = "Age Group") +
          ggplot2::theme(legend.position = "bottom")

      } else if (chart_choice == "bar") {
        # Stacked Bar Chart (per period)
        p <- ggplot2::ggplot(d, ggplot2::aes(x = time_period, y = value, fill = age_group)) +
          ggplot2::geom_bar(stat = "identity", position = "stack") +
          ggplot2::scale_fill_manual(values = AGE_COLOURS, na.value = "#888888") +
          ggplot2::theme_minimal() +
          ggplot2::labs(x = "Time Period", y = "Value (000s)", fill = "Age Group") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom"
          )

      } else if (chart_choice == "line") {
        # Line Graph - Total only
        d_total <- d %>%
          dplyr::group_by(time_period, parsed_date) %>%
          dplyr::summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(parsed_date)

        p <- ggplot2::ggplot(d_total, ggplot2::aes(x = parsed_date, y = total_value)) +
          ggplot2::geom_line(color = primary_colour, size = 1.2) +
          ggplot2::geom_point(color = primary_colour, size = 2) +
          ggplot2::theme_minimal() +
          ggplot2::labs(x = NULL, y = "Total Value (000s)")
      }

      plotly::ggplotly(p) %>%
        plotly::layout(legend = list(orientation = "h", y = -0.2))
    })

    # Render data table
    output$metric_table <- DT::renderDataTable({
      d <- data()
      shiny::req(nrow(d) > 0)

      DT::datatable(
        d[, c("age_group", "economic_activity", "time_period", "value")],
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = "frtip"
        ),
        rownames = FALSE,
        colnames = c("Age Group", "Economic Activity", "Time Period", "Value")
      )
    })

    # Download handlers
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0(tolower(gsub(" ", "_", metric_name)), "_by_age_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(data(), file, row.names = FALSE)
      }
    )

    output$download_xlsx <- shiny::downloadHandler(
      filename = function() {
        paste0(tolower(gsub(" ", "_", metric_name)), "_by_age_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(data(), file)
        } else {
          write.csv(data(), file, row.names = FALSE)
        }
      }
    )

    # Initialize tabs
    session$onFlushed(function() {
      session$sendCustomMessage("ukhsa-tabs-init", list())
    }, once = FALSE)
  })
}


# -----------------------------------------------------------------------------
# Data Fetching Helper
# -----------------------------------------------------------------------------
# Note: get_labour_market_age_data() is defined in lfs_scripts.R (loaded by app.R)
# This is documented here for reference but not redefined to avoid duplication.
