
# R/pages/page_employment.R
#
# Employment page - evolved from original simple pattern to enhanced visualization
# Uses shared helpers from labour_market_helpers.R for consistency and reusability

# Source shared helpers (AGE_CHOICES, AGE_STACK, labour_metric_ui/server, etc.)
source("R/pages/labour_market_helpers.R")

library(ggplot2)
library(scales)
library(plotly)


# -----------------------------------------------------------------------------
# Dataset Code Mappings (from original employment script)
# -----------------------------------------------------------------------------
# These map age groups to their ONS dataset identifier codes for querying

#' Employment age group codes for levels and rates
#' @description Maps each age group to its ONS dataset codes
employment_age_codes <- data.frame(
  age_group = AGE_CHOICES,
  level_code = c("MGRZ", "LF2G", "YBTO", "YBTR", "YBTU", "YBTX", "LF26", "LFK4"),
  rate_code  = c("MGSR", "LF24", "YBUA", "YBUD", "YBUG", "YBUJ", "LF2U", "LFK6"),
  stringsAsFactors = FALSE
)

#' Employment codes for stacked charts (excludes totals)
stacked_employment_codes <- data.frame(
  age_group = AGE_STACK,
  code = c("YBTO", "YBTR", "YBTU", "YBTX", "LF26", "LFK4"),
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# Employment Page UI
# -----------------------------------------------------------------------------

#' Employment Page UI
#'
#' Creates the full Employment page with multiple sections:
#' Overview, Employment by Age, Employment by Gender, Employment by Sector.
#'
#' @param id Character. The module namespace ID.
#' @return A Shiny tagList containing the complete page UI.
#' @export
employment_ui <- function(id) {
  ns <- NS(id)

  # Sidebar TOC sections
  toc_sections <- list(
    list(
      heading = "Live Full Sample",
      items = c("Overview"           = "employment-overview",
                "Employment by Age"  = "employment-age")
    ),
    list(
      heading = "Full Sample Microdata",
      items = c("Employment by Gender" = "employment-gender",
                "Employment by Sector" = "employment-sector")
    )
  )

  tagList(
    # Sidebar nav in left gutter
    side_nav(ns, sections = toc_sections, title = "On this page"),

    div(class = "govuk-width-container",
        tags$main(class = "govuk-main-wrapper",
                  tags$span(class = "govuk-caption-xl", "Labour Market"),
                  tags$h1(class = "govuk-heading-xl", "Employment"),
                  tags$p(class = "govuk-body-s", paste("Last updated:", Sys.Date())),

                  # --- Grid: content area ---
                  div(class = "govuk-grid-row",
                  div(class = "govuk-grid-column-full",

                          # ===== Overview Section =====
                          tags$section(id = "employment-overview",
                                       div(class = "govuk-grid-row",
                                           uiOutput(ns("card_unemploy")),
                                           uiOutput(ns("card_duration")),
                                           uiOutput(ns("card_pop"))
                                       ),

                                       h2(class = "govuk-heading-m", "Trends over time"),

                                       sliderInput(ns("range"), "Year Range",
                                                   min(economics$date), max(economics$date),
                                                   value = c(as.Date("2010-01-01"), max(economics$date)),
                                                   width = "100%"),

                                       shinyWidgets::sliderTextInput(
                                         inputId = ns("lfs_date_range"),
                                         label   = "Select LFS Data Range",
                                         choices = rev(lfs_tables_full$yearquarter),
                                         selected = c("2020 Q1", lfs_tables_full$yearquarter[1]),
                                         grid = TRUE,
                                         width = "100%"
                                       ),

                                       uiOutput(ns("date_slider")),
                                       textOutput(ns("selection")),

                                       selectizeInput(
                                         inputId = ns("table_select"),
                                         label   = "Pick table from range to display:",
                                         choices = NULL,
                                         options = list(placeholder = "Type to search..."),
                                         width   = "100%"
                                       ),

                                       verbatimTextOutput(ns("picked_table")),
                                       textOutput(ns("lfs_list")),

                                       # Trend visualization card
                                       mod_govuk_data_vis_card_ui(
                                         id = ns("trend_card"),
                                         title = "Employment trend",
                                         help_text = "This card hosts the visual only. Global and visual-specific filters live elsewhere.",
                                         visual_content = plotlyOutput(ns("trend"), height = "350px")
                                       )
                          ),

                          # ===== Employment by Age Section =====
                          # Uses shared labour_metric_ui from helpers (evolved pattern)
                          tags$section(id = "employment-age",
                                       tags$h1(class = "govuk-heading-xl", "Employment by Age"),

                                       # Age group filter
                                       div(class = "govuk-grid-row",
                                           div(class = "govuk-grid-column-one-half",
                                               selectizeInput(
                                                 inputId = ns("age_group_filter"),
                                                 label = "Filter by Age Group",
                                                 choices = NULL,
                                                 multiple = TRUE,
                                                 options = list(
                                                   placeholder = "Select age groups (leave empty for all)...",
                                                   plugins = list("remove_button")
                                                 ),
                                                 width = "100%"
                                               )
                                           ),
                                           div(class = "govuk-grid-column-one-half",
                                               selectInput(
                                                 inputId = ns("age_economic_activity"),
                                                 label = "Economic Activity",
                                                 choices = c("Employment", "Unemployment"),
                                                 selected = "Employment",
                                                 width = "100%"
                                               )
                                           )
                                       ),

                                       # Time period range slider (dynamic)
                                       uiOutput(ns("age_time_slider")),

                                       # Employment by Age card using shared helper
                                       # This is the evolved pattern from the original:
                                       #   labour_metric_ui(id, "Employment", "#1d70b8", "#00703c")
                                       labour_metric_ui(
                                         id = ns("age_card"),
                                         metric_name = "Employment",
                                         primary_colour = "#1d70b8",
                                         secondary_colour = "#00703c"
                                       )
                          ),

                          # ===== Employment by Gender Section =====
                          tags$section(id = "employment-gender",
                                       tags$h1(class = "govuk-heading-xl", "Employment by Gender"),
                                       textAreaInput(ns("notes_gender"), label = NULL, value = "",
                                         placeholder = strrep("This is a very long placeholder. ", 200))
                          ),

                          # ===== Employment by Sector Section =====
                          tags$section(id = "employment-sector",
                                       tags$h1(class = "govuk-heading-xl", "Employment by Sector"),
                                       textAreaInput(ns("notes_sector"), label = NULL, value = "",
                                         placeholder = strrep("This is a very long placeholder. ", 200))
                          )
                      )
                  )
        )
    )
  )
}


# -----------------------------------------------------------------------------
# Employment Page Server
# -----------------------------------------------------------------------------

#' Employment Page Server
#'
#' Server logic for the Employment page. Handles data fetching, filtering,
#' and wiring up the various visualization modules.
#'
#' @param id Character. The module namespace ID.
#' @export
employment_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Initialize trend card
    mod_govuk_data_vis_card_server("trend_card")

    # Data for overview section (economics sample data)
    dat <- reactive({
      economics[economics$date >= input$range[1] & economics$date <= input$range[2], ]
    })

    output$selection <- renderText({
      paste("Selected range:",
            input$lfs_date_range[1], "to",
            input$lfs_date_range[2])
    })

    lfs_selected_tables <- reactive({
      get_lfs_table_from_range(input$lfs_date_range[1], input$lfs_date_range[2])
    })

    output$lfs_list <- renderText({
      paste("Selected LFS tables: ",
            paste(lfs_selected_tables(), collapse = ", "))
    })

    observeEvent(lfs_selected_tables(), {
      choices <- lfs_selected_tables()
      req(length(choices) > 0)

      current <- isolate(input$table_select)
      selected <- if (!is.null(current) && current %in% choices) current else choices[1]

      updateSelectizeInput(
        session  = session,
        inputId  = "table_select",
        choices  = choices,
        selected = selected,
        server   = TRUE
      )
    }, ignoreInit = FALSE)

    output$picked_table <- renderPrint({
      input$table_select
    })


    # --- Overview Stats Cards ---

    output$card_unemploy <- renderUI({
      d <- dat(); req(nrow(d) >= 2)
      curr <- tail(d$unemploy, 1); prev <- tail(d$unemploy, 2)[1]
      delta <- curr - prev

      govuk_stats_card(
        id       = session$ns("unemploy"),
        title    = "Total Unemployed",
        headline = govuk_format_number(curr),
        delta    = govuk_format_number(delta),
        period   = "vs last month",
        good_if_increase = FALSE
      )
    })

    output$card_duration <- renderUI({
      d <- dat(); req(nrow(d) >= 2)
      curr <- tail(d$uempmed, 1); prev <- tail(d$uempmed, 2)[1]
      delta <- curr - prev

      govuk_stats_card(
        id       = session$ns("duration"),
        title    = "Duration (Weeks)",
        headline = govuk_format_number(curr),
        delta    = scales::comma(round(delta, 1)),
        period   = "vs last month",
        good_if_increase = FALSE
      )
    })

    output$card_pop <- renderUI({
      d <- dat(); req(nrow(d) >= 2)
      curr <- tail(d$pop, 1); prev <- tail(d$pop, 2)[1]
      delta <- curr - prev

      govuk_stats_card(
        id       = session$ns("population"),
        title    = "Population",
        headline = govuk_format_number(curr),
        delta    = scales::comma(round(delta, 1)),
        period   = "vs last month",
        good_if_increase = TRUE
      )
    })

    # Trend plot
    output$trend <- renderPlotly({
      ggplotly(
        ggplot(dat(), aes(date, unemploy)) +
          geom_area(fill = "#cf102d", alpha = 0.2) +
          geom_line(col = "#cf102d", size = 1) +
          theme_minimal() +
          labs(x = NULL, y = "Unemployed (000s)")
      )
    })


    # =========================================================================
    # Employment by Age Section
    # Uses the evolved labour_metric_server pattern from helpers
    # =========================================================================

    # Fetch all age data
    all_age_data <- reactive({
      tryCatch({
        get_labour_market_age_data(
          conn = APP_DB$pool,
          economic_activity = input$age_economic_activity,
          value_type = "level"
        )
      }, error = function(e) {
        message("Error loading age data: ", e$message)
        data.frame()
      })
    })

    # Get unique time periods for the slider
    age_time_periods <- reactive({
      d <- all_age_data()
      if (nrow(d) == 0) return(character(0))
      unique(d$time_period)
    })

    # Populate age group filter choices
    observe({
      d <- all_age_data()
      if (nrow(d) > 0) {
        age_groups <- sort(unique(d$age_group))
        updateSelectizeInput(
          session = session,
          inputId = "age_group_filter",
          choices = age_groups,
          selected = character(0),
          server = TRUE
        )
      }
    })

    # Render time period slider dynamically
    output$age_time_slider <- renderUI({
      periods <- age_time_periods()
      req(length(periods) > 0)

      # Parse and sort periods chronologically
      period_df <- data.frame(
        period = periods,
        date = sapply(periods, parse_time_period),
        stringsAsFactors = FALSE
      )
      period_df$date <- as.Date(period_df$date, origin = "1970-01-01")
      period_df <- period_df[order(period_df$date), ]
      sorted_periods <- period_df$period

      # Default to last 20 periods or all if fewer
      n_periods <- length(sorted_periods)
      start_idx <- max(1, n_periods - 19)

      shinyWidgets::sliderTextInput(
        inputId = session$ns("age_time_range"),
        label = "Select Time Period Range",
        choices = sorted_periods,
        selected = c(sorted_periods[start_idx], sorted_periods[n_periods]),
        grid = TRUE,
        width = "100%"
      )
    })

    # Filtered age data based on user selections
    filtered_age_data <- reactive({
      d <- all_age_data()
      req(nrow(d) > 0)

      # Filter by selected age groups (if any selected)
      if (!is.null(input$age_group_filter) && length(input$age_group_filter) > 0) {
        d <- d[d$age_group %in% input$age_group_filter, ]
      }

      # Filter by time period range (if slider exists)
      if (!is.null(input$age_time_range) && length(input$age_time_range) == 2) {
        all_periods <- age_time_periods()

        period_df <- data.frame(
          period = all_periods,
          date = sapply(all_periods, parse_time_period),
          stringsAsFactors = FALSE
        )
        period_df$date <- as.Date(period_df$date, origin = "1970-01-01")
        period_df <- period_df[order(period_df$date), ]

        start_period <- input$age_time_range[1]
        end_period <- input$age_time_range[2]
        start_idx <- which(period_df$period == start_period)
        end_idx <- which(period_df$period == end_period)

        if (length(start_idx) > 0 && length(end_idx) > 0) {
          selected_periods <- period_df$period[start_idx:end_idx]
          d <- d[d$time_period %in% selected_periods, ]
        }
      }

      d
    })

    # Call the shared labour_metric_server (evolved from original pattern)
    # Original was: labour_metric_server(id, "Employment", employment_age_codes,
    #                                    stacked_employment_codes, "#1d70b8", "#00703c", invert = FALSE)
    labour_metric_server(
      id = "age_card",
      metric_name = "Employment",
      age_codes = employment_age_codes,
      stacked_codes = stacked_employment_codes,
      primary_colour = "#1d70b8",
      secondary_colour = "#00703c",
      invert = FALSE,
      data = filtered_age_data
    )

  })
}
