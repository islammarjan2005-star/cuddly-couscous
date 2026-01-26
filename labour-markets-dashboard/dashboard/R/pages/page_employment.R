
# R/pages/page_employment.R


# ---- Employment page ----

library(ggplot2)
library(scales)
library(plotly)


employment_ui <- function(id) {
  ns <- NS(id)

  # Sidebar TOC items (labels -> target section IDs)
  # Adjust labels if you want different wording in the left nav.
  toc_items <- list(
    "Overview"             = "employment-overview",
    "Employment by Age"    = "employment-age",
    "Employment by Gender" = "employment-gender",
    "Employment by Sector" = "employment-sector"
  )

  toc_sections <- list(
    list(
      heading = "Live Full Sample",
      items = c("Overview"             = "employment-overview",
                "Employment by Age"    = "employment-age")
    ),
    list(
      heading = "Full Sample Microdata",
      items = c("Employment by Gender" = "employment-gender",
                "Employment by Sector" = "employment-sector")
    )
    )


  tagList(

    # Sidebar nav in left gutter (fixed via CSS) , show_title_visually = TRUE, nav_id = "section-menu"
    side_nav(ns, sections = toc_sections, title = "On this page"),

    div(class = "govuk-width-container",
        tags$main(class = "govuk-main-wrapper",
                  tags$span(class = "govuk-caption-xl", "Labour Market"),
                  tags$h1(class = "govuk-heading-xl", "Employment"),
                  tags$p(class = "govuk-body-s", paste("Last updated:", Sys.Date())),

                  # --- Grid: left TOC + right content ---
                  div(class = "govuk-grid-row",

                  div(class = "govuk-grid-column-full",

                          # ===== Overview =====
                          tags$section(id = "employment-overview",
                                       # Keep your existing components inside this section
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

                                       #Visualize the Plotly Output
                                        mod_govuk_data_vis_card_ui(
                                          id = ns("trend_card"),
                                          title = "Employment trend",
                                          help_text = "This card hosts the visual only. Global and visual-specific filters live elsewhere.",
                                          visual_content = plotlyOutput(ns("trend"), height = "350px"),
                                        )

                          ),

                          # ===== Employment by Age =====
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

                                       # Time period range slider for age data
                                       uiOutput(ns("age_time_slider")),

                                       # Employment by Age visualization card with multiple tabs
                                       mod_employment_age_card_ui(
                                         id = ns("age_card"),
                                         title = "Employment by Age Group",
                                         help_text = "View employment data broken down by age group. Use tabs to switch between chart types."
                                       )
                          ),

                          # ===== Employment by Gender =====
                          tags$section(id = "employment-gender",
                                       tags$h1(class = "govuk-heading-xl", "Employment by Gender"),
                                       textAreaInput(ns("notes_gender"), label = NULL, value = "", placeholder = strrep("This is a very long placeholder. ", 200))
                          ),

                          # ===== Employment by Sector =====
                          tags$section(id = "employment-sector",
                                       tags$h1(class = "govuk-heading-xl", "Employment by Sector"),
                                       textAreaInput(ns("notes_sector"), label = NULL, value = "", placeholder = strrep("This is a very long placeholder. ", 200))
                          )
                      )
                  )
        )
    )
  )
  }
    



# -------------------------------------------------------------------------
# Employment by Age Card Module (UI + Server)
# Provides: Stacked Area, Stacked Bar, Line Graph (Total), Table, Download
# -------------------------------------------------------------------------

mod_employment_age_card_ui <- function(id, title, help_text = NULL) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    ukhsa_card_tabs_assets(),

    htmltools::tags$div(class = "lm-card-ukhsa",
      htmltools::tags$h2(class = "govuk-heading-m", title),
      if (!is.null(help_text)) htmltools::tags$p(class = "govuk-hint", help_text),

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
          plotly::plotlyOutput(ns("age_chart"), height = "400px")
        ),
        htmltools::tags$div(
          id = ns("table"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          DT::dataTableOutput(ns("age_table"))
        ),
        htmltools::tags$div(
          id = ns("download"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          htmltools::tags$p(class = "govuk-body", "Download the data in various formats:
"),
          shiny::downloadButton(ns("download_csv"), "Download CSV", class = "govuk-button govuk-button--secondary"),
          shiny::downloadButton(ns("download_xlsx"), "Download Excel", class = "govuk-button govuk-button--secondary")
        )
      )
    )
  )
}


mod_employment_age_card_server <- function(id, age_data, chart_type) {
  shiny::moduleServer(id, function(input, output, session) {

    # Merge chart_type from parent or use local
    selected_chart_type <- shiny::reactive({
      if (!is.null(chart_type) && shiny::is.reactive(chart_type)) {
        chart_type()
      } else {
        input$chart_type %||% "area"
      }
    })

    # Render the appropriate chart based on selection

    output$age_chart <- plotly::renderPlotly({
      d <- age_data()
      shiny::req(nrow(d) > 0)

      chart_choice <- selected_chart_type()

      # Parse time periods for proper ordering
      d$parsed_date <- sapply(d$time_period, parse_time_period)
      d$parsed_date <- as.Date(d$parsed_date, origin = "1970-01-01
")
      d <- d[order(d$parsed_date, d$age_group), ]

      # Color palette for age groups
      age_colors <- c(
        "Age 16-17" = "#12436D",
        "Age 18-24" = "#28A197",
        "Age 25-34" = "#801650",
        "Age 35-49" = "#F46A25",
        "Age 50-64" = "#3D3D3D",
        "Age 65+"   = "#A285D1"
      )

      if (chart_choice == "area") {
        # Stacked Area Chart
        p <- ggplot2::ggplot(d, ggplot2::aes(x = parsed_date, y = value, fill = age_group)) +
          ggplot2::geom_area(alpha = 0.8, position = "stack") +
          ggplot2::scale_fill_manual(values = age_colors, na.value = "#888888") +
          ggplot2::theme_minimal() +
          ggplot2::labs(x = NULL, y = "Value (000s)", fill = "Age Group") +
          ggplot2::theme(legend.position = "bottom")

      } else if (chart_choice == "bar") {
        # Stacked Bar Chart (per period)
        p <- ggplot2::ggplot(d, ggplot2::aes(x = time_period, y = value, fill = age_group)) +
          ggplot2::geom_bar(stat = "identity", position = "stack") +
          ggplot2::scale_fill_manual(values = age_colors, na.value = "#888888") +
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
          ggplot2::geom_line(color = "#cf102d", size = 1.2) +
          ggplot2::geom_point(color = "#cf102d", size = 2) +
          ggplot2::theme_minimal() +
          ggplot2::labs(x = NULL, y = "Total Value (000s)")
      }

      plotly::ggplotly(p) %>%
        plotly::layout(legend = list(orientation = "h", y = -0.2))
    })

    # Render data table
    output$age_table <- DT::renderDataTable({
      d <- age_data()
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
        paste0("employment_by_age_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(age_data(), file, row.names = FALSE)
      }
    )

    output$download_xlsx <- shiny::downloadHandler(
      filename = function() {
        paste0("employment_by_age_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        # Use writexl if available, otherwise fall back to CSV
        if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(age_data(), file)
        } else {
          write.csv(age_data(), file, row.names = FALSE)
        }
      }
    )

    # Initialize tabs
    session$onFlushed(function() {
      session$sendCustomMessage("ukhsa-tabs-init", list())
    }, once = FALSE)
  })
}


# -------------------------------------------------------------------------
# Main Employment Server
# -------------------------------------------------------------------------

employment_server <- function(id) {
  moduleServer(id, function(input, output, session) {


    # somewhere in your employment route server
    mod_govuk_data_vis_card_server("trend_card")


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
            paste(lfs_selected_tables(), collapse = ", ")
            )
    })

    observeEvent(lfs_selected_tables(), {
      choices <- lfs_selected_tables()
      req(length(choices) > 0)

      # keep current selection if still present, else pick first
      current <- isolate(input$table_select)
      selected <- if (!is.null(current) && current %in% choices) current else choices[1]

      updateSelectizeInput(
        session  = session,
        inputId  = "table_select",   # <- no ns() here; already in module session
        choices  = choices,
        selected = selected,
        server   = TRUE
      )
    }, ignoreInit = FALSE)

    output$picked_table <- renderPrint({
      input$table_select
    })




    # Card 1: Total Unemployed (increase is bad -> red tag; blue accent hex)
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
        good_if_increase = FALSE       # increase is bad => red tag
      )
    })

    # Card 2: Duration (Weeks) (increase is bad; red accent hex)
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

    # Card 3: Population (increase is good; green accent hex)
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

    # Plot (unchanged)
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
    # =========================================================================

    # Fetch all age data once
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

        # Parse and sort all periods
        period_df <- data.frame(
          period = all_periods,
          date = sapply(all_periods, parse_time_period),
          stringsAsFactors = FALSE
        )
        period_df$date <- as.Date(period_df$date, origin = "1970-01-01")
        period_df <- period_df[order(period_df$date), ]

        # Find index range
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

    # Get the chart type selection from the child module's input
    age_chart_type <- reactive({
      # Access the child module's input via namespaced ID
      ns_id <- session$ns("age_card-chart_type")
      input[[ns_id]] %||% "area"
    })

    # Call the Employment by Age card server
    mod_employment_age_card_server(
      id = "age_card",
      age_data = filtered_age_data,
      chart_type = reactive({ input[["age_card-chart_type"]] %||% "area" })
    )

  })
}
