
# R/pages/page_employment.R


# ---- WIP page (FOr trying out new components and elements before rolling out on main dahsboard pages ----

library(ggplot2)
library(scales)
library(plotly)


wip_ui <- function(id) {
  ns <- NS(id)
  
  # Sidebar TOC items (labels -> target section IDs)
  # Adjust labels if you want different wording in the left nav.
  toc_items <- list(
    "Overview"             = "section-overview",
    "Section A"    = "section-a",
    "Section B" = "section-b",
    "Section C" = "section-c"
  )
  
  toc_sections <- list( 
    list(
      heading = "Main Group A", 
      items = c("Overview"             = "section-overview",
                "Section A"    = "section-a")
    ),
    list(
      heading = "Main Group B", 
      items = c("Section B" = "section-b",
                "Section C" = "section-c")
    )
    )
  
  
  tagList(
    
    # Sidebar nav in left gutter (fixed via CSS) , show_title_visually = TRUE, nav_id = "section-menu"
    side_nav(ns, sections = toc_sections, title = "On this page"), 

    div(class = "govuk-width-container",
        tags$main(class = "govuk-main-wrapper",
                  tags$span(class = "govuk-caption-xl", "Dev Page"),
                  tags$h1(class = "govuk-heading-xl", "WIP Components"),
                  tags$p(class = "govuk-body-s", paste("Last updated:", Sys.Date())),
                  
                  # --- Grid: left TOC + right content ---
                  div(class = "govuk-grid-row",

                  div(class = "govuk-grid-column-full",
                          
                          # ===== Overview =====
                          tags$section(id = "section-overview",
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
                                       
                                      mod_govuk_data_vis_card_ui(
                                          id = ns("trend_card"),
                                          title = "Employment trend",
                                          help_text = "This card hosts the visual only. Global and visual-specific filters live elsewhere.",
                                          visual_content = plotlyOutput(ns("trend"), height = "350px"),
                                        
                                          # NEW: controls list (just add your slider here)
                                          controls = list(
                                            sliderInput(
                                              ns("range"), "Year Range",
                                              min(economics$date), max(economics$date),
                                              value = c(as.Date("2010-01-01"), max(economics$date)),
                                              width = "100%"
                                            )
                                          )
                                        )

                          ),
                          
                          # ===== Employment by Age =====
                          tags$section(id = "section-a",
                                       tags$h1(class = "govuk-heading-xl", "Section A"),
                                       textAreaInput("notes", label = NULL, value = "", placeholder = strrep("This is a very long placeholder. ", 200))
                                      
                          ),
                          
                          # ===== Employment by Gender =====
                          tags$section(id = "section-b",
                                       tags$h1(class = "govuk-heading-xl", "Section B"),
                                       textAreaInput("notes", label = NULL, value = "", placeholder = strrep("This is a very long placeholder. ", 200))
                          ),
                          
                          # ===== Employment by Sector =====
                          tags$section(id = "section-c",
                                       tags$h1(class = "govuk-heading-xl", "Section C"),
                                       textAreaInput("notes", label = NULL, value = "", placeholder = strrep("This is a very long placeholder. ", 200))
                          )
                      )
                  )
        )
    )
  )
  }
    



wip_server <- function(id) {
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
  })
}
