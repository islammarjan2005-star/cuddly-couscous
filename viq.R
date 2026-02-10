# R/pages/labour_market_helpers.R
library(ggplot2)
library(scales)
library(plotly)
library(DBI)
library(RPostgres)

# constants
AGE_CHOICES <- c("Aged 16 and over", "Aged 16 to 64", "Aged 16 to 17", "Aged 18 to 24", 
                 "Aged 25 to 34", "Aged 35 to 49", "Aged 50 to 64", "Aged 65 and over")
AGE_STACK <- c("16-17", "18-24", "25-34", "35-49", "50-64", "65+")
AGE_COLOURS <- c("16-17"="#CF102D", "18-24"="#00285F", "25-34"="#004D44", 
                 "35-49"="#4814A0", "50-64"="#0063BE", "65+"="#E24912")

# helpers
parse_ons_periods <- function(periods) {
  as.Date(sapply(periods, function(p) {
    parts <- strsplit(p, " ")[[1]]
    as.Date(paste0("01-", sub(".*-", "", parts[1]), "-", parts[2]), format = "%d-%b-%Y")
  }), origin = "1970-01-01")
}

query_data <- function(conn, code, divide = 1) {
  q <- sprintf('SELECT time_period, value FROM "ons"."labour_market__age_group" 
                WHERE dataset_indentifier_code = \'%s\' ORDER BY time_period', code)
  df <- dbGetQuery(conn, q)
  df$date <- parse_ons_periods(df$time_period)
  df$value <- as.numeric(df$value) / divide
  df[order(df$date), ]
}

render_stat <- function(df, date_select, is_rate = FALSE, invert = FALSE) {
  idx <- which.min(abs(df$date - as.Date(paste0(format(date_select, "%Y-%m"), "-01"))))
  curr <- df$value[idx]; prev <- df$value[max(1, idx - 3)]; change <- curr - prev
  col <- if ((change >= 0) != invert) "green" else "red"
  val <- if (is_rate) paste0(round(curr, 1), "%") else comma(round(curr, 0))
  chg <- if (is_rate) paste0(round(change, 1), "pp") else round(change, 0)
  HTML(paste0("<p class='govuk-body-s' style='margin-bottom:5px;'>", df$time_period[idx], "</p>",
              "<h2 class='govuk-heading-l'>", val, "</h2>",
              "<strong class='govuk-tag govuk-tag--", col, "'>", 
              ifelse(change >= 0, "+", ""), chg, " vs previous quarter</strong>"))
}

render_trend <- function(df, colour, y_label, is_rate = FALSE) {
  latest <- tail(df, 1)
  label <- if (is_rate) paste0(round(latest$value, 1), "%") else comma(round(latest$value, 0))
  y_fmt <- if (is_rate) function(x) paste0(x, "%") else comma
  p <- ggplot(df, aes(date, value)) + geom_area(fill = colour, alpha = 0.15) +
    geom_line(colour = colour, linewidth = 0.8) + geom_point(data = latest, colour = colour, size = 3) +
    annotate("text", x = latest$date, y = latest$value, label = label,
             vjust = -1.5, size = 3.5, fontface = "bold", colour = colour) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(labels = y_fmt, expand = expansion(mult = c(0, 0.1))) +
    theme_DIT() + theme(panel.grid.major.y = element_line(colour = "#f0f0f0", linewidth = 0.5),
                        axis.line.x = element_line(colour = "#b1b4b6", linewidth = 0.5)) + labs(x = NULL, y = NULL)
  ggplotly(p, tooltip = c("x", "y")) %>% config(displayModeBar = FALSE) %>%
    layout(hovermode = "x unified", yaxis = list(title = y_label, fixedrange = TRUE), xaxis = list(fixedrange = TRUE))
}

render_stacked_age <- function(df, selected_ages, y_label) {
  p <- plot_ly()
  for (age in AGE_STACK[AGE_STACK %in% selected_ages]) {
    d <- df[df$age_group == age, ]; d <- d[order(d$date), ]
    p <- p %>% add_trace(data = d, x = ~date, y = ~value, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', fillcolor = AGE_COLOURS[age], name = age, stackgroup = 'one',
                         line = list(color = AGE_COLOURS[age], width = 0.5),
                         hovertemplate = paste0(age, ": %{y:.0f}k<br>%{x}<extra></extra>"))
  }
  p %>% layout(xaxis = list(title = "", fixedrange = TRUE), yaxis = list(title = y_label, fixedrange = TRUE),
               hovermode = "x unified", legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center")) %>%
    config(displayModeBar = FALSE)
}

#  UI
labour_metric_ui <- function(id, title, level_colour, rate_colour) {
  ns <- NS(id)
  card_style <- function(col) paste0("padding:15px;background:#f3f2f1;border-top:4px solid ", col, ";")
  div(class = "govuk-width-container",
    tags$main(class = "govuk-main-wrapper",
      tags$span(class = "govuk-caption-xl", "Labour Market"),
      tags$h1(class = "govuk-heading-xl", title),
      tags$p(class = "govuk-body-s", paste("Last updated:", Sys.Date())),
      div(class = "govuk-grid-row",
          div(class = "govuk-grid-column-one-third", selectInput(ns("age_filter_level"), "Age Group (Level)", AGE_CHOICES, "Aged 16 and over", width = "100%")),
          div(class = "govuk-grid-column-one-third", selectInput(ns("age_filter_rate"), "Age Group (Rate)", AGE_CHOICES, "Aged 16 to 64", width = "100%")),
          div(class = "govuk-grid-column-one-third", dateInput(ns("date_select"), "Select Month", Sys.Date(), format = "MM yyyy", startview = "year", width = "100%"))),
      div(class = "govuk-grid-row",
          div(class = "govuk-grid-column-one-half", div(class = "govuk-summary-card", style = card_style(level_colour),
              h3(class = "govuk-heading-s", paste(title, "Level (000s)")), uiOutput(ns("stat_level")))),
          div(class = "govuk-grid-column-one-half", div(class = "govuk-summary-card", style = card_style(rate_colour),
              h3(class = "govuk-heading-s", paste(title, "Rate (%)")), uiOutput(ns("stat_rate"))))),
      h2(class = "govuk-heading-m", paste(title, "Level over time")), plotlyOutput(ns("trend_level"), height = "350px"),
      h2(class = "govuk-heading-m", paste(title, "Rate over time")), plotlyOutput(ns("trend_rate"), height = "350px"),
      tags$hr(class = "govuk-section-break govuk-section-break--xl govuk-section-break--visible"),
      h2(class = "govuk-heading-m", paste(title, "by Age Group")),
      tags$p(class = "govuk-body", paste("Total", tolower(title), "broken down by age group over time")),
      checkboxGroupInput(ns("stacked_age_select"), "Select Age Groups", AGE_STACK, AGE_STACK, inline = TRUE),
      plotlyOutput(ns("stacked_age"), height = "450px")))
}

# server
labour_metric_server <- function(id, title, age_codes, stacked_codes, level_colour, rate_colour, invert = FALSE) {
  moduleServer(id, function(input, output, session) {
    conn <- dbConnect(RPostgres::Postgres()); onStop(function() dbDisconnect(conn))
    level_data <- reactive(query_data(conn, age_codes$level_code[age_codes$age_group == input$age_filter_level], 1000))
    rate_data <- reactive(query_data(conn, age_codes$rate_code[age_codes$age_group == input$age_filter_rate], 1))
    output$stat_level <- renderUI(render_stat(level_data(), input$date_select, invert = invert))
    output$stat_rate  <- renderUI(render_stat(rate_data(), input$date_select, is_rate = TRUE, invert = invert))
    output$trend_level <- renderPlotly(render_trend(level_data(), level_colour, paste(title, "(000s)")))
    output$trend_rate  <- renderPlotly(render_trend(rate_data(), rate_colour, paste(title, "Rate (%)"), is_rate = TRUE))
    by_age <- reactive({
      req(input$stacked_age_select)
      sel <- stacked_codes[stacked_codes$age_group %in% input$stacked_age_select, ]
      codes <- paste0("'", sel$code, "'", collapse = ", ")
      df <- dbGetQuery(conn, sprintf('SELECT time_period, dataset_indentifier_code, value FROM "ons"."labour_market__age_group" 
                                      WHERE dataset_indentifier_code IN (%s) ORDER BY time_period', codes))
      df$date <- parse_ons_periods(df$time_period); df$value <- as.numeric(df$value) / 1000
      df$age_group <- setNames(sel$age_group, sel$code)[df$dataset_indentifier_code]; df[order(df$date), ]
    })
    output$stacked_age <- renderPlotly(render_stacked_age(by_age(), input$stacked_age_select, paste(title, "(000s)")))
  })
}