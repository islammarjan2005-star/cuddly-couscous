# Type all of you required packages in the same format as below
packages_required <- c('shiny', 'shinydashboard', 'plotly', 'ggplot2', 'shinyWidgets', 'plotly')
# List all the installed packages in DW
already_installed_packages <- rownames(installed.packages())
# List all of the missing packages
missing <- setdiff(packages_required, already_installed_packages)
# Install packages which are not in your local environment and load them
if (length(missing) > 0) {
  install.packages(missing)
}
# Load all required packages
invisible(lapply(packages_required, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}))
ui <- navbarPage(
  'Shiny DBT Dashboard',
 header = tagList( shinyWidgets::useShinydashboard() ),
includeCSS('www/dbt_custom.css'),
  tabPanel('Overview',
    fluidRow(valueBoxOutput('value1'), valueBoxOutput('value2'), valueBoxOutput('value3')),
    selectInput('lineVar', 'Select Variable:', choices = c('y', 'y2')),
    plotlyOutput('linePlot1')),  tabPanel('Unemployment',
    radioButtons('barType', 'Choose Chart Type:', choices = c('Bar', 'Line')),
    plotlyOutput('barPlot1')),  tabPanel('Employment',
    selectInput('scatterVar', 'Select Variable:', choices = c('x', 'y')),
    plotlyOutput('scatterPlot')),  tabPanel('Jobs Market',
    h2('Settings Section'),
    p('This section can be pre-populated with dashboard information.'))
)
server <- function(input, output, session) {
popup_modal <- showModal(modalDialog(
title = tags$a('Welcome to your new dashboard', style = 'color: #CF102D; font-weight: bold', icon('robot')),
easyClose = TRUE,
HTML('<b> This is your new text for the modal. If you have any questions, contact you </b>'),
footer = tagList(modalButton('Close'))))
  output$value1 <- renderValueBox({ valueBox(100, 'Value 1', color = 'red', icon('stats', lib = 'glyphicon')) })
  output$value2 <- renderValueBox({ valueBox(200, 'Value 2', color = 'red', icon('tasks', lib = 'glyphicon')) })
  output$value3 <- renderValueBox({ valueBox(300, 'Value 3', color = 'red', icon('plane', lib = 'glyphicon')) })
  # Line plot with improved styling
  output$linePlot1 <- renderPlotly({
    data <- data.frame(x = 1:10, y = rnorm(10), y2 = runif(10))
    p <- ggplot(data, aes(x = x, y = data[[input$lineVar]])) +
         geom_line(color = '#00285f', size = 1.2) +
         theme_minimal() +
         labs(title = 'Line Plot', x = 'X Axis', y = 'Y Axis') +
         theme(plot.title = element_text(size = 16))
    ggplotly(p)
  })
  #  bar plot with consistent styling
  output$barPlot1 <- renderPlotly({
    data <- data.frame(x = factor(1:5), y = runif(5))
    p <- if (input$barType == 'Bar') {
      ggplot(data, aes(x = x, y = y, fill = x)) +
      geom_bar(stat = 'identity') +
      scale_fill_manual(values = c('#00285f', '#a3abcc', '#777d96', '#d9ddea', '#d6dae1')) +
      theme_minimal() +
      labs(title = ' Line Plot', x = 'X Axis', y = 'Y Axis')
    } else {
      ggplot(data, aes(x = x, y = y, group = 1)) +
      geom_line(color = '#12436D', size = 1.2) +
      theme_minimal() +
      labs(title = ' Line Plot', x = 'X Axis', y = 'Y Axis')
    }
    ggplotly(p)
  })
  # Improved scatter plot with consistent styling
  output$scatterPlot <- renderPlotly({
    data <- data.frame(x = rnorm(100), y = rnorm(100))
    p <- ggplot(data, aes(x = data[[input$scatterVar]], y = y)) +
         geom_point(color = '#7b005b', alpha = 0.7, size = 2) +
         theme_minimal() +
         labs(title = ' Line Plot', x = 'X Axis', y = 'Y Axis')
    ggplotly(p)
  })
}
shinyApp(ui, server)
