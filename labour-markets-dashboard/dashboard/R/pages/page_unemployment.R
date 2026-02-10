unemployment_ui <- function(id) {
  ns <- NS(id)
  govuk_page("Unemployment")
}

unemployment_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Unemployment-specific server logic (add later)
  })
}
