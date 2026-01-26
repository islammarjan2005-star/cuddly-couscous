home_ui <- function(id) {
  ns <- NS(id)
  govuk_page("Home")
}

home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Home-specific server logic (add later)
  })
}
