vacancies_ui <- function(id) {
  ns <- NS(id)
  govuk_page("Vacancies")
}

vacancies_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Vacancies-specific server logic (add later)
  })
}
