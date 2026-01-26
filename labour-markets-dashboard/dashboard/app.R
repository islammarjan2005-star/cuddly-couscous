
# app.R -------------------------------------------------------

# Core packages


#renv::restore()

# Core packages
library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DBI)
library(RPostgres)
library(shinyGovstyle)
library(shinyWidgets)
library(rlang)          
library(shiny.router)
library(bs4Dash)
#devtools::install_git("git@gitlab.data.trade.gov.uk:analysis_and_wages/utils/lmutils.git", upgrade = 'never')
#library(lmutils)
#devtools::install_git("git@gitlab.data.trade.gov.uk:ag-data-science/utils/rdwutils.git", upgrade = 'never')
#library(rdwutils)
library(pool)


`%||%` <- function(x, y) if (!is.null(x)) x else y

#Create single environment for whole app 
app_env <- environment()

#Create a DB pool to manage connections to the DB.. 
APP_DB <- new.env(parent = emptyenv())

APP_DB$pool <- pool::dbPool(
  drv      = RPostgres::Postgres(),
  host     = Sys.getenv("PGHOST"),
  port     = Sys.getenv("PGPORT"),
  dbname   = Sys.getenv("PGDATABASE"),
  user     = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD")
)


shiny::onStop(function() {
  pool::poolClose(APP_DB$pool)
})

  
# Source internal helpers (keep these filenames)
source("R/components/ggplot_DIT.r", local =  app_env)          # your plotting theme/utils
source("R/components/govuk_helpers.R", local =  app_env)       # govuk_dependencies(), govuk_apply_rebrand(), govuk_head_assets()
source("R/components/govuk_header.R", local =  app_env)        # govuk_header(), (and optionally govuk_footer())
source("R/components/govuk_top_nav.R", local =  app_env)       # govuk_top_nav() wrapper for tabset-driven nav (non-router for now)
source("R/components/govuk_visuals.R", local =  app_env)
source("R/components/govuk_page.R", local =  app_env)          # govuk_page() wrapper for a blank page template
source("R/components/lfs_scripts.R", local =  app_env) 
# ------------------------------------------------------------
# Inputs
# ------------------------------------------------------------

tabs <- c(Home = "home", Employment = "employment", Unemployment = "unemployment", Vacancies = "vacancies", WIP = "wip")
default_route <- "home"
style <- "DBT"
lfs_tables_full <- get_latest_lfs_table_list(APP_DB$pool)


# Log that it loaded (appears in the R console)
#message("LFS tables loaded. Rows: ", tryCatch(nrow(lfs_tables_full), error = function(e) NA_integer_))


# ------------------------------------------------------------
# Source page modules (one file per page)
# Each page file exports: <route>_ui(id) and <route>_server(id)
# ------------------------------------------------------------
source("R/pages/page_home.R", local =  app_env)
source("R/pages/page_employment.R", local =  app_env)
source("R/pages/page_unemployment.R", local =  app_env)
source("R/pages/page_vacancies.R", local =  app_env)
source("R/pages/page_WIP.R", local =  app_env)

# ------------------------------------------------------------
# 404 page UI
# ------------------------------------------------------------
page_404_ui <- tags$div(
  class = "govuk-width-container",
  tags$main(class = "govuk-main-wrapper",
            tags$h1(class = "govuk-heading-l", "Page not found"),
            tags$p(class = "govuk-body", "Use the navigation to choose a page."),
            tags$a(class = "govuk-link", href = paste0("#/", default_route), "Back to Home"))
)
# ------------------------------------------------------------
# UI
# ------------------------------------------------------------


ui <- fluidPage(
  # Head assets & design system
  govuk_apply_rebrand(),
  govuk_head_assets(),
  govuk_dependencies(style = style, include_ons = TRUE),
  DBT_widget_styling(),
  govuk_toc_assets(),
  ukhsa_card_tabs_assets(),

  # Full-page wrapper
  tags$div(
    class = "govuk-template",

    # Header
    govuk_header(service_name = "Labour Markets Dashboard",
                 style = style, 
                 home_href = if (style == "GOVUK") {"https://www.gov.uk/"} else {"https://data.trade.gov.uk/"}, # Replace later with link back to datawroksapce page for the LM dashboard
                 service_href = "#!/", default_route),

    # Top navigation
    uiOutput("topnav"),

    
    
    # Inline router: same style as the vignette
    router_ui(
      route("/",            home_ui("home")),
      route("home",         home_ui("home")),
      route("employment",   employment_ui("employment")),
      route("unemployment", unemployment_ui("unemployment")),
      route("vacancies",    vacancies_ui("vacancies")),
      route("wip", wip_ui("wip")),
      page_404 = page_404_ui
    ),
    
    
    # Footer (using shinyGovstyle)
    footer(TRUE)

  )
)




# ------------------------------------------------------------
# Server (NO lazy mounting for now)
# ------------------------------------------------------------
server <- function(input, output, session) {
  # Start router — IMPORTANT: no arguments here in this inline pattern
  router_server()
  
  # Watch hash changes and update nav
  observeEvent(session$clientData$url_hash, {
    # Extract route from hash (strip "#/" if present)
    route <- sub("^#!/?", "", session$clientData$url_hash)
    sel <- if (!nzchar(route)) "home" else route
    
    output$topnav <- renderUI({
      govuk_top_nav(tabs, selected = sel, id_prefix = "nav")
    })
  })
  
  
  # Eagerly mount all page servers for now (no lazy load)
  home_server("home")
  employment_server("employment")
  unemployment_server("unemployment")
  vacancies_server("vacancies")
  wip_server("wip")
}



shinyApp(ui, server)


