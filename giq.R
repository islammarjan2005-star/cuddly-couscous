# R/pages/page_employment.R

source("R/pages/labour_market_helpers.R")

employment_age_codes <- data.frame(
  age_group = AGE_CHOICES,
  level_code = c("MGRZ", "LF2G", "YBTO", "YBTR", "YBTU", "YBTX", "LF26", "LFK4"),
  rate_code  = c("MGSR", "LF24", "YBUA", "YBUD", "YBUG", "YBUJ", "LF2U", "LFK6"),
  stringsAsFactors = FALSE
)

stacked_employment_codes <- data.frame(
  age_group = AGE_STACK,
  code = c("YBTO", "YBTR", "YBTU", "YBTX", "LF26", "LFK4"),
  stringsAsFactors = FALSE
)

#UI & Server
employment_ui <- function(id) {
  labour_metric_ui(id, "Employment", "#1d70b8", "#00703c")
}

employment_server <- function(id) {
  labour_metric_server(id, "Employment", employment_age_codes, stacked_employment_codes, 
                       "#1d70b8", "#00703c", invert = FALSE)
}