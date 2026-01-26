# app.R -------------------------------------------------------

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
library(rlang)          
library(shiny.router)
#devtools::install_git("git@gitlab.data.trade.gov.uk:analysis_and_wages/utils/lmutils.git", upgrade = 'never')
#library(lmutils)
# install rdwutils
#devtools::install_git("git@gitlab.data.trade.gov.uk:ag-data-science/utils/rdwutils.git", upgrade = 'never')
#library(rdwutils) 

#Returns a table that lists out all of the latest LFS quarterly data, parses out the correpsonding year and quarter of the data
get_latest_lfs_table_list <- function(conn){
  
lfs_tables_full <- DBI::dbGetQuery(conn, "
SELECT DISTINCT TABLE_NAME,
  SUBSTRING(TABLE_NAME FROM '__([0-9]{4})_q') AS Year,
  SUBSTRING(TABLE_NAME FROM 'q([0-9])$') AS Quarter, 
  SUBSTRING(TABLE_NAME FROM '__([0-9]{4})_q') || ' Q' || SUBSTRING(TABLE_NAME FROM 'q([0-9])$') AS YearQuarter
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_NAME LIKE 'labour_force_survey%'
  AND TABLE_NAME NOT LIKE '%_label'
ORDER BY TABLE_NAME DESC
")

return(lfs_tables_full)
}

#Using input on selected quarters from slider, retrieves list of LFS table names for use with existing lmutils functions
get_lfs_table_from_range <- function(q1, q2){
  start_idx <- match(q1, lfs_tables_full$yearquarter)
  end_idx <- match(q2, lfs_tables_full$yearquarter)

  filtered_df <- lfs_tables_full[start_idx:end_idx, ]
  table_names <- filtered_df$table_name

  return(table_names)
}


#' Fetch labour market age group data from ONS table
#' @param conn Database connection pool
#' @param economic_activity Filter by economic activity (e.g., "Employment", "Unemployment")
#' @param value_type Filter by value type (e.g., "level", "rate (%)")
#' @return Data frame with age_group, economic_activity, value_type, time_period, value
get_labour_market_age_data <- function(conn, economic_activity = NULL, value_type = "level") {

  # Build query with optional filters
  query <- '
    SELECT age_group, economic_activity, value_type,
           dataset_identifier_code, time_period, value
    FROM ons.labour_market__age_group
    WHERE 1=1
  '

  params <- list()

  if (!is.null(economic_activity) && nzchar(economic_activity)) {
    query <- paste0(query, " AND economic_activity = $1")
    params <- c(params, economic_activity)
  }

  if (!is.null(value_type) && nzchar(value_type)) {
    param_num <- length(params) + 1
    query <- paste0(query, " AND value_type = $", param_num)
    params <- c(params, value_type)
  }

  query <- paste0(query, " ORDER BY time_period, age_group")

  tryCatch({
    if (length(params) > 0) {
      result <- DBI::dbGetQuery(conn, query, params = params)
    } else {
      result <- DBI::dbGetQuery(conn, query)
    }
    return(result)
  }, error = function(e) {
    message("Error fetching labour market age data: ", e$message)
    return(data.frame())
  })
}


#' Get unique age groups from the labour market data
#' @param conn Database connection pool
#' @return Character vector of age groups
get_age_groups <- function(conn) {
  query <- 'SELECT DISTINCT age_group FROM ons.labour_market__age_group ORDER BY age_group'
  tryCatch({
    result <- DBI::dbGetQuery(conn, query)
    return(result$age_group)
  }, error = function(e) {
    message("Error fetching age groups: ", e$message)
    return(character(0))
  })
}


#' Get unique time periods from the labour market data
#' @param conn Database connection pool
#' @return Character vector of time periods
get_time_periods <- function(conn) {
  query <- 'SELECT DISTINCT time_period FROM ons.labour_market__age_group ORDER BY time_period'
  tryCatch({
    result <- DBI::dbGetQuery(conn, query)
    return(result$time_period)
  }, error = function(e) {
    message("Error fetching time periods: ", e$message)
    return(character(0))
  })
}


#' Parse time period string to Date for sorting/filtering
#' @param period Character string like "Feb-Apr 1993", "Mar-May 1993"
#' @return Date object (first day of the first month)
parse_time_period <- function(period) {
  # Extract the year and first month
  tryCatch({
    # Pattern: "Mon-Mon YYYY" or similar
    parts <- strsplit(period, " ")[[1]]
    year <- as.numeric(parts[length(parts)])
    month_range <- parts[1]
    first_month <- strsplit(month_range, "-")[[1]][1]

    # Convert month abbreviation to number
    month_num <- match(first_month, c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    if (is.na(month_num)) month_num <- 1

    as.Date(paste(year, month_num, "01", sep = "-"))
  }, error = function(e) {
    as.Date(NA)
  })
}



