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



