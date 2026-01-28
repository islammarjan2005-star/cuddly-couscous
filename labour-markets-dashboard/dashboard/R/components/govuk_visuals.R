# =============================================================================
# GOV.UK Visual Components for Labour Markets Dashboard
# =============================================================================
#
# This file contains reusable UI components and Shiny modules for displaying
# statistics and data visualizations in GOV.UK Design System style.
#
# Components:
#   - govuk_format_number(), govuk_format_percent1() - Formatters
#   - govuk_stats_card() - Simple stats card (UI only)
#   - mod_stats_card_ui/server() - Reusable stats card module (UI + Server)
#   - mod_govuk_data_vis_card_ui/server() - Data visualization card with tabs
#
# =============================================================================


# -----------------------------------------------------------------------------
# Text Formatters
# -----------------------------------------------------------------------------

#' Format a number with comma separators
#'
#' @param x Numeric value to format
#' @return Character string with comma-separated number (e.g., "12,345")
#' @export
#' @examples
#' govuk_format_number(12345)
#' # Returns: "12,345"
govuk_format_number <- function(x) scales::comma(x)


#' Format a number as percentage with one decimal place
#'
#' @param x Numeric value to format (e.g., 12.3 for 12.3%)
#' @return Character string with percentage (e.g., "12.3%")
#' @export
#' @examples
#' govuk_format_percent1(12.345)
#' # Returns: "12.3%"
govuk_format_percent1 <- function(x) sprintf("%.1f%%", x)


# -----------------------------------------------------------------------------
# Internal Helpers
# -----------------------------------------------------------------------------

#' Determine tag colour based on delta sign and direction preference
#'
#' @param delta Numeric or character value representing change
#' @param good_if_increase Logical; if TRUE, positive change = green, else red
#' @return Character: "green", "red", or "blue"
#' @keywords internal
.govuk_tag_colour <- function(delta, good_if_increase = TRUE) {
  if (is.null(delta)) return("blue")  # neutral when no delta
  # Determine sign robustly for numeric or "+/-" strings
  sign <- if (is.numeric(delta)) {
    if (delta > 0) 1L else if (delta < 0) -1L else 0L
  } else if (is.character(delta)) {
    if (grepl("^\\s*\\+", delta)) 1L else if (grepl("^\\s*-", delta)) -1L else 0L
  } else 0L

  if (sign == 0) "blue" else if (good_if_increase) if (sign > 0) "green" else "red" else if (sign > 0) "red" else "green"
}


# -----------------------------------------------------------------------------
# Simple Stats Card (UI Only)
# -----------------------------------------------------------------------------

#' Create a GOV.UK styled statistics card
#'
#' Renders a summary card displaying a headline metric with optional
#' change indicator (delta). The delta tag is colour-coded based on

#' whether increases are considered good or bad.
#'
#' @param id Character. Unique identifier for the card element.
#' @param title Character. Card header/title text.
#' @param headline Character or numeric. The main value to display prominently.
#' @param delta Character or numeric. Optional change value shown in a tag.
#' @param period Character. Label describing the comparison period (default: "vs last month").
#' @param accent_hex Character. Hex colour for the top border accent (default: DBT red "#cf102d").
#' @param good_if_increase Logical. If TRUE, positive delta shows green tag; if FALSE, shows red.
#' @param tag_colour Character. Optional override for tag colour: "green", "red", or "blue".
#' @param width_class Character. GOV.UK grid column class (default: "govuk-grid-column-one-third").
#' @param classes Character. Additional CSS classes for the card.
#' @param format_headline Function. Optional formatter for numeric headline values.
#' @param format_delta Function. Optional formatter for numeric delta values.
#'
#' @return An htmltools tag object representing the stats card.
#' @export
#'
#' @examples
#' # Basic usage
#' govuk_stats_card(
#'   id = "unemployment_card",
#'   title = "Total Unemployed",
#'   headline = "8,526",
#'   delta = "+22",
#'   good_if_increase = FALSE
#' )
#'
#' # With numeric values and formatters
#' govuk_stats_card(
#'   id = "pop_card",
#'   title = "Population",
#'   headline = 320402,
#'   delta = 172,
#'   format_headline = govuk_format_number,
#'   format_delta = govuk_format_number,
#'   good_if_increase = TRUE
#' )
govuk_stats_card <- function(
    id,
    title,
    headline,
    delta = NULL,
    period = "vs last month",
    accent_hex = "#cf102d",
    good_if_increase = TRUE,
    tag_colour = NULL,
    width_class = "govuk-grid-column-one-third",
    classes = NULL,
    format_headline = NULL,
    format_delta = NULL
) {
  # Prepare outputs; allow numeric or pre-formatted character inputs
  headline_out <- if (is.numeric(headline) && !is.null(format_headline)) format_headline(headline) else headline

  delta_out <- NULL
  if (!is.null(delta)) {
    delta_out <- if (is.numeric(delta) && !is.null(format_delta)) format_delta(delta) else delta
  }

  # Decide tag colour
  tag_col <- if (!is.null(tag_colour)) tag_colour else .govuk_tag_colour(delta, good_if_increase)
  # Ensure known values (fallback to blue)
  if (!tag_col %in% c("green","red","blue")) tag_col <- "blue"

  htmltools::tags$div(
    class = width_class,
    htmltools::tags$div(
      id    = id,
      class = paste("govuk-summary-card", if (!is.null(classes)) classes else ""),
      style = paste0("padding:15px; background:#f3f2f1; border-top:4px solid ", accent_hex, ";"),
      htmltools::tags$h3(class = "govuk-heading-s", title),
      htmltools::tags$h2(class = "govuk-heading-l", headline_out),
      if (!is.null(delta_out)) htmltools::tags$strong(
        class = paste0("govuk-tag govuk-tag--", tag_col),
        paste(delta_out, period)
      )
    )
  )
}


# -----------------------------------------------------------------------------
# Reusable Stats Card Module (UI + Server)
# -----------------------------------------------------------------------------

#' Stats Card Module UI
#'
#' Creates a placeholder for a reactive stats card that updates based on data.
#' Use with \code{mod_stats_card_server()} to populate with reactive data.
#'
#' @param id Character. The module namespace ID.
#'
#' @return A Shiny UI element (uiOutput placeholder).
#' @export
#'
#' @examples
#' # In UI definition
#' mod_stats_card_ui(ns("employment_stats"))
mod_stats_card_ui <- function(id) {

  ns <- shiny::NS(id)
  shiny::uiOutput(ns("card"))
}


#' Stats Card Module Server
#'
#' Server logic for a reactive stats card. Computes current value, previous value,
#' and delta from the provided data, then renders a GOV.UK styled stats card.
#'
#' @param id Character. The module namespace ID (must match the UI).
#' @param data Reactive. A reactive expression returning a data frame.
#' @param value_col Character. Column name containing the metric values.
#' @param title Character. Card title/header text.
#' @param period Character. Comparison period label (default: "vs last period").
#' @param good_if_increase Logical. If TRUE, increases are good (green); else bad (red).
#' @param accent_hex Character. Hex colour for top border accent.
#' @param format_fn Function. Formatter for displaying values (default: govuk_format_number).
#' @param width_class Character. GOV.UK grid column class.
#'
#' @return NULL (called for side effects - renders the card).
#' @export
#'
#' @examples
#' # In server definition
#' mod_stats_card_server(
#'   id = "employment_stats",
#'   data = reactive({ employment_data }),
#'   value_col = "employment_count",
#'   title = "Total Employed",
#'   good_if_increase = TRUE
#' )
mod_stats_card_server <- function(
    id,
    data,
    value_col,
    title,
    period = "vs last period",
    good_if_increase = TRUE,
    accent_hex = "#cf102d",
    format_fn = govuk_format_number,
    width_class = "govuk-grid-column-one-third"
) {
  shiny::moduleServer(id, function(input, output, session) {

    output$card <- shiny::renderUI({
      d <- data()
      shiny::req(nrow(d) >= 2)

      # Get current and previous values
      values <- d[[value_col]]
      curr <- utils::tail(values, 1)
      prev <- utils::tail(values, 2)[1]
      delta <- curr - prev

      govuk_stats_card(
        id               = session$ns("stats_card"),
        title            = title,
        headline         = format_fn(curr),
        delta            = format_fn(delta),
        period           = period,
        accent_hex       = accent_hex,
        good_if_increase = good_if_increase,
        width_class      = width_class
      )
    })
  })
}


# -----------------------------------------------------------------------------
# Stats Card Row Module (Multiple Cards)
# -----------------------------------------------------------------------------

#' Stats Card Row Module UI
#'
#' Creates a row of multiple stats cards. Use with \code{mod_stats_card_row_server()}
#' to populate multiple cards from the same data source.
#'
#' @param id Character. The module namespace ID.
#' @param card_ids Character vector. IDs for each card in the row.
#'
#' @return A GOV.UK grid row containing card placeholders.
#' @export
#'
#' @examples
#' # Create a row with 3 stats cards
#' mod_stats_card_row_ui(
#'   ns("overview_cards"),
#'   card_ids = c("unemployed", "duration", "population")
#' )
mod_stats_card_row_ui <- function(id, card_ids) {
  ns <- shiny::NS(id)

  htmltools::tags$div(
    class = "govuk-grid-row",
    lapply(card_ids, function(card_id) {
      shiny::uiOutput(ns(card_id))
    })
  )
}


#' Stats Card Row Module Server
#'
#' Server logic for rendering multiple stats cards in a row from the same data source.
#' Each card configuration specifies which column to use and display options.
#'
#' @param id Character. The module namespace ID (must match the UI).
#' @param data Reactive. A reactive expression returning a data frame.
#' @param card_configs List of lists. Each inner list must contain:
#'   \itemize{
#'     \item \code{id}: Character. Card identifier (must match card_ids in UI).
#'     \item \code{value_col}: Character. Column name for the metric.
#'     \item \code{title}: Character. Card title.
#'     \item \code{good_if_increase}: Logical. Direction preference.
#'   }
#'   Optional: \code{period}, \code{accent_hex}, \code{format_fn}, \code{width_class}.
#'
#' @return NULL (called for side effects - renders the cards).
#' @export
#'
#' @examples
#' # In server definition
#' mod_stats_card_row_server(
#'   id = "overview_cards",
#'   data = reactive({ economics_data }),
#'   card_configs = list(
#'     list(id = "unemployed", value_col = "unemploy", title = "Total Unemployed",
#'          good_if_increase = FALSE),
#'     list(id = "duration", value_col = "uempmed", title = "Duration (Weeks)",
#'          good_if_increase = FALSE),
#'     list(id = "population", value_col = "pop", title = "Population",
#'          good_if_increase = TRUE)
#'   )
#' )
mod_stats_card_row_server <- function(id, data, card_configs) {
  shiny::moduleServer(id, function(input, output, session) {

    # Create an output for each card configuration
    lapply(card_configs, function(config) {
      local({
        cfg <- config
        output[[cfg$id]] <- shiny::renderUI({
          d <- data()
          shiny::req(nrow(d) >= 2)

          # Get values
          values <- d[[cfg$value_col]]
          curr <- utils::tail(values, 1)
          prev <- utils::tail(values, 2)[1]
          delta <- curr - prev

          # Get optional configs with defaults
          period <- cfg$period %||% "vs last month"
          accent_hex <- cfg$accent_hex %||% "#cf102d"
          format_fn <- cfg$format_fn %||% govuk_format_number
          width_class <- cfg$width_class %||% "govuk-grid-column-one-third"

          govuk_stats_card(
            id               = session$ns(paste0(cfg$id, "_card")),
            title            = cfg$title,
            headline         = format_fn(curr),
            delta            = format_fn(delta),
            period           = period,
            accent_hex       = accent_hex,
            good_if_increase = cfg$good_if_increase,
            width_class      = width_class
          )
        })
      })
    })
  })
}



# modules/govuk_data_vis_card.R
# Dependencies: shiny (you already load GOV.UK Frontend JS/CSS)

# UI: a host card styled with GOV.UK classes and a govuk-tabs block.
# - visual_content: pass a ready-made UI element (e.g., plotlyOutput(ns("trend")))
# - Table and Download tabs are present but their panels are blank (for now).






# ------------------------------------------------------------------------------
# UKHSA Card UI (UI-only): title, hint, optional controls, then tabs + panels
# - No server logic; you wire all inputs/outputs in your main server.
# - 'controls' takes a list of standard Shiny UI tags (sliderInput, selectInput, …).
# - 'tabs' is a list of lists: list(list(id="chart", label="Chart"), …)
# - 'panels' is a named list: names must match tab ids: list(chart = ..., table = ..., ...)
# ------------------------------------------------------------------------------

#' Data Visualization Card Module UI
#'
#' Creates a UKHSA-styled card with tabbed interface for displaying charts,
#' tabular data, and download options. This is the main container for
#' interactive data visualizations in the dashboard.
#'
#' @param id Character. The module namespace ID.
#' @param title Character. Card title displayed as a heading.
#' @param help_text Character. Optional hint text displayed below the title.
#' @param visual_content Shiny UI element. The main visualization content
#'   (e.g., \code{plotlyOutput(ns("chart"))}).
#' @param controls List or tag. Optional control elements (sliders, dropdowns)
#'   displayed above the tabs.
#' @param table_content Shiny UI element. Optional content for the table tab.
#' @param download_content Shiny UI element. Optional content for the download tab.
#'
#' @return A Shiny tagList containing the styled card with tabs.
#' @export
#'
#' @examples
#' # Basic usage with a plotly chart
#' mod_govuk_data_vis_card_ui(
#'   id = ns("employment_chart"),
#'   title = "Employment by Age Group",
#'   help_text = "Shows employment levels across different age groups",
#'   visual_content = plotly::plotlyOutput(ns("chart"))
#' )
#'
#' # With control elements
#' mod_govuk_data_vis_card_ui(
#'   id = ns("trend_card"),
#'   title = "Employment Trend",
#'   visual_content = plotly::plotlyOutput(ns("trend")),
#'   controls = list(
#'     shiny::sliderInput(ns("year"), "Year Range", 2010, 2024, c(2015, 2024)),
#'     shiny::selectInput(ns("region"), "Region", choices = c("All", "London"))
#'   )
#' )
mod_govuk_data_vis_card_ui <- function(
  id,
  title,
  help_text = NULL,
  visual_content,
  controls = NULL,
  table_content = NULL,
  download_content = NULL
) {
  ns <- shiny::NS(id)

  # Helper to accept single tag or list of tags
  controls_block <- NULL
  if (!is.null(controls) && length(controls) > 0) {
    controls_block <- htmltools::tags$div(
      class = "ukhsa-controls",
      htmltools::tagList(controls)
    )
  }

  # default placeholders

  table_block <- table_content %||% htmltools::tags$p(class = "govuk-hint", "Table placeholder")
  download_block <- download_content %||% htmltools::tags$p(class = "govuk-hint", "Download placeholder")

  htmltools::tagList(
    # Include the collision-safe CSS/JS (ARIA selection; seamless merge; square corners)
    ukhsa_card_tabs_assets(),

    htmltools::tags$div(class = "lm-card-ukhsa",
      # Title + hint
      htmltools::tags$h2(class = "govuk-heading-m", title),
      if (!is.null(help_text)) htmltools::tags$p(class = "govuk-hint", help_text),

      # --- Optional controls (your slider, etc.) ---
      controls_block,

      # --- Tabs + panels (Chart selected by default) ---
      htmltools::tags$div(class = "ukhsa-tabs",
        # Tab buttons
        htmltools::tags$div(class = "ukhsa-tabs__list", role = "tablist",
          htmltools::tags$a(
            class = "ukhsa-tabs__tab",
            role = "tab", `aria-selected` = "true", tabindex = "0",
            `data-target` = ns("chart"), "Chart"
          ),
          htmltools::tags$a(
            class = "ukhsa-tabs__tab",
            role = "tab", `aria-selected` = "false", tabindex = "-1",
            `data-target` = ns("table"), "Tabular data"
          ),
          htmltools::tags$a(
            class = "ukhsa-tabs__tab",
            role = "tab", `aria-selected` = "false", tabindex = "-1",
            `data-target` = ns("download"), "Download"
          )
        ),

        # Panels: your visual content goes in the Chart panel (as before)
        htmltools::tags$div(
          id = ns("chart"), class = "ukhsa-tabs__panel", role = "tabpanel",
          visual_content
        ),
        htmltools::tags$div(
          id = ns("table"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          table_block
        ),
        htmltools::tags$div(
          id = ns("download"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          download_block
        )
      )
    ),

    # Small, scoped layout for the controls row (doesn't affect tab-panel merge)
    htmltools::tags$style(htmltools::HTML("
      .lm-card-ukhsa .ukhsa-controls {
        display: flex; flex-wrap: wrap;
        gap: 8px 12px;        /* spacing between controls */
        margin-top: 8px;      /* below hint */
        margin-bottom: 6px;   /* above tabs */
      }
      .lm-card-ukhsa .ukhsa-controls > * {
        flex: 0 1 280px;      /* sane default width per control */
        max-width: 100%;
      }
      @media (max-width: 640px) {
        .lm-card-ukhsa .ukhsa-controls { gap: 6px 8px; }
        .lm-card-ukhsa .ukhsa-controls > * { flex: 1 1 100%; }  /* stack on mobile */
      }
    "))
  )
}


#' Data Visualization Card Module Server
#'
#' Server logic for the data visualization card. Initializes the tab
#' switching functionality and provides a framework for wiring up
#' table displays and download handlers.
#'
#' @param id Character. The module namespace ID (must match the UI).
#'
#' @return NULL (called for side effects - initializes tab behavior).
#' @export
#'
#' @examples
#' # In server definition
#' mod_govuk_data_vis_card_server("employment_chart")
mod_govuk_data_vis_card_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Minimal server - wire table/download handlers here as needed

    # Ensure binding occurs after render passes
    session$onFlushed(function() {
      session$sendCustomMessage("ukhsa-tabs-init", list())
    }, once = FALSE)
  })
}



