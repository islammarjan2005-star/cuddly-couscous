
# Text Formats
govuk_format_number   <- function(x) scales::comma(x)           # 12,345
govuk_format_percent1 <- function(x) sprintf("%.1f%%", x)       # 12.3%


# Internal: pick tag colour from delta sign and "is increase good?"
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


# -------------------------
# Component: GOV.UK Stats Card
# -------------------------
govuk_stats_card <- function(
    id,
    title,
    headline,                  # formatted string OR numeric; displayed as main value
    delta = NULL,              # formatted string OR numeric; displayed in a govuk-tag
    period = "vs last month",  # short label after delta
    accent_hex = "#cf102d",    # << simple hex for the top border (dbtred default)
    good_if_increase = TRUE,   # if TRUE: increase => green tag; else => red tag
    tag_colour = NULL,         # optional override: 'green'|'red'|'blue'
    width_class = "govuk-grid-column-one-third",
    classes = NULL,            # extra classes on the summary card
    format_headline = NULL,    # optional numeric formatter for headline
    format_delta = NULL        # optional numeric formatter for delta
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
      # Use your requested hex for the accent
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

mod_govuk_data_vis_card_ui <- function(
  id,
  title,
  help_text = NULL,
  visual_content,
  controls = NULL   # <- NEW: a single tag or a list of tags (e.g., sliderInput(...))
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
          htmltools::tags$p(class = "govuk-hint", "Table placeholder")
        ),
        htmltools::tags$div(
          id = ns("download"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          htmltools::tags$p(class = "govuk-hint", "Download placeholder")
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


mod_govuk_data_vis_card_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Keep minimal; wire your table/download here later (reactable, downloadHandler, etc.)

    # Ensure binding occurs after render passes
    session$onFlushed(function() {
      session$sendCustomMessage("ukhsa-tabs-init", list())
    }, once = FALSE)
  })
}



