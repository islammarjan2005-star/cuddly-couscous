
# ------------------------------------------------------------
# GOV.UK Helpers for Shiny Apps
# ------------------------------------------------------------

#' Add GOV.UK Frontend CSS/JS dependencies and initialise components
#'
#' Injects the GOV.UK Frontend styles and JavaScript into your Shiny app,
#' and calls `GOVUKFrontend.initAll()` once the DOM is ready. Supports GOV.UK
#' or DBT design variants.
#'
#' @param style Character. Which style set to use: `"GOVUK"` or `"DBT"`.
#'   Defaults to `"GOVUK"`.
#' @param css_govuk Character. Filename for GOV.UK CSS (in `www/`).
#' @param css_dbt Character. Filename for DBT CSS (in `www/`).
#' @param js Character. Filename for GOV.UK JS (in `www/`).
#'
#' @return A `tagList()` containing `<link>` and `<script>` tags for inclusion
#'   in the UI.
#'
#' @examples
#' # In your UI:
#' fluidPage(
#'   govuk_dependencies(style = "DBT"),
#'   h1("My GOV.UK styled app")
#' )
#'
#' @export


# Minimal local-only loader: GOV.UK + ONS (no CDN, no addResourcePath, no parameters)
govuk_dependencies <- function(
 style      = c("GOVUK", "DBT"),
 css_govuk  = "govuk-frontend-6.0.0-beta.0.min.css",
 css_dbt    = "DBT-frontend-6.0.0-beta.0.min.css",
 js_govuk   = "govuk-frontend-6.0.0-beta.0.min.js",
 include_ons = TRUE,  # set FALSE if you want to disable ONS
 disable_ons_js = TRUE,
 enforce_card_tabs_css = TRUE
) {
  style <- match.arg(style)
  css_file <- if (style == "DBT") css_dbt else css_govuk

  tagList(

    # -------------------------------
    # ONS Design System assets FIRST
    # -------------------------------
    if (isTRUE(include_ons)) tagList(
      # ONS CSS (kept)
      tags$link(rel = "stylesheet", href = "ons/css/main.css"),

      # ONS JS (OPTIONAL): only include if you need its behaviours on this page
      if (!isTRUE(disable_ons_js)) tagList(
        # Base URL must be defined BEFORE main.js (ONS chunks resolve relative to this)
        tags$script(HTML("var ONS_assets_base_URL = 'ons/';")),
        tags$script(src = "ons/scripts/main.js", defer = NA)
      )
    ),

    # --------------------------------
    # GOV.UK Frontend assets LAST
    # --------------------------------
    tags$link(rel = "stylesheet", href = css_file),
    tags$script(src = js_govuk, defer = NA),

    # Re-initialise GOV.UK after Shiny DOM updates (loaded from /www)
    tags$script(src = "govuk-shiny-init.js"),

    # --------------------------------
    # Scoped safety net for tab layout
    # --------------------------------
    if (isTRUE(enforce_card_tabs_css)) tags$style(HTML("
      /* Scoped to your GOV.UK/UKHSA-style card wrapper */
      .lm-card-ukhsa .govuk-tabs__list {
        display: flex;
        flex-wrap: wrap;
        border-bottom: 1px solid #b1b4b6;
        margin-bottom: 12px;
      }
      .lm-card-ukhsa .govuk-tabs__tab {
        display: inline-block;
        padding: 8px 12px;
      }
      .lm-card-ukhsa .govuk-tabs__panel {
        padding: 0; /* let inner content choose its spacing */
      }
      .lm-card-ukhsa .govuk-tabs__list-item--selected .govuk-tabs__tab {
        border-bottom: 3px solid #0b0c0c; /* matches GOV.UK */
      }
    "))
  )
}







#' Apply GOV.UK rebrand classes and meta tags
#'
#' Adds classes to `<html>` and `<body>` for GOV.UK rebranded styling,
#' and sets the theme colour meta tag.
#'
#' @return A `tagList()` with `<meta>` and `<script>` tags.
#'
#' @examples
#' fluidPage(
#'   govuk_apply_rebrand(),
#'   h1("Rebranded GOV.UK app")
#' )
#'
#' @export
govuk_apply_rebrand <- function() {
  tagList(
    tags$meta(name = "theme-color", content = "#1d70b8"),
    tags$script(HTML("
      (function () {
        document.documentElement.classList.add('govuk-template', 'govuk-template--rebranded');
        document.addEventListener('DOMContentLoaded', function () {
          document.body.classList.add('govuk-template__body');
        });
      })();
    "))
  )
}





DBT_widget_styling <- function() {
  tags$style(HTML("
    /* Generic widget overrides */
    .irs--shiny .irs-bar { background-color: #FF0000; border-color: #FF0000; height: 8px; }
    .irs--shiny .irs-handle > i:first-child { background-color: #FF0000; border-color: #FF0000; }
    .irs--shiny .irs-handle:hover > i:first-child,
    .irs--shiny .irs-handle:focus > i:first-child { background-color: #CC0000; border-color: #CC0000; }
    .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to {
      background-color: #FF0000; color: #FFFFFF; border-color: #FF0000;
    }
  "))
}

#' Add GOV.UK head assets (favicons, manifest)
#'
#' Injects favicon links, Apple touch icons, and manifest references into the
#' document head. Adjust paths to match your `www/assets/` structure.
#'
#' @return A `tagList()` with `<link>` tags for favicons and manifest.
#'
#' @examples
#' fluidPage(
#'   govuk_head_assets(),
#'   h1("App with GOV.UK favicons")
#' )
#'
#' @export
govuk_head_assets <- function() {
  tagList(
    tags$link(rel = "manifest", href = "assets/manifest.json"),
    tags$link(rel = "icon", type = "image/png",
              href = "assets/images/favicon-32x32.png", sizes = "32x32"),
    tags$link(rel = "icon", type = "image/png",
              href = "assets/images/favicon-16x16.png", sizes = "16x16"),
    tags$link(rel = "apple-touch-icon", href = "assets/images/apple-touch-icon.png"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "assets/rebrand/favicon.svg")
  )
}

#Data Vis Card Asset ----





# Router-safe UKHSA card + tabs assets (inline CSS+JS)
# - No tab offsets (removes the unexpected gap)
# - Active tab merges into panel via border-bottom removal + -1px margin overlap
# - No underline/bold on inactive/hover; tabs use govuk-body typographic feel
# - Slightly reduced top padding on the card
# - Height locking preserved so the card never shrinks when switching tabs


# Router-safe UKHSA card + tabs assets (inline CSS+JS)
# Tweaks in this version:
# - Active tab overlap -> -3px (virtually no visual gap to panel)
# - Panel top padding -> 4px
# - Inactive tab hover background -> #e0dedb (slightly darker)


# UKHSA card + tabs assets — roomy buttons & small panel gap
# Keeps collision-safe ARIA selection; adjustable spacing via CSS variables.

# UKHSA card + tabs assets — roomier buttons, merge with panel, square corners
ukhsa_card_tabs_assets <- function(
  accent_underline = FALSE,          # TRUE -> thin accent underline on selected tab
  accent_colour    = "#CF102D"       # DBT red if you enable accent underline
) {

  css <- paste0("
  /* ================================
     UKHSA card + tabs (scoped)
     ================================ */

  .lm-card-ukhsa {
    --card-border: #b1b4b6;   /* neutral border (panels) */
    --card-text:   #0b0c0c;   /* body text */
    --card-grey:   #f3f2f1;   /* card background */
    --card-grey-2: #eeece9;   /* hover baseline */
    --card-white:  #ffffff;

    /* Spacing knobs (no gap to panel) */
    --tab-pad-y: 8px;         /* vertical padding on tabs */
    --tab-pad-x: 14px;        /* horizontal padding on tabs */
    --tab-gap:    6px;        /* gap between tabs */

    background: var(--card-grey);
    border: none;
    border-radius: 0;                 /* square corners */
    padding: 16px 16px 16px 16px;
    width: 100%;
    box-sizing: border-box;
  }

  /* Headings + body copy inside the card */
  .lm-card-ukhsa .govuk-heading-m,
  .lm-card-ukhsa .govuk-heading-l { margin-top: 0; color: var(--card-text); }
  .lm-card-ukhsa .govuk-body,
  .lm-card-ukhsa .govuk-body-s,
  .lm-card-ukhsa .govuk-hint {
    color: #505a5f;               /* UKHSA-like grey */
    font-size: 14px;
    line-height: 1.45;
    font-weight: 400;
    margin-top: 0.25rem;
  }

  /* Tabs container */
  .lm-card-ukhsa .ukhsa-tabs { margin-top: 10px; }

  /* Tab list strip: prevent flex stretch; slightly larger gap between tabs */
  .lm-card-ukhsa .ukhsa-tabs__list {
    display: flex;
    flex-wrap: wrap;
    gap: var(--tab-gap);
    padding: 0;
    margin: 0;
    align-items: flex-start;   /* stops stretch to tallest item */
  }

  /* Base tab button — neutralize external margins & lock text metrics */
  .lm-card-ukhsa .ukhsa-tabs__tab {
    display: inline-block;
    cursor: pointer;
    text-decoration: none;
    background-color: var(--card-grey);
    color: var(--card-text);
    border: none;                          /* no outline on inactive */
    box-sizing: border-box;                /* borders won't grow box height */
    margin: 0;                             /* cancels any govuk/uk-body margins */
    font-size: 14px;                       /* lock scale to card body */
    line-height: 1.4;                      /* lock line box height */
    font-weight: 400;                      /* govuk-body weight */
    padding: var(--tab-pad-y) var(--tab-pad-x);
    border-radius: 0;                      /* square corners */
  }

  /* Darker hover */
  .lm-card-ukhsa .ukhsa-tabs__tab:hover {
    background-color: #e0dedb;             /* slightly darker than #eeece9 */
    color: var(--card-text);
  }

  /* Remove focus halo on tabs (retain keyboard focus within panel/tab context) */
  .lm-card-ukhsa .ukhsa-tabs__tab:focus,
  .lm-card-ukhsa .ukhsa-tabs__tab:focus-visible {
    outline: none;
    box-shadow: none;
  }

  /* Selected tab (ARIA) — merge visually with panel; text metrics unchanged */
  .lm-card-ukhsa .ukhsa-tabs__tab[aria-selected='true'] {
    text-decoration: none;
    background-color: var(--card-white);
    color: var(--card-text);
    border: 1px solid var(--card-border);
    border-bottom-color: transparent;  /* merges into panel top border */
    margin-bottom: -1px;               /* overlap to eliminate visible seam */
    z-index: 1;",
    if (isTRUE(accent_underline)) paste0(" box-shadow: inset 0 -1px 0 ", accent_colour, ";"), "
  }

  /* Panels: boxed area connected to selected tab; NO extra gap to tab strip */
  .lm-card-ukhsa .ukhsa-tabs__panel {
    border: 1px solid var(--card-border);
    background: var(--card-white);

    /* roomier padding inside the panel */
    padding: 8px 16px 16px;            /* top, sides, bottom */

    margin-top: 0;                      /* keep tight to tab strip */
    min-height: var(--panel-min-h, 0px);/* JS keeps this monotonic */
    box-sizing: border-box;
    border-radius: 0;                   /* square corners */
  }
  .lm-card-ukhsa .ukhsa-tabs__panel.is-hidden { display: none; }

  /* Optional inner frame for chart area (square corners; no extra gap) */
  .lm-card-ukhsa .ukhsa-tabs__panel[data-panel='chart'] {
    border: 1px solid var(--card-border);
    padding: 10px;
    background: var(--card-white);
  }

  /* Responsive compact spacing */
  @media (max-width: 640px) {
    .lm-card-ukhsa { padding: 18px 12px 12px 12px; }
    .lm-card-ukhsa .ukhsa-tabs__list { gap: 4px; }                    /* scale down gap on mobile */
    .lm-card-ukhsa .ukhsa-tabs__tab  { padding: 7px 12px; }           /* modest padding on mobile */
    .lm-card-ukhsa .ukhsa-tabs__panel { padding: 8px 14px 14px; }     /* proportional panel padding */
  }
  ")

  js <- "
  // Router-safe tabs (no hash) + height locking (monotonic)
  // Uses ARIA selected; no `.is-selected` class (collision-safe).
  (function () {
    function recalcHeights(container) {
      const visible = container.querySelector('.ukhsa-tabs__panel:not(.is-hidden)');
      const h = visible ? visible.offsetHeight : 0;
      const prev = container.__ukhsaMinH || 0;
      const next = Math.max(prev, h);
      container.__ukhsaMinH = next;
      container.style.setProperty('--panel-min-h', next + 'px');
    }

    function activate(container, targetId, triggerBtn) {
      const tabs   = Array.from(container.querySelectorAll('.ukhsa-tabs__tab'));
      const panels = Array.from(container.querySelectorAll('.ukhsa-tabs__panel'));

      // Clear selection (ARIA only)
      tabs.forEach(t => {
        t.setAttribute('aria-selected', 'false');
        t.setAttribute('tabindex', '-1');
      });
      panels.forEach(p => p.classList.add('is-hidden'));

      const nextTab   = tabs.find(t => t.getAttribute('data-target') === targetId) || triggerBtn;
      const nextPanel = panels.find(p => p.id === targetId);

      if (nextTab) {
        nextTab.setAttribute('aria-selected', 'true');
        nextTab.setAttribute('tabindex', '0');
        nextTab.focus();
      }
      if (nextPanel) nextPanel.classList.remove('is-hidden');

      requestAnimationFrame(() => setTimeout(() => recalcHeights(container), 30));
    }

    function bind(container) {
      if (!container || container.__ukhsaBound) return;
      container.__ukhsaBound = true;

      const tabList = container.querySelector('.ukhsa-tabs__list');
      const tabs    = Array.from(container.querySelectorAll('.ukhsa-tabs__tab'));

      if (tabList) tabList.setAttribute('role','tablist');

      tabs.forEach((t,i) => {
        t.setAttribute('role','tab');

        // Backward compatibility: convert any legacy .is-selected to ARIA and strip class
        const preSelected = t.classList.contains('is-selected') || t.getAttribute('aria-selected') === 'true';
        if (preSelected) t.setAttribute('aria-selected','true'); else t.setAttribute('aria-selected','false');
        t.classList.remove('is-selected');

        t.setAttribute('tabindex', preSelected ? '0' : '-1');

        const target = t.getAttribute('data-target');
        if (target) t.setAttribute('aria-controls', target);
        if (!t.id) t.id = (container.id || 'ukhsa-tabs') + '-tab-' + i;
      });

      Array.from(container.querySelectorAll('.ukhsa-tabs__panel')).forEach(p => {
        p.setAttribute('role','tabpanel');
        const labelledBy = tabs.find(t => t.getAttribute('data-target') === p.id);
        if (labelledBy) p.setAttribute('aria-labelledby', labelledBy.id);
      });

      container.addEventListener('click', function (e) {
        const btn = e.target.closest('.ukhsa-tabs__tab[data-target]');
        if (!btn) return;
        e.preventDefault(); e.stopPropagation();
        activate(container, btn.getAttribute('data-target'), btn);
      }, true);

      container.addEventListener('keydown', function (e) {
        const current = container.querySelector('.ukhsa-tabs__tab[aria-selected=\"true\"]');
        if (!current) return;
        const list = Array.from(container.querySelectorAll('.ukhsa-tabs__tab'));
        const idx = list.indexOf(current); let nextIdx = idx;
        switch (e.key) {
          case 'ArrowRight': nextIdx = (idx+1) % list.length; break;
          case 'ArrowLeft':  nextIdx = (idx-1+list.length) % list.length; break;
          case 'Home':       nextIdx = 0; break;
          case 'End':        nextIdx = list.length-1; break;
          default: return;
        }
        e.preventDefault();
        const nextTab = list[nextIdx];
        activate(container, nextTab.getAttribute('data-target'), nextTab);
      });

      // initial height lock after first paint
      requestAnimationFrame(() => setTimeout(() => recalcHeights(container), 30));
    }

    function bindAll(){ document.querySelectorAll('.lm-card-ukhsa .ukhsa-tabs').forEach(bind); }

    document.addEventListener('DOMContentLoaded', bindAll);
    document.addEventListener('shiny:connected', bindAll);
    document.addEventListener('shiny:recalculated', () => {
      bindAll();
      document.querySelectorAll('.lm-card-ukhsa .ukhsa-tabs').forEach(c => {
        requestAnimationFrame(() => setTimeout(() => {
          const visible = c.querySelector('.ukhsa-tabs__panel:not(.is-hidden)');
          if (visible) {
            const h = visible.offsetHeight || 0;
            if ((c.__ukhsaMinH || 0) < h) {
              c.__ukhsaMinH = h;
              c.style.setProperty('--panel-min-h', h + 'px');
            }
          }
        }, 30));
      });
    });
    document.addEventListener('shiny:value', bindAll, true);

    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('ukhsa-tabs-init', bindAll);
    }
  })();
  "

  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(css)),
    htmltools::tags$script(htmltools::HTML(js))
  )
}
















# Side Nav Asset ----
govuk_toc_assets <- function(
  NAVBAR_HEIGHT_PX     = 149,   # header height at top of page
  MIN_OFFSET_PX        = 12,    # smallest top offset after shrink
  DRAWER_BREAKPOINT_PX = 1500   # viewport width threshold for drawer mode
) {
  stopifnot(is.numeric(NAVBAR_HEIGHT_PX), NAVBAR_HEIGHT_PX > 0)
  stopifnot(is.numeric(MIN_OFFSET_PX), MIN_OFFSET_PX > 0, MIN_OFFSET_PX <= NAVBAR_HEIGHT_PX)
  stopifnot(is.numeric(DRAWER_BREAKPOINT_PX), DRAWER_BREAKPOINT_PX > 0)

  # -----------------------
  # CSS
  # -----------------------
  css <- sprintf("
  html { scroll-behavior: smooth; }

  /* Global custom properties for FAB & drawer geometry */
  :root {
    --fab-size: 44px;
    --fab-radius: calc(var(--fab-size) / 2);  /* 22px for size 44 */
    --drawer-pad-left: 12px;                  /* keep in sync with drawer padding-left */
    --drawer-pad-top: 12px;                   /* keep in sync with drawer padding-top  */
  }

  /* GOV.UK container stays centred */
  .govuk-width-container {
    position: relative;
    max-width: 960px; /* GOV.UK default */
    margin: 0 auto;
  }

  /* Base overlay sidebar (default state: expanded) — blend into background */
  .sidebar {
    position: fixed;
    top: calc(var(--current-offset, %1$dpx) + %2$dpx);
    left: max(12px, calc((100%% - 960px) / 2 - 240px));
    width: 220px;
    max-height: calc(100vh - var(--current-offset, %1$dpx) - %2$dpx * 2);
    overflow: auto;

    /* blend (no box-shadow/border) */
    background: transparent;
    box-shadow: none;
    border: none;
    padding-right: 0.5rem;

    display: flex; flex-direction: column;
    z-index: 1000; /* draw over content */
  }

  /* Drawer mode — elevation, padding, and rounded top-right corner */
  .sidebar.sidebar--drawer {
    left: 0; /* off-canvas from page edge */
    width: min(85vw, 360px);
    height: calc(100vh - calc(var(--current-offset, %1$dpx) + %2$dpx));
    max-height: none;
    transform: translateX(-100%%);   /* hidden by default in drawer mode */
    transition: transform .25s ease; /* controlled via JS for first-load */
    will-change: transform;

    /* elevate only in drawer mode */
    background: #fff;
    box-shadow: 0 0 0 1px rgba(0,0,0,.08), 0 12px 24px rgba(0,0,0,.24);

    /* comfortable spacing inside drawer (controls FAB geometry too) */
    padding-top: var(--drawer-pad-top);
    padding-left: var(--drawer-pad-left);
    padding-right: 12px;
    padding-bottom: 12px;

    /* Rounded top-right = FAB radius + left padding */
    border-top-right-radius: calc(var(--fab-radius) + var(--drawer-pad-left));
  }
  .sidebar.sidebar--drawer.sidebar--open { transform: translateX(0); }

  /* NEW: disable transitions for initial closed state on small screens */
  .sidebar.sidebar--drawer.sidebar--no-transition { transition: none !important; }

  /* Scrim over the page (no click-to-close) */
  .sidebar__scrim {
    position: fixed; inset: 0;
    background: rgba(0,0,0,.35);
    z-index: 999;
    opacity: 0; pointer-events: none; /* keep non-interactive to avoid accidental closes */
    transition: opacity .2s ease;
  }
  .sidebar__scrim--visible { opacity: 1; pointer-events: auto; }

  /* -------- FABs --------
     Two FABs per sidebar:
     - .sidebar__fab--panel: inside the sidebar (rides the transform smoothly)
     - .sidebar__fab--window: left-edge 'tab' (shown when drawer is closed)
  */

  /* Shared look — minimal grey palette */
  .sidebar__fab {
    width: var(--fab-size);
    min-height: var(--fab-size);
    display: inline-flex; align-items: center; justify-content: center;
    background: #f3f2f1;              /* light grey */
    color: #0b0c0c;                    /* dark grey icon */
    border: 1px solid rgba(0,0,0,.15); /* subtle outline */
    box-shadow: 0 2px 6px rgba(0,0,0,.08);
    cursor: pointer;
  }
  .sidebar__fab:hover  { background: #e6e6e6; }
  .sidebar__fab:active { background: #dbdbdb; }
  .sidebar__fab:focus-visible {
    outline: 3px solid #ffbf47; outline-offset: 2px;  /* GOV.UK focus colour */
  }
  .sidebar__fab svg {
    width: 22px; height: 22px; display: block; fill: currentColor;
  }

  /* Panel FAB (inside panel) — circular, positioned by padding geometry
     Centre distance from right edge = FAB radius + left padding.
     Right CSS offset = desired-centre - radius = (r + padLeft) - r = padLeft
  */
  .sidebar__fab--panel {
    position: absolute;
    top: var(--drawer-pad-top);
    right: var(--drawer-pad-left);
    border-radius: 9999px;   /* circular inside the panel */
    z-index: 10;              /* above nav inside the panel */
  }

  /* Window FAB (left-edge 'tab'): flat left, rounded right (hamburger only)
     Vertical alignment matches the panel FAB (adds drawer top padding).
  */
  .sidebar__fab--window {
    position: fixed;
    left: 0; /* pinned to viewport left edge */
    top: calc(var(--current-offset, %1$dpx) + %2$dpx + var(--drawer-pad-top));
    z-index: 1002; /* above sidebar + scrim */
    transition: top .1s linear;

    /* Tab silhouette: flat left, rounded right */
    border-radius: 0 var(--fab-radius) var(--fab-radius) 0;

    padding: 6px 10px;
    min-width: var(--fab-size);
  }

  /* Keep ONS nav flush */
  .sidebar .ons-section-nav { margin: 0; }

  /* Avoid heading hidden under header+offset when scrolling */
  section[id] { scroll-margin-top: calc(var(--current-offset, %1$dpx) + %2$dpx); }

  /* Sidebar footer pinned */
  .sidebar__footer {
    margin-top: auto; position: sticky; bottom: 0;
    background: #f3f2f1; border-top: 1px solid rgba(0,0,0,0.08); padding: 8px;
  }

  /* ---- ACTIVE look (ONS-like) ---- */
  .ons-section-nav__item--active > .ons-section-nav__link,
  .ons-section-nav__item--active > .nav-link {
    color: #7D0A1B !important;
    font-weight: 700 !important;
    background-color: rgba(243, 242, 241, 0.12) !important;
    border-left: 6px solid #B50E27 !important;
    padding-left: 12px !important;
    text-decoration: none !important;
  }

  /* Non-active links */
  .ons-section-nav__item > .ons-section-nav__link,
  .ons-section-nav__item > .nav-link {
    display: block; padding: 6px 8px; text-decoration: none;
    transition: background-color .2s ease, color .2s ease;
  }
  .sidebar .ons-section-nav__item:not(.ons-section-nav__item--active) > .ons-section-nav__link,
  .sidebar .ons-section-nav__item:not(.ons-section-nav__item--active) > .nav-link { color: #CF102D !important; }
  .sidebar .ons-section-nav__item:not(.ons-section-nav__item--active) > .ons-section-nav__link:visited,
  .sidebar .ons-section-nav__item:not(.ons-section-nav__item--active) > .nav-link:visited { color: #CF102D !important; }
  .sidebar .ons-section-nav__item:not(.ons-section-nav__item--active) > .ons-section-nav__link:hover,
  .sidebar .ons-section-nav__item:not(.ons-section-nav__item--active) > .nav-link:hover {
    color: #7D0A1B !important; text-decoration: underline solid #7D0A1B 2px;
  }
  .sidebar .nav-link.active {
    color: inherit !important; background: transparent !important; border-left: none !important;
    text-transform: none !important; font-weight: inherit !important;
  }

  /* Back to top styling */
  .govuk-back-to-top { display: inline-flex; align-items: center; gap: 6px; font-weight: bold; text-decoration: none; color: #1d70b8; }
  .govuk-back-to-top:hover, .govuk-back-to-top:focus { color: #003078; text-decoration: underline; }
  .govuk-back-to-top__icon { width: 25px; height: 25px; display: inline-block; }
  ",
  NAVBAR_HEIGHT_PX, MIN_OFFSET_PX)

  # -----------------------
  # JavaScript (first-load no-animation close; FAB only closes; keep open on link click)
  # -----------------------
  js <- paste0(
"(function(){
  const NAVBAR_HEIGHT = ", NAVBAR_HEIGHT_PX, ";
  const MIN_OFFSET    = ", MIN_OFFSET_PX, ";
  const DRAW_BP       = ", DRAWER_BREAKPOINT_PX, ";
  const SHRINK_RANGE  = NAVBAR_HEIGHT - MIN_OFFSET;

  // Match CSS transition
  const DRAWER_TRANSITION_MS = 250;
  const FAB_SWAP_DELAY_MS    = 60;

  function computeOffset(){
    var y = window.pageYOffset || document.documentElement.scrollTop || 0;
    var shrink = Math.min(y, SHRINK_RANGE);
    return NAVBAR_HEIGHT - shrink;
  }
  var lastApplied = -1;
  function applyOffsetOnce(){
    var o = computeOffset();
    if (Math.abs(o - lastApplied) >= 1){
      document.documentElement.style.setProperty('--current-offset', Math.round(o) + 'px');
      lastApplied = o;
    }
  }

  // Router-aware visibility check
  function isTreeHidden(el){
    for (let n = el; n; n = n.parentElement){
      const s = window.getComputedStyle(n);
      if (s.display === 'none' || s.visibility === 'hidden' || n.hidden) return true;
    }
    return false;
  }

  function getRouteAndFragment(){
    var h = window.location.hash || '';
    if (!h.startsWith('#!/')) return { route:'', fragment:'' };
    var raw = h.slice(3);
    var parts = raw.split('#');
    return { route: parts[0]||'', fragment: parts[1]||'' };
  }
  function setRouteFragment(route, fragment){
    var newHash = '#!/' + (route||'') + (fragment ? ('#'+fragment) : '');
    history.replaceState(null, document.title, newHash);
  }

  function scrollToSection(id){
    var target = document.getElementById(id);
    if (!target) return;
    var currentOffset = computeOffset();
    var top = target.getBoundingClientRect().top + window.pageYOffset - currentOffset - 8;
    var reduce = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    window.scrollTo({ top: top, behavior: reduce ? 'auto' : 'smooth' });
  }

  function updateActive(sidebar, sections, links) {
    if (!sections.length || !links.length) return;
    const offset = computeOffset();
    const viewportTop = offset + 8;
    let bestIdx = -1, bestDist = Infinity;
    for (let i = 0; i < sections.length; i++) {
      const rect = sections[i].getBoundingClientRect();
      const dist = Math.abs(rect.top - viewportTop);
      const isCandidate = (rect.bottom > viewportTop) && (rect.top <= window.innerHeight * 0.6);
      if (isCandidate && dist < bestDist) { bestDist = dist; bestIdx = i; }
    }
    if (bestIdx === -1) {
      for (let i = 0; i < sections.length; i++) {
        const rect = sections[i].getBoundingClientRect();
        const dist = Math.abs(rect.top - viewportTop);
        if (dist < bestDist) { bestDist = dist; bestIdx = i; }
      }
    }
    const items = Array.from(sidebar.querySelectorAll('.ons-section-nav__item'));
    items.forEach(li => li.classList.remove('ons-section-nav__item--active'));
    links.forEach(a => a.removeAttribute('aria-current'));
    if (bestIdx !== -1) {
      const id = sections[bestIdx].id;
      const match = sidebar.querySelector('.nav-link[data-section=\"' + id + '\"]');
      if (match) {
        const li = match.closest('.ons-section-nav__item');
        if (li) li.classList.add('ons-section-nav__item--active');
        match.setAttribute('aria-current', 'location');
      }
    }
  }

  function ensureBackToTop(sidebar, links, sections){
    if (sidebar.querySelector('.sidebar__footer')) return;
    var footer = document.createElement('div');
    footer.className = 'sidebar__footer';
    footer.innerHTML = [
      '<a href=\"#\" class=\"govuk-link govuk-back-to-top\" aria-label=\"Back to top\">',
      '  <svg class=\"govuk-back-to-top__icon\" viewBox=\"0 0 24 24\" aria-hidden=\"true\" focusable=\"false\">',
      '    <path d=\"M12 5l-6 6h4v8h4v-8h4z\" fill=\"currentColor\"/>',
      '  </svg>',
      '  <span>Back to top</span>',
      '</a>'
    ].join('');
    sidebar.appendChild(footer);
    var link = footer.querySelector('.govuk-back-to-top');
    link.addEventListener('click', function(ev){
      ev.preventDefault();
      var rf = getRouteAndFragment();
      setRouteFragment(rf.route, '');
      links.forEach(a => a.removeAttribute('aria-current'));
      Array.from(sidebar.querySelectorAll('.ons-section-nav__item'))
           .forEach(li => li.classList.remove('ons-section-nav__item--active'));
      var reduce = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
      window.scrollTo({ top: 0, behavior: reduce ? 'auto' : 'smooth' });
      requestAnimationFrame(function(){ applyOffsetOnce(); updateActive(sidebar, sections, links); });
    });
  }

  function uniqueId(){ return 'sidebar-' + Math.random().toString(36).slice(2); }
  function ensureScrim(){ let s = document.querySelector('.sidebar__scrim'); if (!s){ s = document.createElement('div'); s.className = 'sidebar__scrim'; document.body.appendChild(s); } return s; }

  // Create two FABs per sidebar: inside panel & fixed left-edge tab (hamburger only)
  function ensureFabs(sidebar){
    // Panel FAB (inside)
    let panelFab = sidebar.querySelector('.sidebar__fab--panel');
    if (!panelFab){
      panelFab = document.createElement('button');
      panelFab.type = 'button';
      panelFab.className = 'sidebar__fab sidebar__fab--panel';
      panelFab.setAttribute('aria-controls', sidebar.id);
      panelFab.setAttribute('aria-label', 'Toggle sections');
      panelFab.innerHTML = '<svg viewBox=\"0 0 24 24\" aria-hidden=\"true\" focusable=\"false\">'
                         + '  <rect x=\"3\" y=\"6\"  width=\"18\" height=\"2\" rx=\"1\"></rect>'
                         + '  <rect x=\"3\" y=\"11\" width=\"18\" height=\"2\" rx=\"1\"></rect>'
                         + '  <rect x=\"3\" y=\"16\" width=\"18\" height=\"2\" rx=\"1\"></rect>'
                         + '</svg>';
      sidebar.appendChild(panelFab);
    }

    // Window FAB (fixed tab; hamburger only)
    let windowFab = document.querySelector('.sidebar__fab--window[data-for=\"' + sidebar.id + '\"]');
    if (!windowFab){
      windowFab = document.createElement('button');
      windowFab.type = 'button';
      windowFab.className = 'sidebar__fab sidebar__fab--window';
      windowFab.setAttribute('data-for', sidebar.id);
      windowFab.setAttribute('aria-controls', sidebar.id);
      windowFab.setAttribute('aria-label', 'Toggle sections');
      windowFab.innerHTML =
        '<svg viewBox=\"0 0 24 24\" aria-hidden=\"true\" focusable=\"false\">' +
        '  <rect x=\"3\" y=\"6\"  width=\"18\" height=\"2\" rx=\"1\"></rect>' +
        '  <rect x=\"3\" y=\"11\" width=\"18\" height=\"2\" rx=\"1\"></rect>' +
        '  <rect x=\"3\" y=\"16\" width=\"18\" height=\"2\" rx=\"1\"></rect>' +
        '</svg>';
      document.body.appendChild(windowFab);
    }
    return { panelFab, windowFab };
  }

  // Show/hide correct FAB; IMPORTANT: no FABs in overlay mode
  function updateFabState(sidebar, fabs){
    const { panelFab, windowFab } = fabs;
    const visible = !isTreeHidden(sidebar);
    if (!visible){ panelFab.style.display = 'none'; windowFab.style.display = 'none'; return; }

    const animClosing = sidebar.classList.contains('sidebar--anim-closing');
    const inDrawer    = sidebar.classList.contains('sidebar--drawer');
    const isOpen      = sidebar.classList.contains('sidebar--open');

    if (animClosing){
      // During close animation keep panel FAB visible, window FAB hidden
      panelFab.style.display  = 'inline-flex';
      windowFab.style.display = 'none';
      return;
    }

    if (!inDrawer) {
      // Overlay: NO FABs
      panelFab.style.display  = 'none';
      windowFab.style.display = 'none';
    } else if (isOpen) {
      // Drawer open: show panel FAB (inside)
      panelFab.style.display  = 'inline-flex';
      windowFab.style.display = 'none';
    } else {
      // Drawer closed: show window FAB (left-edge tab)
      panelFab.style.display  = 'none';
      windowFab.style.display = 'inline-flex';
    }
  }

  // --- Drawer/Overlay helpers ---
  let closeSwapTimer = null;
  function clearCloseTimer(){ if (closeSwapTimer){ clearTimeout(closeSwapTimer); closeSwapTimer = null; } }

  function openDrawer(sidebar, scrim, fabs){
    sidebar.classList.add('sidebar--drawer');
    sidebar.classList.remove('sidebar--anim-closing'); // ensure flag cleared
    sidebar.classList.add('sidebar--open');
    scrim.classList.add('sidebar__scrim--visible');
    clearCloseTimer();
    updateFabState(sidebar, fabs);
    const firstLink = sidebar.querySelector('.nav-link, .ons-section-nav__link, a, button');
    if (firstLink) firstLink.focus();
  }

  function closeDrawer(sidebar, scrim, fabs){
    // Stay in drawer; begin close animation
    sidebar.classList.add('sidebar--anim-closing'); // keep panel FAB visible while sliding out
    sidebar.classList.remove('sidebar--open');
    scrim.classList.remove('sidebar__scrim--visible');
    clearCloseTimer();

    // Keep panel FAB during transition
    updateFabState(sidebar, fabs);

    const onEnd = function(ev){
      if (ev.propertyName !== 'transform') return;
      sidebar.removeEventListener('transitionend', onEnd);
      clearCloseTimer();
      sidebar.classList.remove('sidebar--anim-closing');
      updateFabState(sidebar, fabs); // now show window FAB
    };
    sidebar.addEventListener('transitionend', onEnd);

    // Safety swap if transitionend is missed
    closeSwapTimer = setTimeout(function(){
      sidebar.classList.remove('sidebar--anim-closing');
      updateFabState(sidebar, fabs);
    }, DRAWER_TRANSITION_MS + FAB_SWAP_DELAY_MS);
  }

  // Breakpoint check
  function shouldUseDrawer(){ return window.innerWidth < DRAW_BP; }

  // First-load entry to drawer CLOSED with NO TRANSITION
  function enterDrawerClosedNoAnim(sidebar, scrim, fabs){
    sidebar.classList.add('sidebar--drawer', 'sidebar--no-transition');
    sidebar.classList.remove('sidebar--open', 'sidebar--anim-closing');
    scrim.classList.remove('sidebar__scrim--visible');
    updateFabState(sidebar, fabs);
    // Force reflow, then remove the no-transition flag
    void sidebar.offsetHeight;
    requestAnimationFrame(function(){ sidebar.classList.remove('sidebar--no-transition'); });
  }

  // Enforce the right mode by breakpoint
  // INITIAL LOAD: if we enter drawer mode, start CLOSED (no animation)
  // SUBSEQUENT RESIZE: overlay->drawer transitions OPEN the drawer
  function enforceMode(sidebar, scrim, fabs, isInitial){
    if (!sidebar) return;
    const visible = !isTreeHidden(sidebar);
    if (!visible){ updateFabState(sidebar, fabs); return; }

    if (shouldUseDrawer()){
      if (!sidebar.classList.contains('sidebar--drawer')){
        // Entering drawer mode from overlay or first load
        if (isInitial || !sidebar.__seenOverlay){
          enterDrawerClosedNoAnim(sidebar, scrim, fabs);  // <-- no animation
        } else {
          openDrawer(sidebar, scrim, fabs);               // <-- open on later transition
        }
      } else {
        // Already in drawer: respect current open/closed state
        updateFabState(sidebar, fabs);
      }
    } else {
      // Force overlay (non-drawer) when width >= breakpoint
      if (sidebar.classList.contains('sidebar--drawer')){
        sidebar.classList.remove('sidebar--drawer', 'sidebar--open', 'sidebar--anim-closing', 'sidebar--no-transition');
        scrim.classList.remove('sidebar__scrim--visible');
      }
      // Mark that we've rendered overlay at least once
      sidebar.__seenOverlay = true;
      updateFabState(sidebar, fabs);
    }
  }

  function bindSidebar(sidebar){
    if (!sidebar) return;
    if (!sidebar.id) sidebar.id = uniqueId();
    if (sidebar.__tocBound) return;
    sidebar.__tocBound = true;
    sidebar.__seenOverlay = false;  // track if overlay has been visible this session

    const links = Array.from(sidebar.querySelectorAll('.nav-link[data-section]'));
    if (!links.length) { sidebar.__tocBound = false; setTimeout(function(){ bindSidebar(sidebar); }, 50); return; }

    const scrim = ensureScrim();
    const fabs  = ensureFabs(sidebar);
    const { panelFab, windowFab } = fabs;

    // Toggle only active in drawer mode (small widths)
    function toggleDrawer(){
      if (!shouldUseDrawer()){
        // Overlay enforced by breakpoint: ignore FAB clicks
        return;
      }
      if (!sidebar.classList.contains('sidebar--drawer')) {
        // From overlay on small width: first click ends in drawer-closed (guard)
        sidebar.classList.add('sidebar--drawer', 'sidebar--no-transition');
        sidebar.classList.add('sidebar--anim-closing'); // harmless when no-transition is set
        sidebar.classList.remove('sidebar--open');
        scrim.classList.remove('sidebar__scrim--visible');
        updateFabState(sidebar, fabs);
        void sidebar.offsetHeight;
        requestAnimationFrame(function(){ sidebar.classList.remove('sidebar--no-transition'); });
        return;
      }

      const isOpen = sidebar.classList.contains('sidebar--open');
      isOpen ? closeDrawer(sidebar, scrim, fabs) : openDrawer(sidebar, scrim, fabs);
    }

    function onFabClick(ev){ ev.preventDefault(); if (isTreeHidden(sidebar)) return; toggleDrawer(); }
    panelFab.addEventListener('click', onFabClick);
    windowFab.addEventListener('click', onFabClick);

    // Clicking nav links SHOULD NOT close the drawer.
    // Keep drawer open and let smooth scroll proceed.
    sidebar.addEventListener('click', function(ev){
      const a = ev.target.closest('a[data-section]');
      if (!a) return;
      ev.preventDefault();
      const section = a.getAttribute('data-section');
      const rf = getRouteAndFragment();
      setRouteFragment(rf.route, section);
      scrollToSection(section);
      // DO NOT close the drawer here.
    });

    // Sections list for scroll-spy
    let sections = links.map(a => document.getElementById(a.getAttribute('data-section'))).filter(Boolean);
    if (!sections.length){ sidebar.__tocBound = false; setTimeout(function(){ bindSidebar(sidebar); }, 50); return; }

    ensureBackToTop(sidebar, links, sections);

    // Initial fragment sync
    const rf = getRouteAndFragment();
    if (rf.fragment){
      const initial = sidebar.querySelector('.nav-link[data-section=\"' + rf.fragment + '\"]');
      if (initial){
        links.forEach(l => {
          l.classList.remove('active');
          l.removeAttribute('aria-current');
          const pli = l.closest('.ons-section-nav__item');
          if (pli) pli.classList.remove('ons-section-nav__item--active');
        });
        initial.setAttribute('aria-current', 'location');
        const li = initial.closest('.ons-section-nav__item');
        if (li) li.classList.add('ons-section-nav__item--active');
        setTimeout(function(){ scrollToSection(rf.fragment); }, 0);
      }
    }

    // Scroll spy, initial mode & FAB state
    let ticking = false;
    function onScrollSpy(){ if (!ticking){ ticking = true; requestAnimationFrame(function(){ applyOffsetOnce(); updateActive(sidebar, sections, links); ticking = false; }); } }
    applyOffsetOnce(); updateActive(sidebar, sections, links);
    enforceMode(sidebar, scrim, fabs, /* isInitial: */ true);   // enforce mode by breakpoint on load

    window.addEventListener('scroll', onScrollSpy, { passive: true });
    window.addEventListener('resize', function(){ applyOffsetOnce(); enforceMode(sidebar, scrim, fabs, /* isInitial: */ false); }, { passive: true });

    // Ensure FAB updates at the end of transitions
    sidebar.addEventListener('transitionend', function(ev){
      if (ev.propertyName === 'transform') updateFabState(sidebar, fabs);
    });
  }

  // Bind all sidebars
  function bindAll(){ Array.from(document.querySelectorAll('.sidebar')).forEach(bindSidebar); }

  // Router-aware re-evaluation (hash routers + DOM visibility) -> re-enforce mode
  function refreshAll(){
    Array.from(document.querySelectorAll('.sidebar')).forEach(sb => {
      if (!sb.__tocBound) return;
      const scrim = ensureScrim();
      const panelFab = sb.querySelector('.sidebar__fab--panel');
      const windowFab = document.querySelector('.sidebar__fab--window[data-for=\"' + sb.id + '\"]');
      if (panelFab && windowFab){
        const fabs = { panelFab, windowFab };
        enforceMode(sb, scrim, fabs, /* isInitial: */ false);
      }
    });
  }

  // MutationObserver watches for display/class/hidden changes (router)
  let moQueued = false;
  const mo = new MutationObserver(function(){
    if (!moQueued){
      moQueued = true;
      requestAnimationFrame(function(){ moQueued = false; refreshAll(); });
    }
  });

  document.addEventListener('DOMContentLoaded', function(){
    applyOffsetOnce();
    bindAll();
    // Do not force refreshAll immediately (avoids accidental auto-open on initial small width).
    mo.observe(document.body, { subtree: true, attributes: true, attributeFilter: ['style','class','hidden'] });
  });
  window.addEventListener('hashchange', function(){ setTimeout(refreshAll, 0); }, { passive: true });
})();"
  )

  # -----------------------
  # Scoped override to remove GOV.UK focus halo inside sidebar
  # -----------------------
  local_focus_override_css <- "
  .sidebar a.nav-link:focus,
  .sidebar a.nav-link:active,
  .sidebar a.nav-link:focus-visible,
  .sidebar .ons-section-nav__link:focus,
  .sidebar .ons-section-nav__link:active,
  .sidebar .ons-section-nav__link:focus-visible,
  .sidebar .govuk-link:focus,
  .sidebar .govuk-link:active,
  .sidebar .govuk-link:focus-visible,
  .sidebar a:focus,
  .sidebar a:active,
  .sidebar a:focus-visible {
    background-color: transparent !important;
    color: inherit !important;
    box-shadow: none !important; /* removes GOV.UK focus halo */
    outline: none !important;
    text-decoration: none !important;
  }
  "

  htmltools::tags$head(
    htmltools::tags$style(htmltools::HTML(css)),
    htmltools::tags$style(htmltools::HTML(local_focus_override_css)),
    htmltools::tags$script(htmltools::HTML(js))
  )
}




















