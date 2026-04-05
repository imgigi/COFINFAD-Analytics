# ============================================================
# COFINFAD Visual Analytics Shiny Application
# Democratizing Fintech Analytics - Team 13
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(glmnet)
library(cluster)
library(factoextra)
library(lubridate)
library(shinycssloaders)
library(viridis)
library(ggstatsplot)

# sf + leaflet for choropleth — optional
has_spatial <- tryCatch({
  library(sf)
  library(leaflet)
  TRUE
}, error = function(e) FALSE)

# Check spatial data exists
dept_sf_file <- "data/dept_spatial.rds"
if (!file.exists(dept_sf_file)) has_spatial <- FALSE

# ============================================================
# Load Pre-processed Data
# ============================================================
customers       <- readRDS("data/customers.rds")
analytics_data  <- readRDS("data/analytics_data.rds")
monthly_summary <- readRDS("data/monthly_summary.rds")
monthly_total   <- readRDS("data/monthly_total.rds")
rfm_data        <- readRDS("data/rfm_data.rds")
dept_stats      <- readRDS("data/dept_stats.rds")

# Pre-computed clusters & models
cluster_pre <- readRDS("data/cluster_precomputed.rds")
model_pre   <- readRDS("data/model_precomputed.rds")

# Spatial data (may not exist)
dept_sf <- tryCatch(readRDS("data/dept_spatial.rds"), error = function(e) NULL)

# Churn thresholds (data-driven Q25 / Q75)
churn_q    <- quantile(customers$churn_probability, c(0.25, 0.75), na.rm = TRUE)
churn_low  <- round(churn_q[1], 3)
churn_high <- round(churn_q[2], 3)

# All possible clustering features (for Custom preset)
ALL_CLUSTER_VARS <- c("tx_count", "avg_tx_value", "total_tx_volume",
                       "weekend_transaction_ratio", "customer_tenure",
                       "credit_utilization_ratio", "active_products",
                       "satisfaction_score", "spending_volatility",
                       "recency_days")

# ============================================================
# Color Palette
# ============================================================
BG_DARK    <- "#080808"
BG_CARD    <- "#111111"
BG_SIDEBAR <- "#0d0d0d"
BG_INPUT   <- "#1a1a1a"
ACCENT     <- "#f97316"
ACCENT2    <- "#a855f7"
ACCENT3    <- "#fb7185"
ACCENT4    <- "#fbbf24"
TEXT_MAIN  <- "#f1f5f9"
TEXT_DIM   <- "#8a9bb0"
BORDER     <- "#252525"
GRID_COL   <- "#1e1e1e"

type_colors    <- c("Transfer" = "#f97316", "Withdrawal" = "#fb7185",
                    "Payment" = "#fbbf24",  "Deposit"   = "#a855f7")
segment_colors <- c("inactive" = "#fb7185", "occasional" = "#fbbf24",
                    "regular"  = "#f97316", "power"     = "#a855f7")
income_colors  <- c("Low" = "#fb7185", "Medium" = "#fbbf24",
                    "High" = "#f97316", "Very High" = "#a855f7")
churn_colors   <- c("Low" = "#a855f7", "Medium" = "#fbbf24", "High" = "#fb7185")
cluster_pal    <- c("#f97316","#a855f7","#fbbf24","#fb7185",
                    "#f472b6","#e879f9","#fdba74","#c084fc")

# ggplot dark theme
theme_dark_dash <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_line(color = GRID_COL, linewidth = 0.25),
      panel.grid.minor = element_blank(),
      text             = element_text(color = TEXT_DIM, family = ""),
      axis.text        = element_text(color = TEXT_DIM,  size = base_size - 2),
      axis.title       = element_text(color = TEXT_MAIN, size = base_size),
      legend.text      = element_text(color = TEXT_DIM,  size = base_size - 2),
      legend.title     = element_text(color = TEXT_MAIN, size = base_size - 1),
      legend.background = element_rect(fill = "transparent"),
      legend.key        = element_rect(fill = "transparent"),
      plot.title        = element_text(color = TEXT_MAIN, face = "bold",
                                       size = base_size + 1),
      plot.subtitle     = element_text(color = TEXT_DIM, size = base_size - 1),
      strip.text        = element_text(color = TEXT_MAIN, size = base_size - 1)
    )
}

# plotly dark layout
plotly_dark <- function(p, margin = NULL) {
  m <- if (!is.null(margin)) margin else list(l = 50, r = 20, t = 30, b = 50)
  p %>% layout(
    paper_bgcolor = "transparent",
    plot_bgcolor  = "transparent",
    font   = list(color = TEXT_DIM,
                  family = "Inter, -apple-system, sans-serif"),
    xaxis  = list(gridcolor = GRID_COL, zerolinecolor = GRID_COL),
    yaxis  = list(gridcolor = GRID_COL, zerolinecolor = GRID_COL),
    legend = list(font = list(color = TEXT_DIM)),
    margin = m
  )
}

# Helpers
fmt_num      <- function(x) format(round(x), big.mark = ",")
fmt_pct      <- function(x) paste0(round(x * 100, 1), "%")
fmt_currency <- function(x) paste0("$", format(round(x / 1e9, 1),
                                                big.mark = ","), "B")

# KPI card (compact, balanced)
kpi_box <- function(icon_name, title, output_id, icon_col = ACCENT) {
  tags$div(
    style = paste0(
      "background:", BG_CARD, ";",
      "border:1px solid ", BORDER, ";border-radius:12px;",
      "height:76px;display:flex;align-items:center;",
      "padding:0 14px;gap:10px;overflow:hidden;box-sizing:border-box;",
      "box-shadow:0 4px 24px rgba(0,0,0,0.35);"
    ),
    tags$div(
      style = paste0(
        "width:36px;height:36px;flex-shrink:0;border-radius:10px;",
        "background:", icon_col, "18;",
        "border:1px solid ", icon_col, "30;",
        "display:flex;align-items:center;justify-content:center;"
      ),
      icon(icon_name, style = paste0("color:", icon_col, ";font-size:1rem;"))
    ),
    tags$div(
      style = "min-width:0;",
      tags$div(
        style = paste0(
          "font-size:0.7rem;color:", TEXT_DIM, ";font-weight:600;",
          "text-transform:uppercase;letter-spacing:0.05em;",
          "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;"),
        title
      ),
      tags$div(
        style = paste0(
          "font-size:1.25rem;font-weight:700;color:", TEXT_MAIN,
          ";margin-top:2px;white-space:nowrap;"),
        textOutput(output_id, inline = TRUE)
      )
    )
  )
}

classify_risk <- function(p) {
  if (p < churn_low) "LOW" else if (p < churn_high) "MEDIUM" else "HIGH"
}

build_sim_input <- function(input, X_colnames, X_means) {
  sim <- X_means
  sim["age"]                      <- input$sim_age
  sim["tx_count"]                 <- input$sim_tx_count
  sim["avg_tx_value"]             <- input$sim_avg_value
  sim["satisfaction_score"]       <- input$sim_satisfaction
  sim["credit_utilization_ratio"] <- input$sim_credit_util
  sim["customer_tenure"]          <- input$sim_tenure
  sim["active_products"]          <- input$sim_active_products
  sim["support_tickets_count"]    <- input$sim_support_tickets
  sim["spending_volatility"]      <- input$sim_volatility
  sim["nps_score"]                <- input$sim_nps
  sim["household_size"]           <- input$sim_household
  matrix(sim[X_colnames], nrow = 1, dimnames = list(NULL, X_colnames))
}

# ============================================================
# Custom CSS
# ============================================================
dark_css <- tags$style(HTML(paste0("
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap');

body, .bslib-page-fill {
  background-color: ", BG_DARK, " !important;
  color: ", TEXT_MAIN, " !important;
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif !important;
}

/* NAVBAR */
.navbar, .navbar-default, .navbar.navbar-expand-md {
  background-color: ", BG_SIDEBAR, " !important;
  border-bottom: 1px solid ", BORDER, " !important;
  box-shadow: 0 1px 20px rgba(0,0,0,0.6);
  padding: 0.35rem 1.5rem;
}
.navbar-brand, .navbar .navbar-brand {
  color: ", TEXT_MAIN, " !important; font-weight: 700;
  font-size: 1rem; letter-spacing: -0.02em;
}
.navbar-nav .nav-link {
  color: ", TEXT_DIM, " !important; font-size: 0.8rem; font-weight: 500;
  transition: color 0.2s, border-color 0.2s;
  padding: 0.45rem 0.8rem !important; border-bottom: 2px solid transparent;
}
.navbar-nav .nav-link:hover { color: ", ACCENT, " !important; }
.navbar-nav .nav-link.active,
.navbar-nav .nav-item.active .nav-link,
.navbar-nav .show > .nav-link {
  color: ", ACCENT, " !important; border-bottom: 2px solid ", ACCENT, ";
}

/* SIDEBAR */
.bslib-sidebar-layout > .sidebar, .sidebar {
  background-color: ", BG_SIDEBAR, " !important;
  border-right: 1px solid ", BORDER, " !important;
  color: ", TEXT_DIM, " !important;
}
.sidebar .sidebar-title {
  color: ", TEXT_MAIN, " !important; font-weight: 600;
  font-size: 0.85rem; letter-spacing: -0.01em;
  padding-bottom: 0.3rem !important;
  border-bottom: 1px solid ", BORDER, " !important;
  margin-bottom: 0.3rem !important;
}
.sidebar-content {
  padding: 0.4rem 0.6rem !important;
  display: flex; flex-direction: column; gap: 0;
}
.shiny-input-container { margin-bottom: 5px !important; }
.irs.irs--shiny { margin-top: 2px !important; }

/* MAIN PANEL */
.bslib-sidebar-layout > .main {
  padding: 0.6rem 0.6rem 0.8rem !important;
  gap: 0.5rem !important;
}

/* ACCORDION */
.accordion-item {
  background-color: transparent !important;
  border: none !important;
  border-bottom: 1px solid ", BORDER, " !important;
}
.accordion-button {
  background-color: transparent !important;
  color: ", TEXT_MAIN, " !important;
  font-size: 0.76rem !important; font-weight: 600 !important;
  padding: 0.4rem 0.25rem !important; box-shadow: none !important;
}
.accordion-button:not(.collapsed) { color: ", ACCENT, " !important; }
.accordion-button::after { filter: invert(0.6) !important; }
.accordion-body { padding: 0.3rem 0.25rem !important; }

.bslib-sidebar-layout > .sidebar {
  overflow-y: auto !important; overflow-x: hidden !important;
}
.sidebar .accordion {
  padding-left: 0.2rem !important; padding-right: 0.2rem !important;
}
.sidebar .accordion-button { padding-left: 0.4rem !important; }
.sidebar .accordion-body {
  padding: 0.3rem 0.2rem 0.3rem 0.6rem !important;
}
.sidebar .accordion-body .shiny-input-container { margin-left: 0 !important; }
.sidebar > .shiny-input-container,
.sidebar-content > .shiny-input-container {
  padding-left: 0.4rem !important; box-sizing: border-box;
}
.sidebar > .shiny-input-container label,
.sidebar-content > .shiny-input-container label,
.sidebar .accordion-body label { padding-left: 0 !important; }

/* CARDS */
.card, .bslib-card {
  background-color: ", BG_CARD, " !important;
  border: 1px solid ", BORDER, " !important;
  border-radius: 12px !important;
  box-shadow: 0 4px 24px rgba(0,0,0,0.35) !important;
  color: ", TEXT_MAIN, " !important; overflow: hidden;
}
.card-header {
  background-color: transparent !important;
  border-bottom: 1px solid ", BORDER, " !important;
  color: ", TEXT_MAIN, " !important; font-weight: 600 !important;
  font-size: 0.8rem; padding: 0.45rem 0.9rem !important;
  letter-spacing: -0.01em;
}
.card-body {
  background-color: transparent !important;
  padding: 0.5rem 0.7rem !important;
}

/* INPUTS */
.form-control, .shiny-input-container select,
.selectize-input, .selectize-dropdown {
  background-color: ", BG_INPUT, " !important;
  border: 1px solid ", BORDER, " !important;
  color: ", TEXT_MAIN, " !important; border-radius: 8px !important;
  font-size: 0.78rem;
}
.selectize-dropdown {
  box-shadow: 0 8px 24px rgba(0,0,0,0.6) !important;
}
.selectize-dropdown .option { color: ", TEXT_MAIN, " !important; }
.selectize-dropdown .active {
  background-color: ", ACCENT, "33 !important;
  color: ", ACCENT, " !important;
}
.form-label, label {
  color: ", TEXT_DIM, " !important; font-size: 0.73rem; font-weight: 500;
}
.irs--shiny .irs-bar {
  background: ", ACCENT, " !important; border-color: ", ACCENT, " !important;
}
.irs--shiny .irs-handle { border-color: ", ACCENT, " !important; }
.irs--shiny .irs-line  { background: #2a2a2a !important; }
.irs--shiny .irs-min, .irs--shiny .irs-max,
.irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to {
  background-color: ", ACCENT, " !important;
  color: white !important; font-size: 0.68rem;
}
.btn-primary {
  background: ", ACCENT, " !important; border: none !important;
  border-radius: 8px !important; font-weight: 600 !important;
  font-size: 0.78rem;
  box-shadow: 0 2px 12px rgba(249,115,22,0.3) !important;
  transition: all 0.2s;
}
.btn-primary:hover {
  background: #ea6b0a !important;
  box-shadow: 0 4px 20px rgba(249,115,22,0.45) !important;
  transform: translateY(-1px);
}
.btn-outline-secondary {
  border-color: ", BORDER, " !important; color: ", TEXT_DIM, " !important;
  font-size: 0.72rem !important; padding: 2px 8px !important;
  border-radius: 6px !important;
}
.btn-outline-secondary:hover {
  background: ", BG_INPUT, " !important; color: ", TEXT_MAIN, " !important;
}

/* CHECKBOX / RADIO */
.form-check-input:checked {
  background-color: ", ACCENT, " !important;
  border-color: ", ACCENT, " !important;
}
.form-check-label { color: ", TEXT_DIM, " !important; font-size: 0.76rem; }

/* DT TABLE */
.dataTables_wrapper { color: ", TEXT_DIM, " !important; }
table.dataTable {
  background-color: transparent !important;
  color: ", TEXT_MAIN, " !important; font-size: 0.78rem;
}
table.dataTable thead th {
  background-color: ", BG_INPUT, " !important;
  color: ", ACCENT, " !important;
  border-bottom: 1px solid ", BORDER, " !important;
  font-weight: 600; font-size: 0.73rem;
}
table.dataTable tbody tr { background-color: transparent !important; }
table.dataTable tbody tr:hover { background-color: #1f1f1f !important; }
table.dataTable tbody td { border-color: ", BORDER, " !important; }

/* VERBATIM */
pre, .shiny-text-output, code {
  background-color: ", BG_INPUT, " !important;
  color: #e2e8f0 !important;
  border: 1px solid ", BORDER, " !important;
  border-radius: 8px !important; padding: 8px 12px !important;
  font-family: 'JetBrains Mono','Fira Code','Consolas',monospace !important;
  font-size: 0.76rem !important; line-height: 1.5 !important;
}

/* SCROLLBAR */
::-webkit-scrollbar { width: 5px; }
::-webkit-scrollbar-track { background: ", BG_DARK, "; }
::-webkit-scrollbar-thumb { background: #333333; border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: ", ACCENT, "; }

/* SPINNER */
.shiny-spinner-output-container { background: transparent !important; }

/* DATE INPUT */
.input-daterange .form-control {
  background-color: ", BG_INPUT, " !important;
  color: ", TEXT_MAIN, " !important;
}

/* MISC */
hr { border-color: ", BORDER, " !important; opacity: 0.5; }
h5, h6 { color: ", TEXT_MAIN, " !important; }
small, .text-muted { color: ", TEXT_DIM, " !important; }
.tab-content { background-color: transparent !important; }

/* RECOMMENDATION BOX */
.recommend-box {
  border-radius: 8px; padding: 8px 12px; margin-bottom: 8px;
}
.recommend-box ul {
  margin: 4px 0 0 0; padding-left: 16px;
  font-size: 0.76rem; color: ", TEXT_DIM, "; line-height: 1.65;
}
.recommend-title {
  font-size: 0.78rem; font-weight: 600;
  display: flex; align-items: center; gap: 6px;
}

/* NAVSET TABS inside cards */
.nav-tabs { border-bottom: 1px solid ", BORDER, " !important; }
.nav-tabs .nav-link {
  color: ", TEXT_DIM, " !important; font-size: 0.74rem !important;
  border: none !important; padding: 0.22rem 0.65rem !important;
  background: transparent !important;
}
.nav-tabs .nav-link.active {
  color: ", ACCENT, " !important; background: transparent !important;
  border-bottom: 2px solid ", ACCENT, " !important;
}

/* ggstatsplot / static plot dark background */
.shiny-plot-output { background: transparent !important; }
")))

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  title = tags$span(
    tags$span(style = paste0("color:", ACCENT,
                             ";font-weight:800;letter-spacing:-0.03em;"),
              "COFINFAD"),
    tags$span(style = paste0("font-weight:400;color:", TEXT_DIM, ";"),
              " Analytics")
  ),
  theme = bs_theme(version = 5, bg = BG_DARK, fg = TEXT_MAIN,
                   primary = ACCENT, secondary = ACCENT2,
                   base_font = font_google("Inter")),
  header  = dark_css,
  fillable = FALSE,

  # ===== TAB 1: ABOUT =====
  nav_panel(
    title = "About", icon = icon("info-circle"),

    # Hero Banner
    tags$div(
      style = paste0(
        "background:", BG_CARD, ";",
        "border:1px solid ", BORDER, ";border-radius:16px;",
        "padding:40px 44px 34px;margin-bottom:14px;",
        "position:relative;overflow:hidden;"
      ),
      tags$div(style = paste0(
        "position:absolute;top:0;left:0;right:0;height:2px;",
        "background:linear-gradient(90deg,", ACCENT4, ",",
        ACCENT, ",", ACCENT2, ",transparent);"
      )),
      tags$div(style = paste0(
        "position:absolute;top:-120px;right:-80px;",
        "width:420px;height:420px;border-radius:50%;",
        "background:radial-gradient(circle,", ACCENT2, "20 0%,transparent 65%);",
        "pointer-events:none;filter:blur(50px);"
      )),
      tags$div(
        style = "position:relative;",
        tags$div(
          style = paste0(
            "display:inline-flex;align-items:center;gap:6px;",
            "margin-bottom:18px;font-size:0.68rem;font-weight:700;",
            "letter-spacing:0.1em;text-transform:uppercase;",
            "color:", ACCENT4, ";background:", ACCENT4,
            "18;padding:3px 12px;border-radius:20px;",
            "border:1px solid ", ACCENT4, "35;"
          ),
          "ISSS608 \u00b7 Visual Analytics & Applications"
        ),
        tags$div(
          style = paste0(
            "font-weight:800;font-size:2.2rem;line-height:1.1;",
            "letter-spacing:-0.04em;color:", TEXT_MAIN, ";margin-bottom:14px;"
          ),
          "Democratizing ", tags$span(style = paste0("color:", ACCENT, ";"), "Fintech"),
          tags$span(style = paste0("color:", ACCENT2, ";"), " Analytics")
        ),
        tags$p(
          style = paste0("color:", TEXT_DIM, ";font-size:0.88rem;line-height:1.7;",
                         "max-width:600px;margin:0 0 24px;"),
          "An interactive visual analytics platform that transforms ",
          "high-dimensional financial data from the COFINFAD dataset ",
          "into actionable intelligence for Colombia's fintech sector."
        ),
        tags$div(
          style = "display:flex;gap:36px;flex-wrap:wrap;",
          lapply(list(
            list(val = "48,723", lbl = "Customers",    col = ACCENT),
            list(val = "3.16M",  lbl = "Transactions", col = ACCENT2),
            list(val = "54",     lbl = "Variables",    col = ACCENT4),
            list(val = "5",      lbl = "Modules",      col = ACCENT3)
          ), function(item) {
            tags$div(
              style = paste0("border-left:2px solid ", item$col, "55;padding-left:12px;"),
              tags$div(style = paste0("font-size:1.6rem;font-weight:800;color:", TEXT_MAIN,
                                     ";letter-spacing:-0.04em;line-height:1;"), item$val),
              tags$div(style = paste0("font-size:0.66rem;color:", TEXT_DIM,
                                     ";text-transform:uppercase;letter-spacing:0.08em;",
                                     "margin-top:4px;"), item$lbl)
            )
          })
        )
      )
    ),

    # Module Cards (horizontal row)
    tags$div(
      style = "display:flex;gap:10px;flex-wrap:nowrap;margin-bottom:0;",
      {
        items <- list(
          list(icon = "chart-line",   col = ACCENT,    title = "Exploratory Overview",
               desc = "Time series, choropleth, distributions"),
          list(icon = "flask",        col = ACCENT4,   title = "Confirmatory Analysis",
               desc = "ggstatsplot tests, correlation matrix"),
          list(icon = "object-group", col = ACCENT2,   title = "Segmentation",
               desc = "K-Means, Hierarchical, PAM"),
          list(icon = "chart-simple", col = ACCENT3,   title = "Model Calibration",
               desc = "CV diagnostics, variable importance"),
          list(icon = "sliders",      col = "#38bdf8", title = "What-If Simulator",
               desc = "Real-time churn prediction")
        )
        tagList(lapply(items, function(item) {
          tags$div(
            style = paste0(
              "flex:1;background:", BG_CARD, ";border:1px solid ", BORDER,
              ";border-radius:12px;padding:14px 12px;position:relative;overflow:hidden;"
            ),
            tags$div(style = paste0(
              "position:absolute;top:0;left:0;right:0;height:2px;background:", item$col, ";"
            )),
            tags$div(
              style = paste0("width:28px;height:28px;border-radius:7px;background:",
                             item$col, "18;border:1px solid ", item$col,
                             "30;display:flex;align-items:center;justify-content:center;",
                             "margin-bottom:8px;"),
              icon(item$icon, style = paste0("color:", item$col, ";font-size:0.85rem;"))
            ),
            tags$div(style = paste0("font-weight:700;color:", TEXT_MAIN,
                                    ";font-size:0.78rem;margin-bottom:3px;"), item$title),
            tags$div(style = paste0("color:", TEXT_DIM, ";font-size:0.7rem;line-height:1.45;"),
                     item$desc)
          )
        }))
      }
    ),

    tags$div(style = "height:10px;"),

    # Dataset + Team
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header(tags$div(style = "display:flex;align-items:center;gap:8px;",
          icon("database", style = paste0("color:", ACCENT4, ";")),
          tags$span("Dataset \u2014 COFINFAD"))),
        card_body(
          tags$p(style = paste0("color:", TEXT_DIM, ";font-size:0.78rem;line-height:1.65;margin-bottom:10px;"),
            "The ", tags$strong(style = paste0("color:", TEXT_MAIN), "COFINFAD"),
            " dataset covers retail banking customers in Colombia, combining CRM attributes, ",
            "transaction history, and customer lifecycle indicators."),
          lapply(list(
            list(icon = "users", col = ACCENT, lbl = "Customer Records", val = "48,723"),
            list(icon = "receipt", col = ACCENT2, lbl = "Transaction Events", val = "3,159,157"),
            list(icon = "table-list", col = ACCENT4, lbl = "Feature Dimensions", val = "54 variables"),
            list(icon = "calendar", col = ACCENT3, lbl = "Observation Period", val = "12 months")
          ), function(r) {
            tags$div(
              style = paste0("display:flex;align-items:center;gap:10px;padding:6px 0;border-bottom:1px solid ", BORDER, ";"),
              tags$div(style = paste0("width:28px;height:28px;border-radius:7px;flex-shrink:0;background:", r$col,
                "18;border:1px solid ", r$col, "28;display:flex;align-items:center;justify-content:center;"),
                icon(r$icon, style = paste0("color:", r$col, ";font-size:0.75rem;"))),
              tags$div(style = "flex:1;",
                tags$div(style = paste0("font-size:0.7rem;color:", TEXT_DIM, ";"), r$lbl),
                tags$div(style = paste0("font-size:0.82rem;font-weight:600;color:", TEXT_MAIN, ";"), r$val))
            )
          })
        )
      ),
      card(
        card_header(tags$div(style = "display:flex;align-items:center;gap:8px;",
          icon("people-group", style = paste0("color:", ACCENT2, ";")),
          tags$span("Team 13"))),
        card_body({
          member <- function(initials, name, role, desc, col) {
            tags$div(
              style = paste0("display:flex;align-items:center;gap:10px;padding:8px 12px;background:", BG_INPUT,
                ";border-radius:9px;border:1px solid ", BORDER, ";margin-bottom:6px;"),
              tags$div(style = paste0("width:32px;height:32px;border-radius:8px;flex-shrink:0;background:", col,
                "25;border:1px solid ", col, "40;display:flex;align-items:center;justify-content:center;",
                "font-weight:700;font-size:0.78rem;color:", col, ";"), initials),
              tags$div(style = "flex:1;min-width:0;",
                tags$div(style = "display:flex;justify-content:space-between;align-items:center;",
                  tags$span(style = paste0("font-weight:700;color:", TEXT_MAIN, ";font-size:0.82rem;"), name),
                  tags$span(style = paste0("font-size:0.64rem;font-weight:600;padding:1px 7px;border-radius:4px;background:", col,
                    "18;color:", col, ";border:1px solid ", col, "28;"), role)),
                tags$div(style = paste0("font-size:0.73rem;color:", TEXT_DIM, ";"), desc))
            )
          }
          tagList(
            member("JG", "Ji Guofang", "Clustering", "Customer segmentation, PCA, cluster profiling.", ACCENT2),
            member("LY", "Lin Yan", "EDA", "Exploratory & confirmatory analysis, statistical testing.", ACCENT),
            member("NN", "Nguyen Trong Nhan", "Prediction", "Regularized regression, What-If simulator.", ACCENT3),
            tags$div(style = paste0("display:flex;align-items:center;justify-content:space-between;padding:8px 12px;",
              "background:", BG_DARK, ";border-radius:9px;border:1px solid ", BORDER, ";margin-top:4px;"),
              tags$div(
                tags$div(style = paste0("font-size:0.74rem;font-weight:600;color:", TEXT_MAIN, ";"),
                  "ISSS608 Visual Analytics & Applications"),
                tags$div(style = paste0("font-size:0.68rem;color:", TEXT_DIM, ";margin-top:1px;"),
                  "Singapore Management University \u00b7 AY2025\u20132026 April Term")),
              icon("graduation-cap", style = paste0("color:", ACCENT2, ";font-size:0.9rem;"))
            )
          )
        })
      )
    )
  ),

  # ===== TAB 2: EXPLORATORY OVERVIEW (EDA) =====
  nav_panel(
    title = "Exploratory Overview", icon = icon("chart-line"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "EDA Filters", width = 250,
        dateRangeInput("eda_date_range", "Date Range",
                       start = min(monthly_total$month),
                       end   = max(monthly_total$month),
                       min   = min(monthly_total$month),
                       max   = max(monthly_total$month)),
        checkboxGroupInput("eda_tx_types", "Transaction Types",
                           choices  = c("Transfer","Withdrawal","Payment","Deposit"),
                           selected = c("Transfer","Withdrawal","Payment","Deposit")),
        selectInput("eda_map_metric", "Map Metric",
                    choices = c("Customer Count" = "customer_count",
                                "Avg Churn Probability" = "avg_churn_prob",
                                "Avg Income Level" = "avg_income",
                                "Avg CLV" = "avg_clv",
                                "Avg Satisfaction" = "avg_satisfaction")),
        selectInput("eda_group_var", "Grouping Variable",
                    choices = c("income_bracket", "customer_segment",
                                "clv_segment", "gender", "age_group",
                                "acquisition_channel", "feedback_sentiment")),
        actionButton("eda_reset", "Reset Filters",
                     class = "btn-outline-secondary w-100 mt-1",
                     icon  = icon("rotate-left"))
      ),

      # KPI Row
      tags$div(
        style = "height:76px;margin-bottom:0.4rem;",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          kpi_box("users",                "Total Customers",   "kpi_customers", ACCENT),
          kpi_box("coins",                "Transaction Volume", "kpi_volume",   ACCENT),
          kpi_box("triangle-exclamation", "Avg Churn Rate",    "kpi_churn",     ACCENT3),
          kpi_box("star",                 "Avg Satisfaction",  "kpi_sat",       ACCENT2)
        )
      ),

      # Row 1: Time series + Choropleth
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Monthly Transaction Volume by Type"),
          card_body(withSpinner(plotlyOutput("eda_ts", height = "250px"), type = 8, color = ACCENT))
        ),
        card(
          card_header("Colombian Department Overview"),
          card_body(
            if (has_spatial) {
              withSpinner(leafletOutput("eda_map", height = "250px"), type = 8, color = ACCENT)
            } else {
              withSpinner(plotlyOutput("eda_map_bar", height = "250px"), type = 8, color = ACCENT)
            }
          )
        )
      ),

      # Row 2: Customer Distribution + Segment Demographics
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Customer Distribution"),
          card_body(withSpinner(plotlyOutput("eda_dist", height = "260px"), type = 8, color = ACCENT))
        ),
        card(
          card_header("Segment Composition by Income Bracket"),
          card_body(withSpinner(plotlyOutput("eda_demo", height = "260px"), type = 8, color = ACCENT))
        )
      )
    )
  ),

  # ===== TAB 3: CONFIRMATORY ANALYSIS (CDA) =====
  nav_panel(
    title = "Confirmatory Analysis", icon = icon("flask"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Statistical Testing", width = 250,
        selectInput("cda_group_var", "Grouping Variable",
                    choices = c("income_bracket", "customer_segment",
                                "clv_segment", "gender", "age_group",
                                "acquisition_channel", "feedback_sentiment")),
        selectInput("cda_test_var", "Test Variable",
                    choices = c("avg_tx_value", "total_tx_volume",
                                "tx_count", "satisfaction_score",
                                "churn_probability",
                                "credit_utilization_ratio",
                                "customer_tenure")),
        radioButtons("cda_test_type", "Test Approach",
                     choices = c("Parametric"     = "parametric",
                                 "Non-parametric" = "nonparametric",
                                 "Robust"         = "robust",
                                 "Bayesian"       = "bayes"),
                     selected = "parametric"),
        selectInput("cda_stat_test", "Comparison Type",
                    choices = c("Between Groups (ANOVA)" = "anova",
                                "Two-Group (T-test)" = "ttest")),
        conditionalPanel(
          condition = "input.cda_stat_test == 'ttest'",
          selectInput("cda_group1", "Group 1", choices = NULL),
          selectInput("cda_group2", "Group 2", choices = NULL)
        ),
        tags$hr(),
        tags$div(
          style = paste0("font-size:0.72rem;font-weight:600;color:", TEXT_MAIN,
                         ";margin-bottom:4px;"),
          "Correlation Matrix"),
        radioButtons("cda_cor_type", "Correlation Method",
                     choices = c("Parametric"     = "parametric",
                                 "Non-parametric" = "nonparametric",
                                 "Robust"         = "robust",
                                 "Bayesian"       = "bayes"),
                     selected = "parametric")
      ),

      # Side by side: ggstatsplot + Correlation matrix
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Statistical Hypothesis Testing (ggstatsplot)"),
          card_body(withSpinner(
            plotOutput("cda_ggstatsplot", height = "580px"),
            type = 8, color = ACCENT))
        ),
        card(
          card_header("Correlation Matrix (ggcorrmat \u2014 hclust reordered)"),
          card_body(withSpinner(
            plotOutput("cda_corr", height = "580px"),
            type = 8, color = ACCENT))
        )
      )
    )
  ),

  # ===== TAB 4: CUSTOMER SEGMENTATION =====
  nav_panel(
    title = "Customer Segmentation", icon = icon("object-group"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Clustering Controls", width = 250,
        selectInput("cluster_preset", "Feature Preset",
                    choices = c("RFM Features" = "rfm",
                                "Full Behavioral" = "behavioral",
                                "Engagement" = "engagement",
                                "Custom" = "custom")),
        sliderInput("cluster_k", "Number of Clusters (k)",
                    min = 2, max = 8, value = 4),
        radioButtons("cluster_method", "Algorithm",
                     choices = c("K-Means" = "kmeans",
                                 "Hierarchical (Ward)" = "hclust",
                                 "PAM (Medoids)" = "pam")),
        selectInput("cluster_color", "Color Points By",
                    choices = c("Cluster"          = "cluster",
                                "Income Bracket"   = "income_bracket",
                                "Customer Segment" = "customer_segment",
                                "CLV Tier"         = "clv_segment",
                                "Churn Risk"       = "churn_risk")),
        conditionalPanel(
          condition = "input.cluster_preset == 'custom'",
          tags$hr(),
          tags$label(style = paste0("color:", TEXT_DIM, ";font-size:0.73rem;font-weight:500;"),
                     "Clustering Features"),
          tags$div(
            style = "display:flex;gap:4px;margin-bottom:4px;",
            actionButton("cluster_sel_all",  "Select All",
                         class = "btn-outline-secondary btn-sm flex-fill"),
            actionButton("cluster_sel_none", "Clear",
                         class = "btn-outline-secondary btn-sm flex-fill")
          ),
          checkboxGroupInput("cluster_vars", NULL,
            choices = ALL_CLUSTER_VARS,
            selected = c("tx_count", "avg_tx_value", "customer_tenure",
                         "spending_volatility", "satisfaction_score")),
          actionButton("cluster_go", "Run Clustering",
                       class = "btn-primary w-100 mt-1", icon = icon("play"))
        ),
        uiOutput("cl_size_warning"),
        downloadButton("cl_download", "Export Centroids CSV",
                       class = "btn-outline-secondary w-100 mt-2",
                       style = "font-size:0.72rem;")
      ),

      # Row 1: PCA + Diagnostics
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("PCA Cluster Visualization"),
          card_body(withSpinner(plotlyOutput("cl_pca", height = "290px"), type = 8, color = ACCENT))
        ),
        card(
          card_header("Cluster Quality Diagnostics"),
          card_body(navset_tab(
            nav_panel("Elbow", withSpinner(plotlyOutput("cl_elbow", height = "175px"), type = 8, color = ACCENT)),
            nav_panel("Silhouette", withSpinner(plotlyOutput("cl_silhouette", height = "175px"), type = 8, color = ACCENT))
          ))
        )
      ),

      # Row 2: Heatmap + Parallel Coordinates / Dendrogram
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Cluster Feature Heatmap (Normalized)"),
          card_body(withSpinner(plotlyOutput("cl_heatmap", height = "280px"), type = 8, color = ACCENT))
        ),
        card(
          card_header("Cluster Visualization"),
          card_body(navset_tab(
            nav_panel("Parallel Coordinates",
              withSpinner(plotlyOutput("cl_parcoord", height = "210px"), type = 8, color = ACCENT)),
            nav_panel("Dendrogram",
              withSpinner(plotOutput("cl_dendro", height = "210px"), type = 8, color = ACCENT)),
            nav_panel("Income Dist.",
              withSpinner(plotlyOutput("cl_demo", height = "210px"), type = 8, color = ACCENT))
          ))
        )
      )
    )
  ),

  # ===== TAB 5: MODEL CALIBRATION =====
  nav_panel(
    title = "Model Calibration", icon = icon("chart-simple"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Model Controls", width = 250,
        radioButtons("cal_type", "Model Type",
                     choices = c("Lasso (L1)" = "lasso", "Ridge (L2)" = "ridge"),
                     inline = TRUE),
        sliderInput("cal_lambda", "Lambda Index (\u03bb)", 1, 100, 50),
        tags$small(style = paste0("color:", TEXT_DIM, ";font-size:0.68rem;display:block;"),
          "Auto-set to \u03bb.min (optimal). Adjust to explore regularization trade-offs.")
      ),

      # KPI Row
      tags$div(
        style = "height:76px;margin-bottom:0.4rem;",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          kpi_box("chart-simple",  "Model R\u00b2",      "cal_rsq",  ACCENT2),
          kpi_box("bullseye",      "RMSE",               "cal_rmse", ACCENT3),
          kpi_box("list-ol",       "Active Features",    "cal_nfeat", ACCENT),
          kpi_box("gauge-high",    "Churn Probability",  "cal_churn", ACCENT4)
        )
      ),

      # Lambda interpretation
      uiOutput("cal_lambda_info"),

      # Row 1: Variable Importance + CV Performance
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Top Churn Predictors (Variable Importance)"),
          card_body(withSpinner(plotlyOutput("cal_imp", height = "260px"), type = 8, color = ACCENT))
        ),
        card(
          card_header("Cross-Validation Performance (\u03bb vs MSE)"),
          card_body(withSpinner(plotlyOutput("cal_cv", height = "260px"), type = 8, color = ACCENT))
        )
      ),

      # Row 2: Predicted vs Actual + Residuals
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Model Calibration: Predicted vs Actual"),
          card_body(withSpinner(plotlyOutput("cal_scatter", height = "240px"), type = 8, color = ACCENT))
        ),
        card(
          card_header("Residual Distribution"),
          card_body(withSpinner(plotlyOutput("cal_resid", height = "240px"), type = 8, color = ACCENT))
        )
      )
    )
  ),

  # ===== TAB 6: WHAT-IF SIMULATOR =====
  nav_panel(
    title = "What-If Simulator", icon = icon("sliders"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Simulator Controls", width = 250, open = TRUE,
        radioButtons("sim_type", "Model Type",
                     choices = c("Lasso (L1)" = "lasso", "Ridge (L2)" = "ridge"),
                     inline = TRUE),
        sliderInput("sim_lambda", "Lambda Index (\u03bb)", 1, 100, 50),
        tags$hr(),
        actionButton("sim_reset", "Reset to Defaults",
                     class = "btn-outline-secondary w-100 mb-1", icon = icon("rotate-left")),
        sliderInput("sim_age",             "Age",                29, 60, 45),
        sliderInput("sim_tx_count",        "Transaction Count",  1, 200, 50),
        sliderInput("sim_avg_value",       "Avg Tx Value",       1e5, 2e7, 2e6, step = 2e5),
        sliderInput("sim_satisfaction",    "Satisfaction",        1, 10, 5, 0.5),
        sliderInput("sim_nps",             "NPS Score",          -100, 100, -20),
        sliderInput("sim_credit_util",     "Credit Utilization", 0, 1, 0.3, 0.05),
        sliderInput("sim_tenure",          "Tenure (months)",    1, 24, 10),
        sliderInput("sim_active_products", "Active Products",    0, 10, 3),
        sliderInput("sim_support_tickets", "Support Tickets",    0, 20, 2),
        sliderInput("sim_volatility",      "Spending Volatility", 0, 1e7, 1e6, step = 2e5),
        sliderInput("sim_household",       "Household Size",     1, 10, 3)
      ),

      # KPI Row
      tags$div(
        style = "height:76px;margin-bottom:0.4rem;",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          kpi_box("gauge-high",    "Churn Probability",  "sim_churn",  ACCENT),
          kpi_box("shield-halved", "Risk Level",         "sim_risk",   ACCENT4),
          kpi_box("chart-simple",  "Model R\u00b2",      "sim_rsq",   ACCENT2),
          kpi_box("bullseye",      "RMSE",               "sim_rmse",  ACCENT3)
        )
      ),

      # Recommendations
      uiOutput("sim_recommend"),

      # Row 1: Prediction Contribution + Sensitivity Analysis
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Prediction Breakdown \u2014 What drives this score?"),
          card_body(withSpinner(plotlyOutput("sim_waterfall", height = "320px"), type = 8, color = ACCENT))
        ),
        card(
          card_header("Sensitivity Analysis \u2014 Which levers matter most?"),
          card_body(withSpinner(plotlyOutput("sim_tornado", height = "320px"), type = 8, color = ACCENT))
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ===== TAB 2: EXPLORATORY OVERVIEW =====

  observeEvent(input$eda_reset, {
    updateDateRangeInput(session, "eda_date_range",
                         start = min(monthly_total$month), end = max(monthly_total$month))
    updateCheckboxGroupInput(session, "eda_tx_types",
                             selected = c("Transfer", "Withdrawal", "Payment", "Deposit"))
  })

  # KPIs
  output$kpi_customers <- renderText(fmt_num(nrow(customers)))
  output$kpi_volume <- renderText({
    d <- monthly_summary %>%
      filter(type %in% input$eda_tx_types,
             month >= input$eda_date_range[1], month <= input$eda_date_range[2])
    fmt_currency(sum(d$total_amount, na.rm = TRUE))
  })
  output$kpi_churn <- renderText(fmt_pct(mean(customers$churn_probability, na.rm = TRUE)))
  output$kpi_sat <- renderText(paste0(round(mean(customers$satisfaction_score, na.rm = TRUE), 1), " / 10"))

  # Time series
  output$eda_ts <- renderPlotly({
    d <- monthly_summary %>%
      filter(type %in% input$eda_tx_types,
             month >= input$eda_date_range[1], month <= input$eda_date_range[2])
    req(nrow(d) > 0)
    p <- ggplot(d, aes(x = month, y = total_amount / 1e9, color = type, group = type)) +
      geom_line(linewidth = 1) + geom_point(size = 2, alpha = 0.9) +
      scale_color_manual(values = type_colors) +
      scale_y_continuous(labels = label_comma(suffix = "B")) +
      labs(x = NULL, y = "Volume (Billions COP)", color = NULL) + theme_dark_dash()
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      plotly_dark(margin = list(l = 50, r = 20, t = 15, b = 55)) %>%
      layout(legend = list(orientation = "h", y = -0.25, x = 0.5, xanchor = "center"))
  })

  # Choropleth
  if (has_spatial) {
    output$eda_map <- renderLeaflet({
      req(dept_sf)
      metric <- input$eda_map_metric; vals <- dept_sf[[metric]]
      pal <- colorNumeric("YlOrRd", domain = vals, na.color = "#333333")
      labels <- sprintf("<strong>%s</strong><br/>%s: %s",
        dept_sf$dept_match, gsub("_", " ", metric), round(vals, 2)) %>% lapply(htmltools::HTML)
      leaflet(dept_sf) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
        addPolygons(fillColor = ~pal(vals), fillOpacity = 0.7, color = "#444", weight = 1,
          label = labels, highlightOptions = highlightOptions(weight = 2, color = ACCENT, fillOpacity = 0.9)) %>%
        addLegend("bottomright", pal = pal, values = vals, title = gsub("_", " ", metric), opacity = 0.8)
    })
  } else {
    output$eda_map_bar <- renderPlotly({
      metric <- input$eda_map_metric
      d <- dept_stats %>% arrange(desc(!!sym(metric)))
      p <- ggplot(d, aes(x = reorder(department, !!sym(metric)), y = !!sym(metric),
                          text = paste0(department, ": ", round(!!sym(metric), 2)))) +
        geom_col(fill = ACCENT, alpha = 0.8, width = 0.7) + coord_flip() +
        labs(x = NULL, y = gsub("_", " ", metric)) + theme_dark_dash()
      ggplotly(p, tooltip = "text") %>% plotly_dark()
    })
  }

  # Customer Distribution
  output$eda_dist <- renderPlotly({
    grp <- input$eda_group_var
    d <- customers %>% count(!!sym(grp)) %>% arrange(desc(n)) %>% mutate(pct = n / sum(n))
    p <- ggplot(d, aes(x = reorder(!!sym(grp), n), y = n,
      text = paste0(!!sym(grp), ": ", fmt_num(n), " (", round(pct * 100, 1), "%)"))) +
      geom_col(fill = ACCENT, alpha = 0.8, width = 0.7) + coord_flip() +
      scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = "Customer Count") + theme_dark_dash()
    ggplotly(p, tooltip = "text") %>% plotly_dark()
  })

  # Segment Demographics
  output$eda_demo <- renderPlotly({
    d <- customers %>% count(income_bracket, customer_segment) %>%
      group_by(income_bracket) %>% mutate(pct = n / sum(n))
    p <- ggplot(d, aes(x = income_bracket, y = n, fill = customer_segment,
      text = paste0(customer_segment, ": ", fmt_num(n), " (", round(pct * 100, 1), "%)"))) +
      geom_col(position = "fill", alpha = 0.85, width = 0.65) +
      scale_fill_manual(values = segment_colors) + scale_y_continuous(labels = label_percent()) +
      labs(x = NULL, y = "Proportion", fill = "Segment") + theme_dark_dash()
    ggplotly(p, tooltip = "text") %>%
      plotly_dark(margin = list(l = 50, r = 20, t = 15, b = 60)) %>%
      layout(legend = list(orientation = "h", y = -0.25, x = 0.5, xanchor = "center"))
  })

  # ===== TAB 3: CONFIRMATORY ANALYSIS =====

  # Update T-test group selectors
  observeEvent(input$cda_group_var, {
    grp_col <- customers[[input$cda_group_var]]
    lvls <- if (is.factor(grp_col)) levels(grp_col) else sort(unique(as.character(grp_col)))
    updateSelectInput(session, "cda_group1", choices = lvls, selected = lvls[1])
    updateSelectInput(session, "cda_group2", choices = lvls, selected = lvls[min(2, length(lvls))])
  })

  # ggstatsplot: ggbetweenstats
  output$cda_ggstatsplot <- renderPlot({
    grp <- input$cda_group_var; var <- input$cda_test_var; test_type <- input$cda_test_type
    d <- customers %>% select(group = !!sym(grp), value = !!sym(var)) %>% drop_na()
    d$group <- as.factor(d$group)

    # Sample for performance
    if (nrow(d) > 5000) { set.seed(42); d <- d[sample(nrow(d), 5000), ] }

    if (input$cda_stat_test == "ttest") {
      req(input$cda_group1, input$cda_group2)
      validate(need(input$cda_group1 != input$cda_group2, "Select two different groups."))
      d <- d %>% filter(group %in% c(input$cda_group1, input$cda_group2))
      d$group <- droplevels(d$group)
      req(nlevels(d$group) == 2)
    }

    p <- ggbetweenstats(
      data = d, x = group, y = value,
      type = test_type,
      pairwise_comparisons = TRUE,
      pairwise_display = if (input$cda_stat_test == "ttest") "all" else "significant",
      p_adjust_method = "holm",
      bf_message = (test_type == "bayes"),
      title = paste0(gsub("_", " ", var), " by ", gsub("_", " ", grp),
                     " (", tools::toTitleCase(test_type), ")"),
      xlab = gsub("_", " ", grp),
      ylab = gsub("_", " ", var),
      ggtheme = theme_dark_dash(base_size = 12),
      package = "ggsci", palette = "default_jco"
    )

    p + theme(
      plot.background = element_rect(fill = BG_CARD, color = NA),
      panel.background = element_rect(fill = BG_CARD, color = NA),
      plot.title = element_text(color = TEXT_MAIN, size = 12, face = "bold"),
      plot.subtitle = element_text(color = TEXT_DIM, size = 9),
      plot.caption = element_text(color = TEXT_DIM, size = 8),
      axis.text = element_text(color = TEXT_DIM),
      axis.title = element_text(color = TEXT_MAIN),
      axis.text.x = element_text(angle = 20, hjust = 1, size = 9)
    )
  }, bg = BG_CARD, execOnResize = TRUE)

  # Correlation matrix: ggcorrmat (ggstatsplot) with hclust reorder
  output$cda_corr <- renderPlot({
    cor_data <- customers %>%
      select(age, tx_count, avg_tx_value, total_tx_volume,
             satisfaction_score, churn_probability,
             credit_utilization_ratio, customer_tenure,
             active_products, nps_score, weekend_transaction_ratio,
             support_tickets_count)

    # Sample for speed
    if (nrow(cor_data) > 5000) { set.seed(42); cor_data <- cor_data[sample(nrow(cor_data), 5000), ] }

    p <- ggcorrmat(
      data = cor_data,
      cor_vars = everything(),
      type = input$cda_cor_type,
      ggcorrplot.args = list(
        outline.color = BORDER,
        hc.order = TRUE,
        tl.cex = 9,
        lab_size = 3
      ),
      title = "Correlation Matrix (hclust reordered)",
      subtitle = paste0(tools::toTitleCase(input$cda_cor_type), " correlation coefficients"),
      ggtheme = theme_dark_dash(base_size = 11)
    )

    p + theme(
      plot.background = element_rect(fill = BG_CARD, color = NA),
      panel.background = element_rect(fill = BG_CARD, color = NA),
      plot.title = element_text(color = TEXT_MAIN, size = 12, face = "bold"),
      plot.subtitle = element_text(color = TEXT_DIM, size = 9),
      plot.caption = element_text(color = TEXT_DIM, size = 8),
      axis.text = element_text(color = TEXT_DIM, size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  }, bg = BG_CARD, execOnResize = TRUE)

  # ===== TAB 4: CLUSTERING =====

  observeEvent(input$cluster_sel_all, {
    updateCheckboxGroupInput(session, "cluster_vars", selected = ALL_CLUSTER_VARS)
  })
  observeEvent(input$cluster_sel_none, {
    updateCheckboxGroupInput(session, "cluster_vars", selected = character(0))
  })

  merge_tiny_clusters <- function(labels, sc, min_pct = 0.01) {
    n <- length(labels); tab <- table(labels)
    tiny <- as.integer(names(tab)[tab < n * min_pct])
    if (length(tiny) == 0) return(labels)
    big <- as.integer(names(tab)[tab >= n * min_pct])
    if (length(big) == 0) return(labels)
    cen <- sapply(big, function(i) colMeans(sc[labels == i, , drop = FALSE]))
    for (ti in tiny) {
      pts <- which(labels == ti)
      cent_ti <- colMeans(sc[pts, , drop = FALSE])
      nearest <- big[which.min(colSums((cen - cent_ti)^2))]
      labels[pts] <- nearest
    }
    as.integer(factor(labels, levels = sort(unique(labels))))
  }

  cl_precomputed <- reactive({
    preset_id <- input$cluster_preset; req(preset_id != "custom")
    method <- input$cluster_method; k_val <- input$cluster_k
    pc <- cluster_pre$presets[[preset_id]]; md <- pc$methods[[method]]
    k_col <- paste0("k", k_val)
    raw_labels <- md$labels[, k_col]
    raw <- cluster_pre$raw_features
    sc  <- scale(as.matrix(raw[, pc$features]))
    labels <- merge_tiny_clusters(raw_labels, sc)
    demo <- cluster_pre$demographics

    pca_df <- pc$pca_scores
    pca_df$cluster <- factor(labels)
    pca_df$income_bracket <- demo$income_bracket
    pca_df$customer_segment <- demo$customer_segment
    pca_df$clv_segment <- demo$clv_segment
    pca_df$churn_risk <- demo$churn_risk

    feat_df <- raw %>% select(all_of(pc$features))
    feat_df$cluster <- factor(labels)
    cm <- feat_df %>% group_by(cluster) %>%
      summarise(across(everything(), mean), count = n(), .groups = "drop")

    list(pc = pca_df, cm = cm, wss = md$wss, sil = md$sil, ve = pc$var_explained,
         vars = pc$features, sc = sc, labels = labels,
         df = data.frame(cluster = factor(labels), income_bracket = demo$income_bracket))
  })

  cl_custom <- eventReactive(input$cluster_go, {
    vars <- input$cluster_vars
    validate(need(length(vars) >= 2, "Select at least 2 features."))
    withProgress(message = "Running clustering...", value = 0.05, {
      raw <- cluster_pre$raw_features; demo <- cluster_pre$demographics
      feat <- as.matrix(raw[, vars]); sc <- scale(feat); incProgress(0.1)
      pca <- prcomp(sc, center = FALSE, scale. = FALSE)
      n_pcs <- min(2, ncol(sc))
      pca_df <- as.data.frame(pca$x[, 1:n_pcs])
      if (ncol(pca_df) == 1) pca_df$PC2 <- 0
      colnames(pca_df) <- c("PC1", "PC2")
      ve <- summary(pca)$importance[2, 1:n_pcs]; incProgress(0.1)
      k <- input$cluster_k; method <- input$cluster_method; n <- nrow(sc)
      if (method == "kmeans") {
        set.seed(42); cl <- kmeans(sc, k, nstart = 25, iter.max = 100)$cluster
      } else if (method == "hclust") {
        if (n > 10000) {
          set.seed(42); idx <- sample(n, 10000)
          hc <- hclust(dist(sc[idx, ]), method = "ward.D2"); hc_cut <- cutree(hc, k)
          cen <- sapply(1:k, function(i) colMeans(sc[idx, ][hc_cut == i, , drop = FALSE]))
          cl <- apply(sc, 1, function(r) which.min(colSums((cen - r)^2)))
        } else { cl <- cutree(hclust(dist(sc), method = "ward.D2"), k) }
      } else {
        set.seed(42); cl <- clara(sc, k, metric = "euclidean", samples = 50, sampsize = min(500, n))$clustering
      }
      incProgress(0.2); cl <- merge_tiny_clusters(cl, sc)
      pca_df$cluster <- factor(cl); pca_df$income_bracket <- demo$income_bracket
      pca_df$customer_segment <- demo$customer_segment; pca_df$clv_segment <- demo$clv_segment
      pca_df$churn_risk <- demo$churn_risk
      feat_df <- as.data.frame(raw[, vars]); feat_df$cluster <- factor(cl)
      cm <- feat_df %>% group_by(cluster) %>%
        summarise(across(everything(), mean), count = n(), .groups = "drop")
      incProgress(0.1)
      set.seed(42); ed <- if (n > 10000) sc[sample(n, 10000), ] else sc
      wss <- sapply(2:8, function(kk) kmeans(ed, kk, nstart = 10, iter.max = 50)$tot.withinss)
      incProgress(0.2)
      sil_sc <- if (nrow(ed) > 3000) ed[sample(nrow(ed), 3000), ] else ed
      sil_scores <- sapply(2:8, function(kk) {
        set.seed(42); km <- kmeans(sil_sc, kk, nstart = 10, iter.max = 50)
        mean(silhouette(km$cluster, dist(sil_sc))[, 3])
      })
      incProgress(0.2)
      list(pc = pca_df, cm = cm, wss = wss, sil = sil_scores, ve = ve,
           vars = vars, sc = sc, labels = cl,
           df = data.frame(cluster = factor(cl), income_bracket = demo$income_bracket))
    })
  })

  cl_result <- reactive({
    if (input$cluster_preset == "custom") cl_custom() else cl_precomputed()
  })

  # PCA scatter
  output$cl_pca <- renderPlotly({
    r <- cl_result(); req(r); cv <- input$cluster_color; d <- r$pc
    trim_iqr <- function(x) { q <- quantile(x, c(0.02, 0.98), na.rm = TRUE); x >= q[1] & x <= q[2] }
    keep <- trim_iqr(d$PC1) & trim_iqr(d$PC2); d <- d[keep, ]
    if (nrow(d) > 6000) { set.seed(42); d <- d[sample(nrow(d), 6000), ] }
    p <- ggplot(d, aes(x = PC1, y = PC2, color = !!sym(cv), text = paste("Cluster:", cluster))) +
      geom_point(alpha = 0.35, size = 1.2) +
      labs(x = paste0("PC1 (", round(r$ve[1] * 100, 1), "%)"),
           y = paste0("PC2 (", round(r$ve[2] * 100, 1), "%)"),
           color = gsub("_", " ", cv)) + theme_dark_dash()
    if (cv == "cluster") p <- p + scale_color_manual(values = cluster_pal)
    else if (cv == "churn_risk") p <- p + scale_color_manual(values = churn_colors)
    else if (cv == "income_bracket") p <- p + scale_color_manual(values = income_colors)
    else if (cv == "customer_segment") p <- p + scale_color_manual(values = segment_colors)
    ggplotly(p, tooltip = c("text", "x", "y")) %>% plotly_dark()
  })

  # Elbow
  output$cl_elbow <- renderPlotly({
    r <- cl_result(); req(r); actual_k <- length(unique(r$cm$cluster))
    d <- data.frame(k = 2:8, wss = r$wss)
    p <- ggplot(d, aes(x = k, y = wss)) +
      geom_line(color = ACCENT, linewidth = 1) + geom_point(color = ACCENT, size = 2.5) +
      geom_vline(xintercept = actual_k, color = ACCENT3, linetype = "dashed", linewidth = 0.7) +
      annotate("text", x = actual_k + 0.3, y = max(r$wss) * 0.93,
               label = paste0("k=", actual_k), color = ACCENT3, size = 3, fontface = "bold") +
      scale_x_continuous(breaks = 2:8) +
      labs(x = "k", y = "Within-Cluster SS") + theme_dark_dash(base_size = 10)
    ggplotly(p, tooltip = c("x", "y")) %>% plotly_dark()
  })

  # Silhouette
  output$cl_silhouette <- renderPlotly({
    r <- cl_result(); req(r); best_k <- which.max(r$sil) + 1; actual_k <- length(unique(r$cm$cluster))
    d <- data.frame(k = 2:8, sil = r$sil)
    p <- ggplot(d, aes(x = k, y = sil)) +
      geom_line(color = ACCENT2, linewidth = 1) + geom_point(color = ACCENT2, size = 2.5) +
      geom_vline(xintercept = actual_k, color = ACCENT3, linetype = "dashed", linewidth = 0.7) +
      geom_vline(xintercept = best_k, color = ACCENT2, linetype = "dotted", linewidth = 0.6) +
      annotate("text", x = actual_k + 0.3, y = max(r$sil) * 1.05,
               label = paste0("k=", actual_k), color = ACCENT3, size = 3, fontface = "bold") +
      scale_x_continuous(breaks = 2:8) +
      labs(x = "k", y = "Avg Silhouette") + theme_dark_dash(base_size = 10)
    ggplotly(p, tooltip = c("x", "y")) %>% plotly_dark()
  })

  # Heatmap
  output$cl_heatmap <- renderPlotly({
    r <- cl_result(); req(r)
    ml <- r$cm %>% select(-count) %>%
      pivot_longer(-cluster, names_to = "var", values_to = "val") %>%
      group_by(var) %>% mutate(sv = (val - min(val)) / (max(val) - min(val) + 1e-10)) %>%
      ungroup() %>% mutate(dn = gsub("_", " ", var))
    # hclust reorder variables
    val_mat <- ml %>% select(cluster, dn, sv) %>% pivot_wider(names_from = dn, values_from = sv)
    mat <- as.matrix(val_mat[, -1])
    if (ncol(mat) > 2) {
      ordered_vars <- colnames(mat)[hclust(dist(t(mat)), method = "ward.D2")$order]
    } else { ordered_vars <- colnames(mat) }
    ml$dn <- factor(ml$dn, levels = ordered_vars)
    p <- ggplot(ml, aes(x = cluster, y = dn, fill = sv,
      text = paste0("Cluster ", cluster, " | ", dn, "\nRaw: ", round(val, 2), "\nNorm: ", round(sv, 2)))) +
      geom_tile(color = BG_DARK, linewidth = 0.5) +
      geom_text(aes(label = round(sv, 2)), color = TEXT_MAIN, size = 2.8) +
      scale_fill_gradient2(low = "#a855f7", mid = BG_INPUT, high = "#f97316", midpoint = 0.5, limits = c(0, 1)) +
      labs(x = "Cluster", y = NULL, fill = "Norm") +
      theme_dark_dash(base_size = 10) + theme(panel.grid.major = element_blank())
    ggplotly(p, tooltip = "text") %>% plotly_dark(margin = list(l = 100, r = 20, t = 10, b = 35))
  })

  # Parallel Coordinates (plotly native)
  output$cl_parcoord <- renderPlotly({
    r <- cl_result(); req(r)
    raw <- cluster_pre$raw_features
    feat <- raw %>% select(all_of(r$vars))
    labels <- r$labels
    # Sample for performance
    n <- nrow(feat)
    if (n > 3000) { set.seed(42); idx <- sample(n, 3000); feat <- feat[idx, ]; labels <- labels[idx] }
    # Scale features to 0-1
    feat_scaled <- as.data.frame(lapply(feat, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE) + 1e-10)))
    dims <- lapply(seq_along(r$vars), function(i) {
      list(label = gsub("_", " ", r$vars[i]), values = feat_scaled[[i]])
    })
    plot_ly(type = "parcoords",
      line = list(color = as.numeric(labels),
                  colorscale = list(c(0, ACCENT), c(0.33, ACCENT2), c(0.66, ACCENT4), c(1, ACCENT3)),
                  showscale = TRUE),
      dimensions = dims
    ) %>% layout(
      paper_bgcolor = "transparent", plot_bgcolor = "transparent",
      font = list(color = TEXT_DIM, family = "Inter, sans-serif", size = 10),
      margin = list(l = 60, r = 30, t = 20, b = 30)
    )
  })

  # Dendrogram
  output$cl_dendro <- renderPlot({
    r <- cl_result(); req(r)
    sc <- r$sc; k <- input$cluster_k
    # Sample for dendrogram performance
    n <- nrow(sc)
    if (n > 1000) { set.seed(42); sc <- sc[sample(n, 1000), ] }
    hc <- hclust(dist(sc), method = "ward.D2")
    par(bg = BG_CARD, fg = TEXT_DIM, col.axis = TEXT_DIM, col.lab = TEXT_DIM, col.main = TEXT_MAIN)
    plot(hc, labels = FALSE, hang = -1, main = paste0("Dendrogram (Ward.D2, sample=", nrow(sc), ")"),
         xlab = "", sub = "", cex.main = 0.9)
    rect.hclust(hc, k = k, border = cluster_pal[1:k])
  }, bg = BG_CARD, execOnResize = TRUE)

  # Income distribution
  output$cl_demo <- renderPlotly({
    r <- cl_result(); req(r)
    d <- r$df %>% count(cluster, income_bracket) %>% group_by(cluster) %>% mutate(pct = n / sum(n))
    p <- ggplot(d, aes(x = cluster, y = pct, fill = income_bracket,
      text = paste0("Cluster ", cluster, " | ", income_bracket, ": ", round(pct * 100, 1), "%"))) +
      geom_col(position = "fill", alpha = 0.85, width = 0.6) +
      scale_fill_manual(values = income_colors) + scale_y_continuous(labels = label_percent()) +
      labs(x = NULL, y = NULL, fill = "Income") + theme_dark_dash()
    ggplotly(p, tooltip = "text") %>%
      plotly_dark(margin = list(l = 35, r = 15, t = 10, b = 55)) %>%
      layout(legend = list(orientation = "h", y = -0.25, x = 0.5, xanchor = "center"))
  })

  # Cluster warnings
  output$cl_size_warning <- renderUI({
    r <- cl_result(); req(r)
    sizes <- r$cm$count; total <- sum(sizes); small <- which(sizes < total * 0.02)
    warnings <- tagList()
    if (input$cluster_method == "hclust" && total > 10000) {
      warnings <- tagList(warnings, tags$div(
        style = paste0("background:", ACCENT4, "22;border:1px solid ", ACCENT4,
          ";border-radius:6px;padding:5px 8px;margin-top:8px;font-size:0.72rem;color:", ACCENT4, ";"),
        icon("info-circle"), " Large sample: hclust on 10k subsample."
      ))
    }
    if (length(small) > 0) {
      warnings <- tagList(warnings, tags$div(
        style = paste0("background:#fb718533;border:1px solid #fb7185;border-radius:6px;",
          "padding:5px 8px;margin-top:6px;font-size:0.72rem;color:#fb7185;"),
        icon("triangle-exclamation"), paste0(" Cluster ", paste(small, collapse = ","), " very small.")
      ))
    }
    if (length(warnings) > 0) warnings else NULL
  })

  output$cl_download <- downloadHandler(
    filename = function() paste0("cluster_centroids_", Sys.Date(), ".csv"),
    content = function(file) { r <- cl_result(); req(r); write.csv(r$cm, file, row.names = FALSE) }
  )

  # ===== TAB 5: MODEL CALIBRATION =====

  observeEvent(input$cal_type, {
    mod <- model_pre[[input$cal_type]]; cv_obj <- mod$cv
    n_lam <- length(cv_obj$lambda)
    lmin_idx <- which.min(abs(cv_obj$lambda - cv_obj$lambda.min))
    updateSliderInput(session, "cal_lambda", value = round(lmin_idx * 100 / n_lam))
  })

  cal_model <- reactive({
    mt <- input$cal_type; mod <- model_pre[[mt]]; cv_obj <- mod$cv
    X <- model_pre$X; y <- model_pre$y
    n_lam <- length(cv_obj$lambda)
    li <- max(1, min(round(input$cal_lambda * n_lam / 100), n_lam))
    lam <- cv_obj$lambda[li]
    pr <- c(predict(cv_obj, newx = X, s = lam)); res <- c(y - pr)
    rsq <- 1 - sum(res^2) / sum((y - mean(y))^2); rmse <- sqrt(mean(res^2))
    co <- coef(cv_obj, s = lam)
    cdf <- data.frame(variable = rownames(co)[-1], coefficient = c(co[-1, 1]), stringsAsFactors = FALSE) %>%
      filter(abs(coefficient) > 1e-8) %>% arrange(desc(abs(coefficient)))
    list(cv = cv_obj, cdf = cdf, rsq = rsq, rmse = rmse, pr = pr, y = y, res = res, lam = lam,
         xm = model_pre$X_means, xn = model_pre$X_colnames)
  })

  output$cal_rsq <- renderText({ m <- cal_model(); round(m$rsq, 4) })
  output$cal_rmse <- renderText({ m <- cal_model(); round(m$rmse, 4) })
  output$cal_nfeat <- renderText({ m <- cal_model(); paste0(nrow(m$cdf), "/", length(m$xn)) })
  output$cal_churn <- renderText({
    m <- cal_model()
    fmt_pct(mean(m$y))
  })

  output$cal_lambda_info <- renderUI({
    m <- cal_model(); cv_obj <- m$cv; lam <- m$lam
    mt <- input$cal_type; model_name <- if (mt == "lasso") "Lasso (L1)" else "Ridge (L2)"
    position <- if (abs(lam - cv_obj$lambda.min) / cv_obj$lambda.min < 0.1) {
      "at \u03bb.min \u2014 minimizes CV error."
    } else if (abs(lam - cv_obj$lambda.1se) / cv_obj$lambda.1se < 0.1) {
      "at \u03bb.1se \u2014 more parsimonious."
    } else if (lam < cv_obj$lambda.min) {
      "below \u03bb.min \u2014 less regularization."
    } else if (lam > cv_obj$lambda.1se) {
      "above \u03bb.1se \u2014 stronger regularization."
    } else { "between \u03bb.min and \u03bb.1se." }
    tags$div(
      style = paste0("background:", BG_INPUT, ";border:1px solid ", BORDER,
        ";border-radius:8px;padding:7px 10px;margin-bottom:6px;font-size:0.76rem;color:", TEXT_DIM, ";"),
      tags$span(style = paste0("font-weight:700;color:", ACCENT, ";"), model_name),
      " \u2014 \u03bb = ", formatC(lam, format = "e", digits = 3), " \u2014 ", position
    )
  })

  output$cal_imp <- renderPlotly({
    m <- cal_model(); req(nrow(m$cdf) > 0)
    d <- head(m$cdf, 15); d$variable <- factor(d$variable, levels = rev(d$variable))
    d$dir <- ifelse(d$coefficient > 0, "Increases Churn", "Decreases Churn")
    p <- ggplot(d, aes(x = variable, y = coefficient, fill = dir,
      text = paste0(gsub("_", " ", variable), "\nCoef: ", formatC(coefficient, format = "e", digits = 2)))) +
      geom_col(alpha = 0.85, width = 0.65) + coord_flip() +
      scale_fill_manual(values = c("Increases Churn" = ACCENT3, "Decreases Churn" = ACCENT2)) +
      scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
      labs(x = NULL, y = NULL, fill = NULL) + theme_dark_dash()
    ggplotly(p, tooltip = "text") %>%
      plotly_dark(margin = list(l = 50, r = 20, t = 35, b = 20)) %>%
      layout(legend = list(orientation = "h", y = 1.05, x = 0.5, xanchor = "center", yanchor = "bottom"))
  })

  output$cal_cv <- renderPlotly({
    m <- cal_model(); cv_obj <- m$cv
    d <- data.frame(log_lambda = log(cv_obj$lambda), mse = cv_obj$cvm,
                    mse_lo = cv_obj$cvlo, mse_hi = cv_obj$cvup)
    p <- ggplot(d, aes(x = log_lambda, y = mse)) +
      geom_ribbon(aes(ymin = mse_lo, ymax = mse_hi), fill = ACCENT, alpha = 0.15) +
      geom_line(color = ACCENT, linewidth = 0.8) + geom_point(color = ACCENT, size = 1, alpha = 0.6) +
      geom_vline(xintercept = log(cv_obj$lambda.min), color = ACCENT2, linetype = "dashed", linewidth = 0.7) +
      geom_vline(xintercept = log(cv_obj$lambda.1se), color = ACCENT4, linetype = "dotted", linewidth = 0.7) +
      geom_vline(xintercept = log(m$lam), color = ACCENT3, linetype = "dashed", linewidth = 0.8) +
      annotate("text", x = log(cv_obj$lambda.min), y = max(d$mse) * 0.95,
               label = "\u03bb.min", color = ACCENT2, size = 2.8, hjust = -0.2) +
      annotate("text", x = log(cv_obj$lambda.1se), y = max(d$mse) * 0.88,
               label = "\u03bb.1se", color = ACCENT4, size = 2.8, hjust = -0.2) +
      annotate("text", x = log(m$lam), y = max(d$mse) * 0.81,
               label = "Selected", color = ACCENT3, size = 2.8, hjust = -0.2) +
      labs(x = "Log(\u03bb)", y = "MSE (CV)") + theme_dark_dash()
    ggplotly(p) %>% plotly_dark()
  })

  output$cal_scatter <- renderPlotly({
    m <- cal_model()
    d <- data.frame(actual = c(m$y), predicted = c(m$pr))
    if (nrow(d) > 5000) { set.seed(42); d <- d[sample(nrow(d), 5000), ] }
    p <- ggplot(d, aes(x = actual, y = predicted)) +
      geom_point(alpha = 0.12, size = 0.7, color = ACCENT) +
      geom_abline(slope = 1, intercept = 0, color = ACCENT3, linetype = "dashed", linewidth = 0.6) +
      labs(x = "Actual", y = "Predicted") + theme_dark_dash()
    ggplotly(p) %>% plotly_dark()
  })

  output$cal_resid <- renderPlotly({
    m <- cal_model(); res_vec <- c(m$res)
    if (length(res_vec) > 10000) { set.seed(42); res_vec <- res_vec[sample(length(res_vec), 10000)] }
    d <- data.frame(residual = res_vec)
    p <- ggplot(d, aes(x = residual)) +
      geom_histogram(bins = 50, fill = ACCENT, alpha = 0.75, color = BG_CARD, linewidth = 0.3) +
      geom_vline(xintercept = 0, color = ACCENT3, linetype = "dashed", linewidth = 0.6) +
      labs(x = "Residual", y = "Frequency") + theme_dark_dash()
    ggplotly(p) %>% plotly_dark()
  })

  # ===== TAB 6: WHAT-IF SIMULATOR =====

  observeEvent(input$sim_type, {
    mod <- model_pre[[input$sim_type]]; cv_obj <- mod$cv
    n_lam <- length(cv_obj$lambda)
    lmin_idx <- which.min(abs(cv_obj$lambda - cv_obj$lambda.min))
    updateSliderInput(session, "sim_lambda", value = round(lmin_idx * 100 / n_lam))
  })

  sim_model <- reactive({
    mt <- input$sim_type; mod <- model_pre[[mt]]; cv_obj <- mod$cv
    X <- model_pre$X; y <- model_pre$y
    n_lam <- length(cv_obj$lambda)
    li <- max(1, min(round(input$sim_lambda * n_lam / 100), n_lam))
    lam <- cv_obj$lambda[li]
    pr <- c(predict(cv_obj, newx = X, s = lam)); res <- c(y - pr)
    rsq <- 1 - sum(res^2) / sum((y - mean(y))^2); rmse <- sqrt(mean(res^2))
    co <- coef(cv_obj, s = lam)
    cdf <- data.frame(variable = rownames(co)[-1], coefficient = c(co[-1, 1]), stringsAsFactors = FALSE) %>%
      filter(abs(coefficient) > 1e-8) %>% arrange(desc(abs(coefficient)))
    list(cv = cv_obj, cdf = cdf, rsq = rsq, rmse = rmse, lam = lam,
         xm = model_pre$X_means, xn = model_pre$X_colnames)
  })

  observeEvent(input$sim_reset, {
    updateSliderInput(session, "sim_age", value = 45)
    updateSliderInput(session, "sim_tx_count", value = 50)
    updateSliderInput(session, "sim_avg_value", value = 2e6)
    updateSliderInput(session, "sim_satisfaction", value = 5)
    updateSliderInput(session, "sim_nps", value = -20)
    updateSliderInput(session, "sim_credit_util", value = 0.3)
    updateSliderInput(session, "sim_tenure", value = 10)
    updateSliderInput(session, "sim_active_products", value = 3)
    updateSliderInput(session, "sim_support_tickets", value = 2)
    updateSliderInput(session, "sim_volatility", value = 1e6)
    updateSliderInput(session, "sim_household", value = 3)
  })

  sim_pred <- reactive({
    m <- sim_model()
    nx <- build_sim_input(input, m$xn, m$xm)
    p <- c(predict(m$cv, newx = nx, s = m$lam))
    max(0, min(1, p[1]))
  })

  output$sim_churn <- renderText(fmt_pct(sim_pred()))
  output$sim_risk <- renderText(classify_risk(sim_pred()))
  output$sim_rsq <- renderText({ m <- sim_model(); round(m$rsq, 4) })
  output$sim_rmse <- renderText({ m <- sim_model(); round(m$rmse, 4) })

  # Prediction Contribution Waterfall
  output$sim_waterfall <- renderPlotly({
    m <- sim_model(); req(nrow(m$cdf) > 0)
    nx <- build_sim_input(input, m$xn, m$xm)
    co <- coef(m$cv, s = m$lam)
    intercept <- co[1, 1]
    cnames <- rownames(co)[-1]; cvals <- co[-1, 1]
    # Compute individual contributions: coefficient * input_value
    contribs <- cvals * nx[1, ]
    cd <- data.frame(variable = cnames, contribution = contribs, stringsAsFactors = FALSE) %>%
      filter(abs(contribution) > 1e-8) %>% arrange(desc(abs(contribution))) %>% head(12)
    req(nrow(cd) > 0)
    cd$variable <- factor(cd$variable, levels = rev(cd$variable))
    cd$dir <- ifelse(cd$contribution > 0, "Increases Churn", "Decreases Churn")
    p <- ggplot(cd, aes(x = variable, y = contribution, fill = dir,
      text = paste0(gsub("_", " ", variable),
                    "\nContribution: ", round(contribution, 4)))) +
      geom_col(alpha = 0.85, width = 0.65) + coord_flip() +
      scale_fill_manual(values = c("Increases Churn" = ACCENT3, "Decreases Churn" = ACCENT2)) +
      scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
      labs(x = NULL, y = "Contribution to Prediction", fill = NULL) + theme_dark_dash()
    ggplotly(p, tooltip = "text") %>%
      plotly_dark(margin = list(l = 50, r = 20, t = 35, b = 20)) %>%
      layout(legend = list(orientation = "h", y = 1.05, x = 0.5, xanchor = "center", yanchor = "bottom"))
  })

  # Sensitivity Analysis Tornado
  output$sim_tornado <- renderPlotly({
    m <- sim_model(); req(nrow(m$cdf) > 0)
    base_pred <- sim_pred()
    # Map slider IDs to model feature names
    slider_map <- list(
      sim_age = "age", sim_tx_count = "tx_count", sim_avg_value = "avg_tx_value",
      sim_satisfaction = "satisfaction_score", sim_nps = "nps_score",
      sim_credit_util = "credit_utilization_ratio", sim_tenure = "customer_tenure",
      sim_active_products = "active_products", sim_support_tickets = "support_tickets_count",
      sim_volatility = "spending_volatility", sim_household = "household_size"
    )
    results <- lapply(names(slider_map), function(sid) {
      feat <- slider_map[[sid]]
      cur_val <- input[[sid]]
      if (is.null(cur_val) || cur_val == 0) return(NULL)
      # Predict at +20% and -20% of current value
      lo_val <- cur_val * 0.8; hi_val <- cur_val * 1.2
      nx_lo <- build_sim_input(input, m$xn, m$xm); nx_lo[1, feat] <- lo_val
      nx_hi <- build_sim_input(input, m$xn, m$xm); nx_hi[1, feat] <- hi_val
      p_lo <- max(0, min(1, c(predict(m$cv, newx = nx_lo, s = m$lam))[1]))
      p_hi <- max(0, min(1, c(predict(m$cv, newx = nx_hi, s = m$lam))[1]))
      data.frame(variable = feat, low = p_lo - base_pred, high = p_hi - base_pred,
                 swing = abs(p_hi - p_lo), stringsAsFactors = FALSE)
    })
    td <- do.call(rbind, Filter(Negate(is.null), results))
    req(nrow(td) > 0)
    td <- td %>% arrange(desc(swing)) %>% head(10)
    td$variable <- factor(td$variable, levels = rev(td$variable))
    # Build tornado with low/high bars
    td_long <- rbind(
      data.frame(variable = td$variable, delta = td$low, direction = "-20%", stringsAsFactors = FALSE),
      data.frame(variable = td$variable, delta = td$high, direction = "+20%", stringsAsFactors = FALSE)
    )
    p <- ggplot(td_long, aes(x = variable, y = delta, fill = direction,
      text = paste0(gsub("_", " ", variable), " (", direction, ")\n\u0394 Churn: ", round(delta * 100, 2), "%"))) +
      geom_col(position = "identity", alpha = 0.8, width = 0.6) + coord_flip() +
      scale_fill_manual(values = c("-20%" = ACCENT2, "+20%" = ACCENT3)) +
      scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
      scale_y_continuous(labels = scales::label_percent()) +
      geom_hline(yintercept = 0, color = TEXT_DIM, linewidth = 0.3) +
      labs(x = NULL, y = "\u0394 Churn Probability", fill = NULL) + theme_dark_dash()
    ggplotly(p, tooltip = "text") %>%
      plotly_dark(margin = list(l = 50, r = 20, t = 35, b = 20)) %>%
      layout(legend = list(orientation = "h", y = 1.05, x = 0.5, xanchor = "center", yanchor = "bottom"))
  })

  output$sim_recommend <- renderUI({
    p <- sim_pred(); risk <- classify_risk(p)
    recs <- switch(risk,
      "HIGH" = list(icon_el = icon("circle-exclamation"),
        title = "High Risk \u2014 Immediate Action Needed",
        items = list("Increase active products", "Improve satisfaction score", "Boost app login frequency")),
      "MEDIUM" = list(icon_el = icon("triangle-exclamation"),
        title = "Medium Risk \u2014 Monitor & Nurture",
        items = list("Schedule periodic check-in", "Watch credit utilization", "Encourage additional products")),
      "LOW" = list(icon_el = icon("circle-check"),
        title = "Low Risk \u2014 Retain & Grow",
        items = list("Maintain current service quality", "Consider cross-sell opportunities"))
    )
    border_col <- switch(risk, "HIGH" = ACCENT3, "MEDIUM" = ACCENT4, "LOW" = ACCENT2)
    tags$div(class = "recommend-box",
      style = paste0("border-left:3px solid ", border_col, ";background:", BG_INPUT, ";"),
      tags$div(class = "recommend-title", style = paste0("color:", border_col, ";"),
               recs$icon_el, tags$span(recs$title)),
      tags$ul(lapply(recs$items, tags$li))
    )
  })
}

shinyApp(ui = ui, server = server)
