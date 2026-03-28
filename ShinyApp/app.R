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
theme_dark_dash <- function(base_size = 12) {
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

# KPI card
kpi_box <- function(icon_name, title, output_id, icon_col = ACCENT) {
  tags$div(
    style = paste0(
      "background:", BG_CARD, ";",
      "border:1px solid ", BORDER, ";border-radius:14px;",
      "height:110px;display:flex;align-items:center;",
      "padding:0 18px;gap:14px;overflow:hidden;box-sizing:border-box;",
      "box-shadow:0 4px 24px rgba(0,0,0,0.35);"
    ),
    tags$div(
      style = paste0(
        "width:44px;height:44px;flex-shrink:0;border-radius:12px;",
        "background:", icon_col, "18;",
        "border:1px solid ", icon_col, "30;",
        "display:flex;align-items:center;justify-content:center;"
      ),
      icon(icon_name, style = paste0("color:", icon_col, ";font-size:1.2rem;"))
    ),
    tags$div(
      style = "min-width:0;",
      tags$div(
        style = paste0(
          "font-size:0.67rem;color:", TEXT_DIM, ";font-weight:500;",
          "text-transform:uppercase;letter-spacing:0.06em;",
          "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;"),
        title
      ),
      tags$div(
        style = paste0(
          "font-size:1.22rem;font-weight:700;color:", TEXT_MAIN,
          ";margin-top:4px;white-space:nowrap;"),
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
  padding: 0.5rem 1.5rem;
}
.navbar-brand, .navbar .navbar-brand {
  color: ", TEXT_MAIN, " !important; font-weight: 700;
  font-size: 1.1rem; letter-spacing: -0.02em;
}
.navbar-nav .nav-link {
  color: ", TEXT_DIM, " !important; font-size: 0.88rem; font-weight: 500;
  transition: color 0.2s, border-color 0.2s;
  padding: 0.6rem 1.1rem !important; border-bottom: 2px solid transparent;
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
  font-size: 0.95rem; letter-spacing: -0.01em;
  padding-bottom: 0.5rem !important;
  border-bottom: 1px solid ", BORDER, " !important;
  margin-bottom: 0.5rem !important;
}
.sidebar-content {
  padding: 0.75rem 1rem !important;
  display: flex; flex-direction: column; gap: 0;
}
.shiny-input-container { margin-bottom: 10px !important; }
.irs.irs--shiny { margin-top: 4px !important; }

/* MAIN PANEL */
.bslib-sidebar-layout > .main {
  padding: 1rem 1rem 1.5rem !important;
  gap: 1rem !important;
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
  font-size: 0.82rem !important; font-weight: 600 !important;
  padding: 0.55rem 0.25rem !important; box-shadow: none !important;
}
.accordion-button:not(.collapsed) { color: ", ACCENT, " !important; }
.accordion-button::after { filter: invert(0.6) !important; }
.accordion-body { padding: 0.5rem 0.25rem !important; }

.bslib-sidebar-layout > .sidebar {
  overflow-y: auto !important; overflow-x: hidden !important;
}
.sidebar .accordion {
  padding-left: 0.25rem !important; padding-right: 0.25rem !important;
}
.sidebar .accordion-button { padding-left: 0.5rem !important; }
.sidebar .accordion-body {
  padding: 0.5rem 0.25rem 0.5rem 0.75rem !important;
}
.sidebar .accordion-body .shiny-input-container { margin-left: 0 !important; }
.sidebar > .shiny-input-container,
.sidebar-content > .shiny-input-container {
  padding-left: 0.5rem !important; box-sizing: border-box;
}
.sidebar > .shiny-input-container label,
.sidebar-content > .shiny-input-container label,
.sidebar .accordion-body label { padding-left: 0 !important; }

/* CARDS */
.card, .bslib-card {
  background-color: ", BG_CARD, " !important;
  border: 1px solid ", BORDER, " !important;
  border-radius: 14px !important;
  box-shadow: 0 4px 24px rgba(0,0,0,0.35) !important;
  color: ", TEXT_MAIN, " !important; overflow: hidden;
}
.card-header {
  background-color: transparent !important;
  border-bottom: 1px solid ", BORDER, " !important;
  color: ", TEXT_MAIN, " !important; font-weight: 600 !important;
  font-size: 0.88rem; padding: 0.75rem 1.1rem !important;
  letter-spacing: -0.01em;
}
.card-body {
  background-color: transparent !important;
  padding: 0.9rem 1.1rem !important;
}

/* VALUE BOXES */
.bslib-value-box {
  border-radius: 14px !important; border: 1px solid ", BORDER, " !important;
  min-height: unset !important; height: 110px !important;
  overflow: hidden !important;
}
.bslib-value-box .value-box-area {
  background: ", BG_CARD, " !important;
  height: 110px !important; overflow: hidden !important;
}
.bslib-value-box .value-box-title {
  color: ", TEXT_DIM, " !important; font-size: 0.68rem; font-weight: 500;
  text-transform: uppercase; letter-spacing: 0.05em;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}
.bslib-value-box .value-box-value {
  color: ", TEXT_MAIN, " !important; font-weight: 700; font-size: 1.15rem;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}
.bslib-value-box .value-box-showcase {
  color: ", ACCENT, " !important; opacity: 0.7;
  max-width: 54px !important; min-width: 54px !important;
  padding: 0.4rem !important; flex-shrink: 0 !important;
}

/* INPUTS */
.form-control, .shiny-input-container select,
.selectize-input, .selectize-dropdown {
  background-color: ", BG_INPUT, " !important;
  border: 1px solid ", BORDER, " !important;
  color: ", TEXT_MAIN, " !important; border-radius: 8px !important;
  font-size: 0.85rem;
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
  color: ", TEXT_DIM, " !important; font-size: 0.8rem; font-weight: 500;
}
.irs--shiny .irs-bar {
  background: ", ACCENT, " !important; border-color: ", ACCENT, " !important;
}
.irs--shiny .irs-handle { border-color: ", ACCENT, " !important; }
.irs--shiny .irs-line  { background: #2a2a2a !important; }
.irs--shiny .irs-min, .irs--shiny .irs-max,
.irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to {
  background-color: ", ACCENT, " !important;
  color: white !important; font-size: 0.75rem;
}
.btn-primary {
  background: ", ACCENT, " !important; border: none !important;
  border-radius: 8px !important; font-weight: 600 !important;
  font-size: 0.85rem;
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
  font-size: 0.78rem !important; padding: 3px 10px !important;
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
.form-check-label { color: ", TEXT_DIM, " !important; font-size: 0.83rem; }

/* DT TABLE */
.dataTables_wrapper { color: ", TEXT_DIM, " !important; }
table.dataTable {
  background-color: transparent !important;
  color: ", TEXT_MAIN, " !important; font-size: 0.85rem;
}
table.dataTable thead th {
  background-color: ", BG_INPUT, " !important;
  color: ", ACCENT, " !important;
  border-bottom: 1px solid ", BORDER, " !important;
  font-weight: 600; font-size: 0.8rem;
}
table.dataTable tbody tr { background-color: transparent !important; }
table.dataTable tbody tr:hover { background-color: #1f1f1f !important; }
table.dataTable tbody td { border-color: ", BORDER, " !important; }

/* VERBATIM */
pre, .shiny-text-output, code {
  background-color: ", BG_INPUT, " !important;
  color: #e2e8f0 !important;
  border: 1px solid ", BORDER, " !important;
  border-radius: 8px !important; padding: 12px 16px !important;
  font-family: 'JetBrains Mono','Fira Code','Consolas',monospace !important;
  font-size: 0.82rem !important; line-height: 1.5 !important;
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

/* STAT TEST CARD */
.analysis-result {
  background: ", BG_INPUT, " !important;
  border-left: 3px solid ", ACCENT, " !important;
  border-radius: 8px !important; padding: 12px 14px !important;
  margin-top: 10px;
}
.stat-row {
  display: flex; justify-content: space-between; align-items: center;
  padding: 5px 0; border-bottom: 1px solid ", BORDER, ";
  font-size: 0.81rem;
}
.stat-row:last-child { border-bottom: none; }
.stat-label { color: ", TEXT_DIM, "; }
.stat-value { color: ", TEXT_MAIN, "; font-weight: 600; }
.stat-badge {
  display: inline-block; padding: 1px 7px; border-radius: 4px;
  font-size: 0.73rem; font-weight: 700; margin-left: 6px;
}
.stat-conclusion {
  margin-top: 10px; font-size: 0.82rem; font-weight: 600; line-height: 1.5;
}

/* RECOMMENDATION BOX */
.recommend-box {
  border-radius: 8px; padding: 10px 14px; margin-bottom: 12px;
}
.recommend-box ul {
  margin: 5px 0 0 0; padding-left: 16px;
  font-size: 0.8rem; color: ", TEXT_DIM, "; line-height: 1.75;
}
.recommend-title {
  font-size: 0.82rem; font-weight: 600;
  display: flex; align-items: center; gap: 6px;
}

/* NAVSET TABS inside cards */
.nav-tabs { border-bottom: 1px solid ", BORDER, " !important; }
.nav-tabs .nav-link {
  color: ", TEXT_DIM, " !important; font-size: 0.8rem !important;
  border: none !important; padding: 0.3rem 0.8rem !important;
  background: transparent !important;
}
.nav-tabs .nav-link.active {
  color: ", ACCENT, " !important; background: transparent !important;
  border-bottom: 2px solid ", ACCENT, " !important;
}
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

  # ===== ABOUT =====
  nav_panel(
    title = "About", icon = icon("info-circle"),

    # Hero Banner
    tags$div(
      style = paste0(
        "background:", BG_CARD, ";",
        "border:1px solid ", BORDER, ";border-radius:16px;",
        "padding:56px 52px 48px;margin-bottom:20px;",
        "position:relative;overflow:hidden;"
      ),
      tags$div(style = paste0(
        "position:absolute;top:0;left:0;right:0;height:2px;",
        "background:linear-gradient(90deg,", ACCENT4, ",",
        ACCENT, ",", ACCENT2, ",transparent);"
      )),
      tags$div(style = paste0(
        "position:absolute;top:-120px;right:-80px;",
        "width:420px;height:420px;",
        "border-radius:50%;background:radial-gradient(circle,",
        ACCENT2, "20 0%,transparent 65%);",
        "pointer-events:none;filter:blur(50px);"
      )),
      tags$div(style = paste0(
        "position:absolute;bottom:-80px;left:200px;",
        "width:280px;height:280px;",
        "border-radius:50%;background:radial-gradient(circle,",
        ACCENT, "15 0%,transparent 65%);",
        "pointer-events:none;filter:blur(40px);"
      )),
      tags$div(
        style = paste0(
          "position:relative;display:flex;",
          "justify-content:space-between;align-items:flex-end;",
          "flex-wrap:wrap;gap:32px;"
        ),
        tags$div(
          style = "max-width:620px;",
          tags$div(
            style = paste0(
              "display:inline-flex;align-items:center;gap:6px;",
              "margin-bottom:24px;font-size:0.72rem;font-weight:700;",
              "letter-spacing:0.1em;text-transform:uppercase;",
              "color:", ACCENT4, ";background:", ACCENT4,
              "18;padding:4px 14px;border-radius:20px;",
              "border:1px solid ", ACCENT4, "35;"
            ),
            "ISSS608 \u00b7 Visual Analytics & Applications"
          ),
          tags$div(
            style = paste0(
              "font-weight:800;font-size:2.6rem;line-height:1.08;",
              "letter-spacing:-0.04em;color:", TEXT_MAIN,
              ";margin-bottom:20px;"
            ),
            "Democratizing", tags$br(),
            tags$span(style = paste0("color:", ACCENT, ";"), "Fintech"),
            tags$span(style = paste0("color:", ACCENT2, ";"),
                      " Analytics")
          ),
          tags$p(
            style = paste0(
              "color:", TEXT_DIM, ";font-size:0.97rem;line-height:1.8;",
              "max-width:520px;margin:0 0 36px;"
            ),
            "An interactive visual analytics platform that transforms ",
            "high-dimensional financial data from the COFINFAD dataset ",
            "into actionable intelligence \u2014 covering customer ",
            "behaviour, segmentation, and churn risk prediction ",
            "for Colombia's fintech sector."
          ),
          tags$div(
            style = "display:flex;gap:40px;flex-wrap:wrap;",
            lapply(
              list(
                list(val = "48,723", lbl = "Customers",    col = ACCENT),
                list(val = "3.16M",  lbl = "Transactions", col = ACCENT2),
                list(val = "54",     lbl = "Variables",    col = ACCENT4),
                list(val = "3",      lbl = "Modules",      col = ACCENT3)
              ),
              function(item) {
                tags$div(
                  style = paste0("border-left:2px solid ", item$col,
                                 "55;padding-left:14px;"),
                  tags$div(
                    style = paste0(
                      "font-size:1.9rem;font-weight:800;color:",
                      TEXT_MAIN,
                      ";letter-spacing:-0.04em;line-height:1;"),
                    item$val),
                  tags$div(
                    style = paste0(
                      "font-size:0.72rem;color:", TEXT_DIM,
                      ";text-transform:uppercase;",
                      "letter-spacing:0.08em;margin-top:5px;"),
                    item$lbl)
                )
              }
            )
          )
        )
      )
    ),

    # How to Use Guide
    tags$div(
      style = paste0(
        "background:", BG_CARD, ";border:1px solid ", BORDER,
        ";border-radius:14px;padding:28px 32px;margin-bottom:20px;"
      ),
      tags$div(
        style = paste0("font-size:0.72rem;font-weight:700;",
                       "letter-spacing:0.1em;text-transform:uppercase;",
                       "color:", ACCENT4, ";margin-bottom:16px;"),
        "HOW TO USE THIS APP"
      ),
      tags$div(
        style = "display:flex;gap:24px;flex-wrap:wrap;",
        lapply(
          list(
            list(num = "1", icon = "chart-line", col = ACCENT,
                 title = "Explore",
                 desc = paste0("Start with the Exploratory Analysis tab. ",
                               "Filter by date range, transaction types, and ",
                               "income brackets. Run ANOVA or T-tests to ",
                               "validate patterns.")),
            list(num = "2", icon = "object-group", col = ACCENT2,
                 title = "Segment",
                 desc = paste0("Move to Customer Segmentation. Choose a feature ",
                               "preset and algorithm, then adjust k to discover ",
                               "natural customer groups with PCA visualization.")),
            list(num = "3", icon = "brain", col = ACCENT3,
                 title = "Predict",
                 desc = paste0("Use Churn Prediction to identify risk drivers. ",
                               "Adjust the What-If sliders to simulate ",
                               "interventions and see churn probability change ",
                               "in real time."))
          ),
          function(item) {
            tags$div(
              style = "flex:1;min-width:200px;",
              tags$div(
                style = "display:flex;align-items:center;gap:10px;margin-bottom:10px;",
                tags$div(
                  style = paste0(
                    "width:32px;height:32px;border-radius:8px;flex-shrink:0;",
                    "background:", item$col, "18;border:1px solid ",
                    item$col, "30;display:flex;align-items:center;",
                    "justify-content:center;font-weight:800;font-size:0.85rem;",
                    "color:", item$col, ";"),
                  item$num),
                tags$span(
                  style = paste0("font-weight:700;font-size:0.95rem;color:",
                                 TEXT_MAIN, ";"),
                  item$title)
              ),
              tags$p(
                style = paste0("color:", TEXT_DIM,
                               ";font-size:0.82rem;line-height:1.7;margin:0;"),
                item$desc)
            )
          }
        )
      )
    ),

    # Module Cards
    layout_columns(
      col_widths = c(4, 4, 4),

      # Module 1
      tags$div(
        style = paste0(
          "background:", BG_CARD, ";border:1px solid ", BORDER,
          ";border-radius:14px;padding:28px 24px;height:100%;",
          "position:relative;overflow:hidden;"
        ),
        tags$div(style = paste0(
          "position:absolute;top:0;left:0;right:0;height:2px;",
          "background:", ACCENT, ";"
        )),
        tags$div(
          style = paste0(
            "width:44px;height:44px;border-radius:12px;background:",
            ACCENT, "18;border:1px solid ", ACCENT, "30;",
            "display:flex;align-items:center;justify-content:center;",
            "margin-bottom:18px;"),
          icon("chart-line",
               style = paste0("color:", ACCENT, ";font-size:1.15rem;"))
        ),
        tags$h5(
          style = paste0("font-weight:700;color:", TEXT_MAIN,
                         ";margin-bottom:8px;font-size:0.97rem;"),
          "Exploratory & Confirmatory Analysis"),
        tags$p(
          style = paste0("color:", TEXT_DIM,
                         ";font-size:0.84rem;line-height:1.75;",
                         "margin-bottom:18px;"),
          "Uncover temporal trends, geographic patterns, and customer ",
          "distributions. Validate hypotheses with ANOVA and ",
          "Welch\u2019s T-tests with effect-size reporting."),
        tags$div(
          style = "display:flex;flex-wrap:wrap;gap:6px;",
          lapply(c("Time Series", "Choropleth", "ANOVA",
                   "Welch's T-test", "Effect Size",
                   "\u03b7\u00b2 / Cohen's d"),
                 function(t)
            tags$span(
              style = paste0(
                "font-size:0.72rem;padding:3px 9px;border-radius:5px;",
                "background:", ACCENT, "15;color:", ACCENT,
                ";font-weight:500;border:1px solid ", ACCENT, "25;"),
              t)
          )
        )
      ),

      # Module 2
      tags$div(
        style = paste0(
          "background:", BG_CARD, ";border:1px solid ", BORDER,
          ";border-radius:14px;padding:28px 24px;height:100%;",
          "position:relative;overflow:hidden;"
        ),
        tags$div(style = paste0(
          "position:absolute;top:0;left:0;right:0;height:2px;",
          "background:", ACCENT2, ";"
        )),
        tags$div(
          style = paste0(
            "width:44px;height:44px;border-radius:12px;background:",
            ACCENT2, "18;border:1px solid ", ACCENT2, "30;",
            "display:flex;align-items:center;justify-content:center;",
            "margin-bottom:18px;"),
          icon("object-group",
               style = paste0("color:", ACCENT2, ";font-size:1.15rem;"))
        ),
        tags$h5(
          style = paste0("font-weight:700;color:", TEXT_MAIN,
                         ";margin-bottom:8px;font-size:0.97rem;"),
          "Customer Segmentation"),
        tags$p(
          style = paste0("color:", TEXT_DIM,
                         ";font-size:0.84rem;line-height:1.75;",
                         "margin-bottom:18px;"),
          "Identify distinct customer groups using three unsupervised ",
          "learning algorithms. Pre-computed clusters enable instant ",
          "exploration with PCA visualisation and silhouette diagnostics."),
        tags$div(
          style = "display:flex;flex-wrap:wrap;gap:6px;",
          lapply(c("K-Means", "Hierarchical", "PAM",
                   "PCA", "Elbow", "Silhouette", "RFM"),
                 function(t)
            tags$span(
              style = paste0(
                "font-size:0.72rem;padding:3px 9px;border-radius:5px;",
                "background:", ACCENT2, "15;color:", ACCENT2,
                ";font-weight:500;border:1px solid ", ACCENT2, "25;"),
              t)
          )
        )
      ),

      # Module 3
      tags$div(
        style = paste0(
          "background:", BG_CARD, ";border:1px solid ", BORDER,
          ";border-radius:14px;padding:28px 24px;height:100%;",
          "position:relative;overflow:hidden;"
        ),
        tags$div(style = paste0(
          "position:absolute;top:0;left:0;right:0;height:2px;",
          "background:", ACCENT3, ";"
        )),
        tags$div(
          style = paste0(
            "width:44px;height:44px;border-radius:12px;background:",
            ACCENT3, "18;border:1px solid ", ACCENT3, "30;",
            "display:flex;align-items:center;justify-content:center;",
            "margin-bottom:18px;"),
          icon("brain",
               style = paste0("color:", ACCENT3, ";font-size:1.15rem;"))
        ),
        tags$h5(
          style = paste0("font-weight:700;color:", TEXT_MAIN,
                         ";margin-bottom:8px;font-size:0.97rem;"),
          "Churn Prediction"),
        tags$p(
          style = paste0("color:", TEXT_DIM,
                         ";font-size:0.84rem;line-height:1.75;",
                         "margin-bottom:18px;"),
          "Pre-trained Lasso and Ridge regression models quantify ",
          "churn risk. A real-time What-If simulator lets practitioners ",
          "test intervention scenarios with cross-validation diagnostics."),
        tags$div(
          style = "display:flex;flex-wrap:wrap;gap:6px;",
          lapply(c("Lasso", "Ridge", "CV Plot",
                   "R\u00b2 / RMSE", "What-If", "Recommendations"),
                 function(t)
            tags$span(
              style = paste0(
                "font-size:0.72rem;padding:3px 9px;border-radius:5px;",
                "background:", ACCENT3, "15;color:", ACCENT3,
                ";font-weight:500;border:1px solid ", ACCENT3, "25;"),
              t)
          )
        )
      )
    ),

    tags$div(style = "height:16px;"),

    # Dataset + Team
    layout_columns(
      col_widths = c(5, 7),

      # Dataset card
      card(
        card_header(
          tags$div(
            style = "display:flex;align-items:center;gap:8px;",
            icon("database",
                 style = paste0("color:", ACCENT4, ";")),
            tags$span("Dataset \u2014 COFINFAD"))
        ),
        card_body(
          tags$p(
            style = paste0("color:", TEXT_DIM,
                           ";font-size:0.85rem;line-height:1.75;",
                           "margin-bottom:16px;"),
            "The ",
            tags$strong(style = paste0("color:", TEXT_MAIN), "COFINFAD"),
            " dataset covers retail banking customers in Colombia, ",
            "combining CRM attributes, transaction history, and ",
            "customer lifecycle indicators into a unified ",
            "analytical frame."),
          lapply(
            list(
              list(icon = "users",      col = ACCENT,
                   lbl = "Customer Records",  val = "48,723"),
              list(icon = "receipt",    col = ACCENT2,
                   lbl = "Transaction Events", val = "3,159,157"),
              list(icon = "table-list", col = ACCENT4,
                   lbl = "Feature Dimensions", val = "54 variables"),
              list(icon = "calendar",   col = ACCENT3,
                   lbl = "Observation Period",  val = "12 months")
            ),
            function(r) {
              tags$div(
                style = paste0(
                  "display:flex;align-items:center;gap:12px;",
                  "padding:10px 0;border-bottom:1px solid ", BORDER, ";"),
                tags$div(
                  style = paste0(
                    "width:34px;height:34px;border-radius:9px;",
                    "flex-shrink:0;background:", r$col, "18;",
                    "border:1px solid ", r$col, "28;",
                    "display:flex;align-items:center;",
                    "justify-content:center;"),
                  icon(r$icon,
                       style = paste0("color:", r$col,
                                      ";font-size:0.85rem;"))
                ),
                tags$div(
                  style = "flex:1;",
                  tags$div(
                    style = paste0("font-size:0.77rem;color:",
                                   TEXT_DIM, ";"), r$lbl),
                  tags$div(
                    style = paste0(
                      "font-size:0.92rem;font-weight:600;color:",
                      TEXT_MAIN, ";margin-top:1px;"), r$val)
                )
              )
            }
          ),
          tags$div(style = "height:8px;"),
          tags$p(
            style = paste0(
              "font-size:0.77rem;color:", TEXT_DIM,
              ";margin:0;border-top:1px solid ", BORDER,
              ";padding-top:10px;"),
            icon("circle-info", style = "margin-right:5px;"),
            "Data sourced from the Colombian Financial Inclusion ",
            "& Fintech Analytics Database (COFINFAD)."
          )
        )
      ),

      # Team card
      card(
        card_header(
          tags$div(
            style = "display:flex;align-items:center;gap:8px;",
            icon("people-group",
                 style = paste0("color:", ACCENT2, ";")),
            tags$span("Team 13"))
        ),
        card_body(
          tags$div(
            style = paste0(
              "display:flex;flex-direction:column;gap:10px;",
              "margin-bottom:16px;"),
            {
              member <- function(initials, name, role,
                                 desc, avatar_col) {
                tags$div(
                  style = paste0(
                    "display:flex;align-items:flex-start;gap:14px;",
                    "padding:14px 16px;background:", BG_INPUT,
                    ";border-radius:12px;border:1px solid ",
                    BORDER, ";"),
                  tags$div(
                    style = paste0(
                      "width:42px;height:42px;border-radius:10px;",
                      "flex-shrink:0;background:", avatar_col, "25;",
                      "border:1px solid ", avatar_col, "40;",
                      "display:flex;align-items:center;",
                      "justify-content:center;font-weight:700;",
                      "font-size:0.9rem;color:", avatar_col,
                      ";letter-spacing:0.02em;"),
                    initials
                  ),
                  tags$div(
                    style = "flex:1;min-width:0;",
                    tags$div(
                      style = paste0(
                        "display:flex;justify-content:space-between;",
                        "align-items:center;margin-bottom:4px;"),
                      tags$span(
                        style = paste0(
                          "font-weight:700;color:", TEXT_MAIN,
                          ";font-size:0.92rem;"), name),
                      tags$span(
                        style = paste0(
                          "font-size:0.7rem;font-weight:600;",
                          "padding:2px 9px;border-radius:5px;",
                          "background:", avatar_col, "18;color:",
                          avatar_col, ";border:1px solid ",
                          avatar_col, "28;"), role)
                    ),
                    tags$div(
                      style = paste0(
                        "font-size:0.81rem;color:", TEXT_DIM,
                        ";line-height:1.55;"), desc)
                  )
                )
              }
              tagList(
                member("JG", "Ji Guofang", "Clustering",
                       paste0("Customer segmentation pipeline, PCA ",
                              "visualisation, cluster profiling."),
                       ACCENT2),
                member("LY", "Lin Yan", "EDA",
                       paste0("Exploratory & confirmatory analysis, ",
                              "statistical testing, choropleth map."),
                       ACCENT),
                member("NN", "Nguyen Trong Nhan", "Prediction",
                       paste0("Regularized regression, What-If ",
                              "simulator, documentation & deployment."),
                       ACCENT3)
              )
            }
          ),
          tags$div(
            style = paste0(
              "display:flex;align-items:center;",
              "justify-content:space-between;padding:12px 16px;",
              "background:", BG_DARK, ";border-radius:10px;",
              "border:1px solid ", BORDER, ";"
            ),
            tags$div(
              tags$div(
                style = paste0("font-size:0.8rem;font-weight:600;",
                               "color:", TEXT_MAIN, ";"),
                "ISSS608 Visual Analytics & Applications"),
              tags$div(
                style = paste0("font-size:0.74rem;color:", TEXT_DIM,
                               ";margin-top:2px;"),
                "Singapore Management University",
                " \u00b7 AY2024\u20132025 April Term")
            ),
            tags$div(
              style = paste0(
                "width:38px;height:38px;border-radius:10px;",
                "background:", ACCENT2, "18;border:1px solid ",
                ACCENT2, "28;display:flex;align-items:center;",
                "justify-content:center;"),
              icon("graduation-cap",
                   style = paste0("color:", ACCENT2,
                                  ";font-size:1rem;"))
            )
          )
        )
      )
    )
  ),

  # ===== MODULE 1: EDA & CDA =====
  nav_panel(
    title = "Exploratory Analysis",
    icon  = icon("chart-line"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Filters & Controls", width = 270,
        accordion(
          open = TRUE,
          accordion_panel(
            "Time & Transaction Filters",
            dateRangeInput("eda_date_range", "Date Range",
                           start = min(monthly_total$month),
                           end   = max(monthly_total$month),
                           min   = min(monthly_total$month),
                           max   = max(monthly_total$month)),
            checkboxGroupInput("eda_tx_types", "Transaction Types",
                               choices  = c("Transfer","Withdrawal",
                                            "Payment","Deposit"),
                               selected = c("Transfer","Withdrawal",
                                            "Payment","Deposit")),
            actionButton("eda_reset", "Reset Filters",
                         class = "btn-outline-secondary w-100 mt-1",
                         icon  = icon("rotate-left"))
          ),
          accordion_panel(
            "Choropleth Map",
            selectInput("eda_map_metric", "Map Metric",
                        choices = c("Customer Count" = "customer_count",
                                    "Avg Churn Probability" = "avg_churn_prob",
                                    "Avg Income Level" = "avg_income",
                                    "Avg CLV" = "avg_clv",
                                    "Avg Satisfaction" = "avg_satisfaction"))
          ),
          accordion_panel(
            "Statistical Testing",
            selectInput("eda_group_var", "Grouping Variable",
                        choices = c("income_bracket", "customer_segment",
                                    "clv_segment", "gender", "age_group",
                                    "acquisition_channel",
                                    "feedback_sentiment")),
            selectInput("eda_test_var", "Test Variable",
                        choices = c("avg_tx_value", "total_tx_volume",
                                    "tx_count", "satisfaction_score",
                                    "churn_probability",
                                    "credit_utilization_ratio",
                                    "customer_tenure")),
            selectInput("eda_stat_test", "Statistical Test",
                        choices = c("One-Way ANOVA" = "anova",
                                    "Welch's T-test" = "ttest")),
            conditionalPanel(
              condition = "input.eda_stat_test == 'ttest'",
              selectInput("eda_group1", "Group 1", choices = NULL),
              selectInput("eda_group2", "Group 2", choices = NULL)
            )
          )
        )
      ),

      # KPI Row
      tags$div(
        style = "height:110px;margin-bottom:1rem;",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          kpi_box("users",                "Total Customers",
                  "kpi_customers", ACCENT),
          kpi_box("coins",                "Transaction Volume",
                  "kpi_volume",    ACCENT),
          kpi_box("triangle-exclamation", "Avg Churn Rate",
                  "kpi_churn",     ACCENT3),
          kpi_box("star",                 "Avg Satisfaction",
                  "kpi_sat",       ACCENT2)
        )
      ),

      # Row 1: Time series + Choropleth map
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Monthly Transaction Volume by Type"),
          card_body(withSpinner(
            plotlyOutput("eda_ts", height = "320px"),
            type = 8, color = ACCENT))
        ),
        card(
          card_header("Colombian Department Overview \u2014 All Data"),
          card_body(
            if (has_spatial) {
              withSpinner(leafletOutput("eda_map", height = "320px"),
                          type = 8, color = ACCENT)
            } else {
              withSpinner(plotlyOutput("eda_map_bar", height = "320px"),
                          type = 8, color = ACCENT)
            }
          )
        )
      ),

      # Row 2: Distribution + Statistical Test
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Customer Distribution"),
          card_body(withSpinner(
            plotlyOutput("eda_dist", height = "340px"),
            type = 8, color = ACCENT))
        ),
        card(
          card_header("Statistical Hypothesis Testing"),
          card_body(
            withSpinner(plotlyOutput("eda_box", height = "220px"),
                        type = 8, color = ACCENT),
            uiOutput("eda_test")
          )
        )
      ),

      # Row 3: Correlation + Demographics
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Correlation Matrix \u2014 All Data"),
          card_body(withSpinner(
            plotlyOutput("eda_corr", height = "420px"),
            type = 8, color = ACCENT))
        ),
        card(
          card_header("Segment Composition by Income Bracket"),
          card_body(withSpinner(
            plotlyOutput("eda_demo", height = "420px"),
            type = 8, color = ACCENT))
        )
      )
    )
  ),

  # ===== MODULE 2: CLUSTERING =====
  nav_panel(
    title = "Customer Segmentation",
    icon  = icon("object-group"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Clustering Controls", width = 270,
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
          tags$div(
            style = "margin-bottom:4px;",
            tags$label(
              style = paste0("color:", TEXT_DIM,
                             ";font-size:0.8rem;font-weight:500;"),
              "Clustering Features"),
            tags$div(
              style = "display:flex;gap:6px;margin-bottom:6px;",
              actionButton("cluster_sel_all",  "Select All",
                           class = "btn-outline-secondary btn-sm flex-fill"),
              actionButton("cluster_sel_none", "Clear",
                           class = "btn-outline-secondary btn-sm flex-fill")
            )
          ),
          checkboxGroupInput(
            "cluster_vars", NULL,
            choices  = ALL_CLUSTER_VARS,
            selected = c("tx_count", "avg_tx_value", "customer_tenure",
                         "spending_volatility", "satisfaction_score")),
          actionButton("cluster_go", "Run Clustering",
                       class = "btn-primary w-100 mt-2",
                       icon = icon("play"))
        )
      ),

      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("PCA Cluster Visualization"),
          card_body(withSpinner(
            plotlyOutput("cl_pca", height = "380px"),
            type = 8, color = ACCENT))
        ),
        card(
          card_header("Cluster Quality Diagnostics"),
          card_body(
            navset_tab(
              nav_panel("Elbow",
                        withSpinner(plotlyOutput("cl_elbow", height = "220px"),
                                    type = 8, color = ACCENT)),
              nav_panel("Silhouette",
                        withSpinner(plotlyOutput("cl_silhouette", height = "220px"),
                                    type = 8, color = ACCENT))
            )
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Cluster Feature Profiles (Normalized)"),
          card_body(withSpinner(
            plotlyOutput("cl_profile", height = "340px"),
            type = 8, color = ACCENT))
        ),
        card(
          card_header("Income Distribution per Cluster"),
          card_body(withSpinner(
            plotlyOutput("cl_demo", height = "340px"),
            type = 8, color = ACCENT))
        )
      ),

      card(
        card_header(
          tags$div(
            style = paste0("display:flex;justify-content:space-between;",
                           "align-items:center;"),
            tags$span("Cluster Centroids Summary"),
            downloadButton("cl_download", "Export CSV",
                           class = "btn-outline-secondary btn-sm",
                           style = "font-size:0.78rem;padding:2px 10px;")
          )
        ),
        card_body(
          uiOutput("cl_size_warning"),
          withSpinner(DTOutput("cl_table"),
                              type = 8, color = ACCENT))
      )
    )
  ),

  # ===== MODULE 3: PREDICTION =====
  nav_panel(
    title = "Churn Prediction",
    icon  = icon("brain"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Model & Simulator", width = 270, open = TRUE,
        accordion(
          open = TRUE,
          accordion_panel(
            "Regularized Regression",
            radioButtons("pred_type", "Model Type",
                         choices = c("Lasso (L1)" = "lasso",
                                     "Ridge (L2)" = "ridge"),
                         inline = TRUE),
            sliderInput("pred_lambda", "Lambda Index (\u03bb)",
                        1, 100, 50),
            tags$small(
              style = paste0("color:", TEXT_DIM,
                             ";font-size:0.75rem;margin-top:-8px;",
                             "display:block;"),
              "Auto-set to \u03bb.min (optimal). Adjust to explore regularization trade-offs.")
          ),
          accordion_panel(
            "What-If Simulator",
            actionButton("sim_reset", "Reset to Defaults",
                         class = "btn-outline-secondary w-100 mb-2",
                         icon  = icon("rotate-left")),
            sliderInput("sim_age",             "Age",
                        29, 60, 45),
            sliderInput("sim_tx_count",        "Transaction Count",
                        1, 200, 50),
            sliderInput("sim_avg_value",       "Avg Transaction Value",
                        1e5, 2e7, 2e6, step = 2e5),
            sliderInput("sim_satisfaction",    "Satisfaction Score",
                        1, 10, 5, 0.5),
            sliderInput("sim_nps",             "NPS Score",
                        -100, 100, -20),
            sliderInput("sim_credit_util",     "Credit Utilization",
                        0, 1, 0.3, 0.05),
            sliderInput("sim_tenure",          "Tenure (months)",
                        1, 24, 10),
            sliderInput("sim_active_products", "Active Products",
                        0, 10, 3),
            sliderInput("sim_support_tickets", "Support Tickets",
                        0, 20, 2),
            sliderInput("sim_volatility",      "Spending Volatility",
                        0, 1e7, 1e6, step = 2e5),
            sliderInput("sim_household",       "Household Size",
                        1, 10, 3)
          )
        )
      ),

      # KPI Row
      tags$div(
        style = "height:110px;margin-bottom:1rem;",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          kpi_box("gauge-high",    "Churn Probability",
                  "sim_churn", ACCENT),
          kpi_box("shield-halved", "Risk Level",
                  "sim_risk",  ACCENT4),
          kpi_box("chart-simple",  "Model R\u00b2",
                  "pred_rsq",  ACCENT2),
          kpi_box("bullseye",      "RMSE",
                  "pred_rmse", ACCENT3)
        )
      ),

      # Lambda interpretation
      uiOutput("pred_lambda_info"),

      # Recommendations
      uiOutput("sim_recommend"),

      # Row 1: Importance + CV Performance
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Top Churn Predictors (Variable Importance)"),
          card_body(withSpinner(
            plotlyOutput("pred_imp", height = "360px"),
            type = 8, color = ACCENT))
        ),
        card(
          card_header("Cross-Validation Performance (\u03bb vs MSE)"),
          card_body(withSpinner(
            plotlyOutput("pred_cv", height = "360px"),
            type = 8, color = ACCENT))
        )
      ),

      # Row 2: Predicted vs Actual + Residuals
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Model Calibration: Predicted vs Actual"),
          card_body(withSpinner(
            plotlyOutput("pred_scatter", height = "340px"),
            type = 8, color = ACCENT))
        ),
        card(
          card_header("Residual Distribution"),
          card_body(withSpinner(
            plotlyOutput("pred_resid", height = "340px"),
            type = 8, color = ACCENT))
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ===== MODULE 1: EDA & CDA =====

  # Reset filters
  observeEvent(input$eda_reset, {
    updateDateRangeInput(session, "eda_date_range",
                         start = min(monthly_total$month),
                         end   = max(monthly_total$month))
    updateCheckboxGroupInput(session, "eda_tx_types",
                             selected = c("Transfer", "Withdrawal",
                                          "Payment", "Deposit"))
  })

  # Update T-test group selectors when grouping variable changes
  observeEvent(input$eda_group_var, {
    grp_col <- customers[[input$eda_group_var]]
    if (is.factor(grp_col)) {
      lvls <- levels(grp_col)
    } else {
      lvls <- sort(unique(as.character(grp_col)))
    }
    updateSelectInput(session, "eda_group1",
                      choices = lvls, selected = lvls[1])
    updateSelectInput(session, "eda_group2",
                      choices = lvls,
                      selected = lvls[min(2, length(lvls))])
  })

  # KPIs
  output$kpi_customers <- renderText(fmt_num(nrow(customers)))
  output$kpi_volume <- renderText({
    d <- monthly_summary %>%
      filter(type %in% input$eda_tx_types,
             month >= input$eda_date_range[1],
             month <= input$eda_date_range[2])
    fmt_currency(sum(d$total_amount, na.rm = TRUE))
  })
  output$kpi_churn <- renderText(
    fmt_pct(mean(customers$churn_probability, na.rm = TRUE)))
  output$kpi_sat <- renderText(
    paste0(round(mean(customers$satisfaction_score, na.rm = TRUE), 1),
           " / 10"))

  # Time series
  output$eda_ts <- renderPlotly({
    d <- monthly_summary %>%
      filter(type %in% input$eda_tx_types,
             month >= input$eda_date_range[1],
             month <= input$eda_date_range[2])
    req(nrow(d) > 0)
    p <- ggplot(d, aes(x = month, y = total_amount / 1e9,
                        color = type, group = type)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.5, alpha = 0.9) +
      scale_color_manual(values = type_colors) +
      scale_y_continuous(labels = label_comma(suffix = "B")) +
      labs(x = NULL, y = "Volume (Billions COP)", color = NULL) +
      theme_dark_dash()
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      plotly_dark(margin = list(l = 50, r = 20, t = 20, b = 65)) %>%
      layout(legend = list(orientation = "h", y = -0.22,
                           x = 0.5, xanchor = "center"))
  })

  # Choropleth map (leaflet if available, bar chart fallback)
  if (has_spatial) {
    output$eda_map <- renderLeaflet({
      req(dept_sf)
      metric <- input$eda_map_metric
      vals <- dept_sf[[metric]]
      pal <- colorNumeric("YlOrRd", domain = vals, na.color = "#333333")

      labels <- sprintf(
        "<strong>%s</strong><br/>%s: %s<br/>Customers: %s",
        dept_sf$dept_match,
        gsub("_", " ", metric), round(vals, 2),
        ifelse(is.na(dept_sf$customer_count), "N/A",
               formatC(dept_sf$customer_count, format = "d", big.mark = ","))
      ) %>% lapply(htmltools::HTML)

      leaflet(dept_sf) %>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        addPolygons(
          fillColor = ~pal(vals),
          fillOpacity = 0.7,
          color = "#444444", weight = 1, opacity = 0.8,
          label = labels,
          highlightOptions = highlightOptions(
            weight = 2, color = ACCENT, fillOpacity = 0.9,
            bringToFront = TRUE)
        ) %>%
        addLegend("bottomright", pal = pal, values = vals,
                  title = gsub("_", " ", metric), opacity = 0.8)
    })
  } else {
    output$eda_map_bar <- renderPlotly({
      metric <- input$eda_map_metric
      d <- dept_stats %>% arrange(desc(!!sym(metric)))
      p <- ggplot(d, aes(x = reorder(department, !!sym(metric)),
                          y = !!sym(metric),
                          text = paste0(department, ": ",
                                        round(!!sym(metric), 2)))) +
        geom_col(fill = ACCENT, alpha = 0.8, width = 0.7) +
        coord_flip() +
        labs(x = NULL, y = gsub("_", " ", metric)) +
        theme_dark_dash()
      ggplotly(p, tooltip = "text") %>% plotly_dark()
    })
  }

  # Distribution
  output$eda_dist <- renderPlotly({
    grp <- input$eda_group_var
    d <- customers %>%
      count(!!sym(grp)) %>%
      arrange(desc(n)) %>%
      mutate(pct = n / sum(n))
    p <- ggplot(d, aes(
      x = reorder(!!sym(grp), n), y = n,
      text = paste0(!!sym(grp), ": ", fmt_num(n),
                    " (", round(pct * 100, 1), "%)"))) +
      geom_col(fill = ACCENT, alpha = 0.8, width = 0.7) +
      coord_flip() +
      scale_y_continuous(labels = label_comma(),
                         expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = "Customer Count") +
      theme_dark_dash()
    ggplotly(p, tooltip = "text") %>% plotly_dark()
  })

  # Boxplot for CDA
  output$eda_box <- renderPlotly({
    grp <- input$eda_group_var
    var <- input$eda_test_var
    d <- customers %>%
      select(g = !!sym(grp), v = !!sym(var)) %>% drop_na()
    if (input$eda_stat_test == "ttest") {
      req(input$eda_group1, input$eda_group2)
      d <- d %>% filter(g %in% c(input$eda_group1, input$eda_group2))
    }
    # Trim y-axis to IQR * 3 range to avoid extreme outliers squishing the plot
    q1 <- quantile(d$v, 0.25, na.rm = TRUE)
    q3 <- quantile(d$v, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    y_lo <- max(min(d$v, na.rm = TRUE), q1 - 3 * iqr)
    y_hi <- min(max(d$v, na.rm = TRUE), q3 + 3 * iqr)

    p <- ggplot(d, aes(x = g, y = v, fill = g)) +
      geom_boxplot(alpha = 0.75, outlier.size = 0.3,
                   outlier.alpha = 0.15, width = 0.6) +
      coord_cartesian(ylim = c(y_lo, y_hi)) +
      scale_fill_manual(
        values = c("#f97316", "#a855f7", "#fbbf24", "#fb7185",
                   "#f472b6", "#e879f9", "#fdba74", "#c084fc")) +
      labs(x = NULL, y = gsub("_", " ", var)) +
      theme_dark_dash() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 20, hjust = 1, size = 9))
    ggplotly(p) %>% plotly_dark()
  })

  # Statistical test result
  output$eda_test <- renderUI({
    grp       <- input$eda_group_var
    var       <- input$eda_test_var
    d         <- customers %>%
      select(group = !!sym(grp), value = !!sym(var)) %>% drop_na()
    d$group   <- as.factor(d$group)
    grp_label <- gsub("_", " ", grp)
    var_label <- gsub("_", " ", var)

    sig_badge <- function(pv) {
      stars <- ifelse(pv < 0.001, "***",
                      ifelse(pv < 0.01, "**",
                             ifelse(pv < 0.05, "*", "ns")))
      col <- if (pv < 0.05) ACCENT3 else ACCENT2
      tags$span(class = "stat-badge",
                style = paste0("background:", col, "33;color:", col, ";"),
                stars)
    }
    effect_interp <- function(e)
      ifelse(e < 0.01, "negligible",
             ifelse(e < 0.06, "small",
                    ifelse(e < 0.14, "medium", "large")))

    make_row <- function(lbl, val) {
      tags$div(class = "stat-row",
               tags$span(class = "stat-label", lbl),
               tags$span(class = "stat-value", val))
    }

    if (input$eda_stat_test == "anova") {
      ngrp    <- nlevels(d$group)
      n_total <- nrow(d)
      mod  <- aov(value ~ group, data = d)
      s    <- summary(mod)
      pv   <- s[[1]]$`Pr(>F)`[1]
      fv   <- s[[1]]$`F value`[1]
      eta2 <- s[[1]]$`Sum Sq`[1] / sum(s[[1]]$`Sum Sq`)
      conc_col  <- if (pv < 0.05) ACCENT3 else ACCENT2
      small_effect_note <- if (pv < 0.05 && eta2 < 0.06) {
        paste0(" Note: despite statistical significance, the effect size (\u03B7\u00B2=",
               round(eta2, 3), ") is small — the practical difference between groups ",
               "is minimal. This is common with large samples (n=",
               fmt_num(n_total), ").")
      } else ""
      conc_text <- if (pv < 0.05) {
        paste0("REJECT H\u2080: Significant difference in ",
               var_label, " across ", grp_label, " groups.",
               small_effect_note)
      } else {
        paste0("FAIL TO REJECT H\u2080: No significant difference in ",
               var_label, " across ", grp_label, " groups. ",
               "The groups show similar distributions for this variable, ",
               "suggesting ", grp_label, " does not meaningfully influence ",
               var_label, ".")
      }

      tags$div(
        class = "analysis-result",
        tags$div(
          style = paste0(
            "font-size:0.74rem;font-weight:700;color:", ACCENT,
            ";text-transform:uppercase;letter-spacing:0.06em;",
            "margin-bottom:8px;"),
          "ONE-WAY ANOVA"),
        make_row("Variable",    var_label),
        make_row("Grouping",
                 paste0(grp_label, " (", ngrp, " groups)")),
        make_row("Sample Size", fmt_num(n_total)),
        make_row("F-statistic", round(fv, 3)),
        tags$div(
          class = "stat-row",
          tags$span(class = "stat-label", "p-value"),
          tags$span(class = "stat-value",
                    formatC(pv, format = "e", digits = 3),
                    sig_badge(pv))),
        make_row("Effect Size (\u03b7\u00b2)",
                 paste0(round(eta2, 3), " \u2014 ",
                        effect_interp(eta2))),
        tags$div(class = "stat-conclusion",
                 style = paste0("color:", conc_col, ";"),
                 conc_text)
      )

    } else {
      # Welch's T-test
      req(input$eda_group1, input$eda_group2)
      validate(need(input$eda_group1 != input$eda_group2,
                    "Please select two different groups for comparison."))
      g1 <- input$eda_group1
      g2 <- input$eda_group2
      d1 <- d$value[d$group == g1]
      d2 <- d$value[d$group == g2]
      req(length(d1) > 1, length(d2) > 1)

      t_res <- t.test(d1, d2, var.equal = FALSE)
      pv    <- t_res$p.value
      tstat <- t_res$statistic
      n1    <- length(d1)
      n2    <- length(d2)
      # Cohen's d
      pooled_sd <- sqrt(((n1 - 1) * var(d1) + (n2 - 1) * var(d2)) /
                          (n1 + n2 - 2))
      cohens_d <- abs(mean(d1) - mean(d2)) / pooled_sd
      d_interp <- ifelse(cohens_d < 0.2, "negligible",
                         ifelse(cohens_d < 0.5, "small",
                                ifelse(cohens_d < 0.8, "medium",
                                       "large")))
      conc_col <- if (pv < 0.05) ACCENT3 else ACCENT2
      conc_text <- if (pv < 0.05) {
        paste0("REJECT H\u2080: ", var_label,
               " differs significantly between ",
               g1, " and ", g2, ".")
      } else {
        paste0("FAIL TO REJECT H\u2080: No significant difference in ",
               var_label, " between ", g1, " and ", g2, ".")
      }

      tags$div(
        class = "analysis-result",
        tags$div(
          style = paste0(
            "font-size:0.74rem;font-weight:700;color:", ACCENT,
            ";text-transform:uppercase;letter-spacing:0.06em;",
            "margin-bottom:8px;"),
          "WELCH'S T-TEST"),
        make_row("Variable",   var_label),
        make_row("Comparing",
                 paste0(g1, " (n=", n1, ") vs ", g2, " (n=", n2, ")")),
        make_row("t-statistic", round(tstat, 3)),
        tags$div(
          class = "stat-row",
          tags$span(class = "stat-label", "p-value"),
          tags$span(class = "stat-value",
                    formatC(pv, format = "e", digits = 3),
                    sig_badge(pv))),
        make_row("Effect Size (Cohen's d)",
                 paste0(round(cohens_d, 3), " \u2014 ", d_interp)),
        make_row("Mean Difference",
                 round(mean(d1) - mean(d2), 2)),
        tags$div(class = "stat-conclusion",
                 style = paste0("color:", conc_col, ";"),
                 conc_text)
      )
    }
  })

  # Correlation heatmap
  corr_cols <- customers %>%
    select(age, tx_count, avg_tx_value, total_tx_volume,
           satisfaction_score, churn_probability,
           credit_utilization_ratio, customer_tenure,
           active_products, nps_score, weekend_transaction_ratio,
           support_tickets_count)
  cor_matrix <- cor(corr_cols, use = "pairwise.complete.obs")
  cor_labels <- c("Age", "Tx Count", "Avg Tx Val", "Total Vol",
                  "Satisfaction", "Churn Prob", "Credit Util",
                  "Tenure", "Active Prod", "NPS", "Wknd Ratio",
                  "Support Tix")

  output$eda_corr <- renderPlotly({
    cor_df    <- expand.grid(V1 = cor_labels, V2 = cor_labels,
                             stringsAsFactors = FALSE)
    cor_df$r  <- as.vector(cor_matrix)
    cor_df$V1 <- factor(cor_df$V1, levels = cor_labels)
    cor_df$V2 <- factor(cor_df$V2, levels = rev(cor_labels))
    p <- ggplot(cor_df, aes(
      x = V1, y = V2, fill = r,
      text = paste0(V1, " vs ", V2, "\nr = ", round(r, 2)))) +
      geom_tile(color = BG_DARK, linewidth = 0.5) +
      scale_fill_gradient2(low = "#a855f7", mid = BG_CARD,
                           high = "#f97316",
                           midpoint = 0, limits = c(-1, 1)) +
      labs(x = NULL, y = NULL, fill = "r") +
      theme_dark_dash(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            axis.text.y = element_text(size = 8))
    ggplotly(p, tooltip = "text") %>%
      plotly_dark(margin = list(l = 90, r = 20, t = 20, b = 90))
  })

  # Demographics
  output$eda_demo <- renderPlotly({
    d <- customers %>%
      count(income_bracket, customer_segment) %>%
      group_by(income_bracket) %>%
      mutate(pct = n / sum(n))
    p <- ggplot(d, aes(
      x = income_bracket, y = n, fill = customer_segment,
      text = paste0(customer_segment, ": ", fmt_num(n),
                    " (", round(pct * 100, 1), "%)"))) +
      geom_col(position = "fill", alpha = 0.85, width = 0.65) +
      scale_fill_manual(values = segment_colors) +
      scale_y_continuous(labels = label_percent()) +
      labs(x = NULL, y = "Proportion", fill = "Segment") +
      theme_dark_dash()
    ggplotly(p, tooltip = "text") %>%
      plotly_dark(margin = list(l = 50, r = 20, t = 20, b = 65)) %>%
      layout(legend = list(orientation = "h", y = -0.22,
                           x = 0.5, xanchor = "center"))
  })

  # ===== MODULE 2: CLUSTERING =====

  # Select All / Clear for custom
  observeEvent(input$cluster_sel_all, {
    updateCheckboxGroupInput(session, "cluster_vars",
                             selected = ALL_CLUSTER_VARS)
  })
  observeEvent(input$cluster_sel_none, {
    updateCheckboxGroupInput(session, "cluster_vars",
                             selected = character(0))
  })

  # Helper: merge tiny clusters (< 1% of total) into nearest large cluster
  merge_tiny_clusters <- function(labels, sc, min_pct = 0.01) {
    n <- length(labels)
    tab <- table(labels)
    tiny <- as.integer(names(tab)[tab < n * min_pct])
    if (length(tiny) == 0) return(labels)
    big <- as.integer(names(tab)[tab >= n * min_pct])
    if (length(big) == 0) return(labels)
    # centroids of big clusters
    cen <- sapply(big, function(i) colMeans(sc[labels == i, , drop = FALSE]))
    for (ti in tiny) {
      pts <- which(labels == ti)
      cent_ti <- colMeans(sc[pts, , drop = FALSE])
      nearest <- big[which.min(colSums((cen - cent_ti)^2))]
      labels[pts] <- nearest
    }
    # Re-number clusters consecutively
    labels <- as.integer(factor(labels, levels = sort(unique(labels))))
    labels
  }

  # Pre-computed cluster result (for named presets)
  cl_precomputed <- reactive({
    preset_id <- input$cluster_preset
    req(preset_id != "custom")
    method <- input$cluster_method
    k_val  <- input$cluster_k

    pc <- cluster_pre$presets[[preset_id]]
    md <- pc$methods[[method]]
    k_col <- paste0("k", k_val)

    raw_labels <- md$labels[, k_col]
    # Merge tiny clusters
    raw <- cluster_pre$raw_features
    sc  <- scale(as.matrix(raw[, pc$features]))
    labels <- merge_tiny_clusters(raw_labels, sc)
    demo   <- cluster_pre$demographics
    raw    <- cluster_pre$raw_features

    pca_df <- pc$pca_scores
    pca_df$cluster          <- factor(labels)
    pca_df$income_bracket   <- demo$income_bracket
    pca_df$customer_segment <- demo$customer_segment
    pca_df$clv_segment      <- demo$clv_segment
    pca_df$churn_risk       <- demo$churn_risk

    feat_df <- raw %>% select(all_of(pc$features))
    feat_df$cluster <- factor(labels)
    cm <- feat_df %>%
      group_by(cluster) %>%
      summarise(across(everything(), mean), count = n(),
                .groups = "drop")

    list(
      pc   = pca_df,
      cm   = cm,
      wss  = md$wss,
      sil  = md$sil,
      ve   = pc$var_explained,
      vars = pc$features,
      df   = data.frame(
        cluster = factor(labels),
        income_bracket = demo$income_bracket)
    )
  })

  # Custom cluster result (runs live on button click)
  cl_custom <- eventReactive(input$cluster_go, {
    vars <- input$cluster_vars
    validate(need(length(vars) >= 2,
                  "Please select at least 2 features for clustering."))

    withProgress(message = "Running clustering...", value = 0.05, {
      raw  <- cluster_pre$raw_features
      demo <- cluster_pre$demographics
      feat <- as.matrix(raw[, vars])
      sc   <- scale(feat)
      incProgress(0.1)

      pca <- prcomp(sc, center = FALSE, scale. = FALSE)
      n_pcs <- min(2, ncol(sc))
      pca_df <- as.data.frame(pca$x[, 1:n_pcs])
      if (ncol(pca_df) == 1) pca_df$PC2 <- 0
      colnames(pca_df) <- c("PC1", "PC2")
      ve <- summary(pca)$importance[2, 1:n_pcs]
      incProgress(0.1)

      k <- input$cluster_k
      method <- input$cluster_method
      n <- nrow(sc)

      if (method == "kmeans") {
        set.seed(42)
        cl <- kmeans(sc, k, nstart = 25, iter.max = 100)$cluster
      } else if (method == "hclust") {
        if (n > 10000) {
          set.seed(42)
          idx <- sample(n, 10000)
          hc  <- hclust(dist(sc[idx, ]), method = "ward.D2")
          hc_cut <- cutree(hc, k)
          cen <- sapply(1:k, function(i)
            colMeans(sc[idx, ][hc_cut == i, , drop = FALSE]))
          cl <- apply(sc, 1, function(r)
            which.min(colSums((cen - r)^2)))
        } else {
          cl <- cutree(hclust(dist(sc), method = "ward.D2"), k)
        }
      } else {
        set.seed(42)
        cl <- clara(sc, k, metric = "euclidean", samples = 50,
                    sampsize = min(500, n))$clustering
      }
      incProgress(0.2)

      # Merge tiny clusters
      cl <- merge_tiny_clusters(cl, sc)

      pca_df$cluster          <- factor(cl)
      pca_df$income_bracket   <- demo$income_bracket
      pca_df$customer_segment <- demo$customer_segment
      pca_df$clv_segment      <- demo$clv_segment
      pca_df$churn_risk       <- demo$churn_risk

      feat_df <- as.data.frame(raw[, vars])
      feat_df$cluster <- factor(cl)
      cm <- feat_df %>%
        group_by(cluster) %>%
        summarise(across(everything(), mean), count = n(),
                  .groups = "drop")
      incProgress(0.1)

      # WSS / Silhouette diagnostics
      set.seed(42)
      ed <- if (n > 10000) sc[sample(n, 10000), ] else sc
      wss <- sapply(2:8, function(kk)
        kmeans(ed, kk, nstart = 10, iter.max = 50)$tot.withinss)
      incProgress(0.2)

      sil_sc <- if (nrow(ed) > 3000) ed[sample(nrow(ed), 3000), ] else ed
      sil_scores <- sapply(2:8, function(kk) {
        set.seed(42)
        km <- kmeans(sil_sc, kk, nstart = 10, iter.max = 50)
        mean(silhouette(km$cluster, dist(sil_sc))[, 3])
      })
      incProgress(0.2)

      list(
        pc   = pca_df,
        cm   = cm,
        wss  = wss,
        sil  = sil_scores,
        ve   = ve,
        vars = vars,
        df   = data.frame(
          cluster = factor(cl),
          income_bracket = demo$income_bracket)
      )
    })
  })

  # Unified cluster result
  cl_result <- reactive({
    if (input$cluster_preset == "custom") {
      cl_custom()
    } else {
      cl_precomputed()
    }
  })

  # PCA scatter (with outlier trimming via IQR)
  output$cl_pca <- renderPlotly({
    r <- cl_result(); req(r)
    cv <- input$cluster_color
    d  <- r$pc

    # Trim outliers: remove points beyond 1.5*IQR on PC1/PC2
    trim_iqr <- function(x) {
      q <- quantile(x, c(0.02, 0.98), na.rm = TRUE)
      x >= q[1] & x <= q[2]
    }
    keep <- trim_iqr(d$PC1) & trim_iqr(d$PC2)
    d <- d[keep, ]

    if (nrow(d) > 6000) {
      set.seed(42); d <- d[sample(nrow(d), 6000), ]
    }
    p <- ggplot(d, aes(x = PC1, y = PC2, color = !!sym(cv),
                        text = paste("Cluster:", cluster))) +
      geom_point(alpha = 0.35, size = 1.3) +
      labs(x = paste0("PC1 (", round(r$ve[1] * 100, 1), "% var)"),
           y = paste0("PC2 (", round(r$ve[2] * 100, 1), "% var)"),
           color = gsub("_", " ", cv)) +
      theme_dark_dash()
    if (cv == "cluster")
      p <- p + scale_color_manual(values = cluster_pal)
    else if (cv == "churn_risk")
      p <- p + scale_color_manual(values = churn_colors)
    else if (cv == "income_bracket")
      p <- p + scale_color_manual(values = income_colors)
    else if (cv == "customer_segment")
      p <- p + scale_color_manual(values = segment_colors)
    ggplotly(p, tooltip = c("text", "x", "y")) %>% plotly_dark()
  })

  # Elbow plot (ggplot2 + plotly)
  output$cl_elbow <- renderPlotly({
    r <- cl_result(); req(r)
    actual_k <- length(unique(r$cm$cluster))
    d <- data.frame(k = 2:8, wss = r$wss)
    p <- ggplot(d, aes(x = k, y = wss)) +
      geom_line(color = ACCENT, linewidth = 1.1) +
      geom_point(color = ACCENT, size = 3) +
      geom_vline(xintercept = actual_k, color = ACCENT3,
                 linetype = "dashed", linewidth = 0.8) +
      annotate("text", x = actual_k + 0.35, y = max(r$wss) * 0.93,
               label = paste0("k = ", actual_k),
               color = ACCENT3, size = 3.5, fontface = "bold") +
      scale_x_continuous(breaks = 2:8) +
      labs(x = "Number of Clusters (k)",
           y = "Within-Cluster SS") +
      theme_dark_dash(base_size = 11)
    ggplotly(p, tooltip = c("x", "y")) %>% plotly_dark()
  })

  # Silhouette plot (ggplot2 + plotly)
  output$cl_silhouette <- renderPlotly({
    r <- cl_result(); req(r)
    best_k <- which.max(r$sil) + 1
    actual_k <- length(unique(r$cm$cluster))
    d <- data.frame(k = 2:8, sil = r$sil)
    p <- ggplot(d, aes(x = k, y = sil)) +
      geom_line(color = ACCENT2, linewidth = 1.1) +
      geom_point(color = ACCENT2, size = 3) +
      geom_vline(xintercept = actual_k, color = ACCENT3,
                 linetype = "dashed", linewidth = 0.8) +
      geom_vline(xintercept = best_k, color = ACCENT2,
                 linetype = "dotted", linewidth = 0.7) +
      annotate("text", x = actual_k + 0.35, y = max(r$sil) * 1.05,
               label = paste0("k = ", actual_k),
               color = ACCENT3, size = 3.5, fontface = "bold") +
      annotate("text", x = best_k - 0.35, y = min(r$sil) * 0.95,
               label = paste0("Optimal: k=", best_k),
               color = ACCENT2, size = 3, hjust = 1) +
      scale_x_continuous(breaks = 2:8) +
      labs(x = "k", y = "Avg Silhouette Width") +
      theme_dark_dash(base_size = 11)
    ggplotly(p, tooltip = c("x", "y")) %>% plotly_dark()
  })

  # Cluster profile bar chart
  output$cl_profile <- renderPlotly({
    r <- cl_result(); req(r)
    ml <- r$cm %>%
      select(-count) %>%
      pivot_longer(-cluster, names_to = "var", values_to = "val") %>%
      group_by(var) %>%
      mutate(sv = (val - min(val)) /
               (max(val) - min(val) + 1e-10)) %>%
      ungroup() %>%
      mutate(dn = gsub("_", " ", var))
    p <- ggplot(ml, aes(
      x = dn, y = sv, fill = cluster,
      text = paste0("Cluster ", cluster, " | ", dn,
                    "\nRaw Mean: ", round(val, 2),
                    "\nNormalized: ", round(sv, 2)))) +
      geom_col(position = "dodge", alpha = 0.85, width = 0.7) +
      scale_fill_manual(values = cluster_pal) +
      coord_flip() +
      labs(x = NULL, y = "Normalized Value", fill = "Cluster") +
      theme_dark_dash()
    ggplotly(p, tooltip = "text") %>% plotly_dark()
  })

  # Income distribution per cluster
  output$cl_demo <- renderPlotly({
    r <- cl_result(); req(r)
    d <- r$df %>%
      count(cluster, income_bracket) %>%
      group_by(cluster) %>%
      mutate(pct = n / sum(n))
    p <- ggplot(d, aes(
      x = cluster, y = pct, fill = income_bracket,
      text = paste0("Cluster ", cluster, " | ", income_bracket,
                    ": ", round(pct * 100, 1), "%"))) +
      geom_col(position = "fill", alpha = 0.85, width = 0.6) +
      scale_fill_manual(values = income_colors) +
      scale_y_continuous(labels = label_percent()) +
      labs(x = NULL, y = NULL, fill = "Income") +
      theme_dark_dash()
    ggplotly(p, tooltip = "text") %>%
      plotly_dark(margin = list(l = 40, r = 20, t = 20, b = 70)) %>%
      layout(legend = list(orientation = "h", y = -0.22,
                           x = 0.5, xanchor = "center",
                           tracegroupgap = 4))
  })

  # Cluster size warning
  output$cl_size_warning <- renderUI({
    r <- cl_result(); req(r)
    sizes <- r$cm$count
    total <- sum(sizes)
    small <- which(sizes < total * 0.02)
    warnings <- tagList()

    # Large-sample approximation notice for hierarchical clustering
    if (input$cluster_method == "hclust" && total > 10000) {
      warnings <- tagList(warnings, tags$div(
        style = paste0("background:", ACCENT4, "22;border:1px solid ", ACCENT4,
                       ";border-radius:6px;padding:8px 12px;margin-bottom:10px;",
                       "font-size:0.82rem;color:", ACCENT4, ";"),
        tags$strong("\u2139 Large Sample Mode: "),
        paste0("Hierarchical clustering was run on a 10,000-row sample ",
               "and remaining points were assigned via nearest centroid. ",
               "Results are approximate.")
      ))
    }

    if (length(small) > 0) {
      warnings <- tagList(warnings, tags$div(
        style = paste0("background:#fb718533;border:1px solid #fb7185;",
                       "border-radius:6px;padding:8px 12px;margin-bottom:10px;",
                       "font-size:0.82rem;color:#fb7185;"),
        tags$strong("\u26A0 Small Cluster Warning: "),
        paste0("Cluster ", paste(small, collapse = ", "),
               " has very few members (",
               paste(sizes[small], collapse = ", "),
               "). Consider reducing k or using a different algorithm ",
               "for more balanced segmentation.")
      ))
    }
    if (length(warnings) > 0) warnings else NULL
  })

  # Cluster table
  output$cl_table <- renderDT({
    r <- cl_result(); req(r)
    d <- r$cm %>% rename(Cluster = cluster, Count = count)
    num_cols <- names(d)[sapply(d, is.numeric) & names(d) != "Count"]
    datatable(d, options = list(pageLength = 10, scrollX = TRUE,
                                dom = "t"),
              rownames = FALSE, class = "compact stripe hover") %>%
      formatRound(columns = num_cols, digits = 2) %>%
      formatStyle("Count", fontWeight = "bold", color = ACCENT)
  })

  output$cl_download <- downloadHandler(
    filename = function() paste0("cluster_centroids_", Sys.Date(), ".csv"),
    content  = function(file) {
      r <- cl_result(); req(r)
      write.csv(r$cm, file, row.names = FALSE)
    }
  )

  # ===== MODULE 3: PREDICTION =====

  # Auto-set lambda slider to lambda.min when model type changes
  observeEvent(input$pred_type, {
    mod <- model_pre[[input$pred_type]]
    cv_obj <- mod$cv
    n_lam <- length(cv_obj$lambda)
    # Find index of lambda.min, map to 1-100 scale
    lmin_idx <- which.min(abs(cv_obj$lambda - cv_obj$lambda.min))
    slider_val <- round(lmin_idx * 100 / n_lam)
    updateSliderInput(session, "pred_lambda", value = slider_val)
  })

  # Current model reactive (pre-trained, instant switch)
  cur_model <- reactive({
    mt  <- input$pred_type
    mod <- model_pre[[mt]]
    cv_obj  <- mod$cv
    fit_obj <- mod$fit
    X <- model_pre$X
    y <- model_pre$y

    # Map lambda slider (1-100) to actual lambda index
    n_lam <- length(cv_obj$lambda)
    li    <- max(1, min(round(input$pred_lambda * n_lam / 100), n_lam))
    lam   <- cv_obj$lambda[li]

    # Predictions at selected lambda
    pr  <- c(predict(cv_obj, newx = X, s = lam))
    res <- c(y - pr)
    rsq  <- 1 - sum(res^2) / sum((y - mean(y))^2)
    rmse <- sqrt(mean(res^2))

    # Coefficients
    co  <- coef(cv_obj, s = lam)
    cdf <- data.frame(
      variable = rownames(co)[-1],
      coefficient = c(co[-1, 1]),
      stringsAsFactors = FALSE) %>%
      filter(abs(coefficient) > 1e-8) %>%
      arrange(desc(abs(coefficient)))

    list(cv = cv_obj, fit = fit_obj, cdf = cdf,
         rsq = rsq, rmse = rmse,
         pr = pr, y = y, res = res, lam = lam,
         xm = model_pre$X_means, xn = model_pre$X_colnames)
  })

  # Reset simulator sliders
  observeEvent(input$sim_reset, {
    updateSliderInput(session, "sim_age",             value = 45)
    updateSliderInput(session, "sim_tx_count",        value = 50)
    updateSliderInput(session, "sim_avg_value",       value = 2e6)
    updateSliderInput(session, "sim_satisfaction",    value = 5)
    updateSliderInput(session, "sim_nps",             value = -20)
    updateSliderInput(session, "sim_credit_util",     value = 0.3)
    updateSliderInput(session, "sim_tenure",          value = 10)
    updateSliderInput(session, "sim_active_products", value = 3)
    updateSliderInput(session, "sim_support_tickets", value = 2)
    updateSliderInput(session, "sim_volatility",      value = 1e6)
    updateSliderInput(session, "sim_household",       value = 3)
  })

  # Lambda explanation panel
  output$pred_lambda_info <- renderUI({
    m <- cur_model(); req(m)
    cv_obj <- m$cv
    lam <- m$lam
    lam_min <- cv_obj$lambda.min
    lam_1se <- cv_obj$lambda.1se
    mt <- input$pred_type
    model_name <- if (mt == "lasso") "Lasso (L1)" else "Ridge (L2)"

    # Determine where the selected lambda sits
    position <- if (abs(lam - lam_min) / lam_min < 0.1) {
      paste0("at \u03bb.min \u2014 the regularization strength that minimizes ",
             "10-fold cross-validation error. This is the default optimal choice.")
    } else if (abs(lam - lam_1se) / lam_1se < 0.1) {
      paste0("at \u03bb.1se \u2014 the largest \u03bb within 1 standard error of the ",
             "minimum. A more parsimonious model that trades marginal accuracy ",
             "for fewer active predictors.")
    } else if (lam < lam_min) {
      paste0("below \u03bb.min \u2014 lower regularization retains more model ",
             "complexity. Useful for exploring full feature contributions, ",
             "but cross-validation error is slightly higher.")
    } else if (lam > lam_1se) {
      paste0("above \u03bb.1se \u2014 stronger regularization produces a sparser model. ",
             "Fewer features are active, which aids interpretability at the ",
             "cost of higher prediction error.")
    } else {
      paste0("between \u03bb.min and \u03bb.1se \u2014 balancing predictive accuracy ",
             "with model parsimony.")
    }

    n_nonzero <- nrow(m$cdf)
    total_feat <- length(m$xn)

    tags$div(
      style = paste0("background:#1a1a1a;border:1px solid ", BORDER,
                     ";border-radius:8px;padding:10px 14px;margin-bottom:12px;",
                     "font-size:0.82rem;color:", TEXT_DIM, ";"),
      tags$div(
        style = paste0("font-weight:700;color:", ACCENT,
                       ";margin-bottom:4px;font-size:0.78rem;",
                       "text-transform:uppercase;letter-spacing:0.05em;"),
        paste0(model_name, " \u2014 \u03bb Regularization")),
      tags$div(
        tags$span(style = paste0("color:", TEXT_MAIN, ";"),
                  paste0("\u03bb = ", formatC(lam, format = "e", digits = 3))),
        " \u2014 ", position),
      tags$div(
        style = "margin-top:4px;",
        paste0("Active features: ", n_nonzero, "/", total_feat,
               if (mt == "lasso" && n_nonzero < total_feat)
                 paste0(" (Lasso zeroed out ",
                        total_feat - n_nonzero, " irrelevant features)")
               else ""))
    )
  })

  # KPIs
  output$pred_rsq  <- renderText({
    m <- cur_model(); paste0(round(m$rsq, 4))
  })
  output$pred_rmse <- renderText({
    m <- cur_model(); paste0(round(m$rmse, 4))
  })

  # Variable importance
  output$pred_imp <- renderPlotly({
    m <- cur_model(); req(nrow(m$cdf) > 0)
    d <- head(m$cdf, 15)
    d$variable <- factor(d$variable, levels = rev(d$variable))
    d$dir <- ifelse(d$coefficient > 0,
                    "Increases Churn", "Decreases Churn")
    p <- ggplot(d, aes(
      x = variable, y = coefficient, fill = dir,
      text = paste0(gsub("_", " ", variable), "\nCoef: ",
                    formatC(coefficient, format = "e", digits = 2)))) +
      geom_col(alpha = 0.85, width = 0.65) + coord_flip() +
      scale_fill_manual(
        values = c("Increases Churn" = ACCENT3,
                   "Decreases Churn" = ACCENT2)) +
      scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
      labs(x = NULL, y = NULL, fill = NULL) + theme_dark_dash()
    ggplotly(p, tooltip = "text") %>%
      plotly_dark(margin = list(l = 50, r = 20, t = 45, b = 20)) %>%
      layout(legend = list(orientation = "h", y = 1.05, x = 0.5,
                           xanchor = "center", yanchor = "bottom"))
  })

  # CV Performance plot (lambda vs MSE)
  output$pred_cv <- renderPlotly({
    m <- cur_model()
    cv_obj <- m$cv
    d <- data.frame(
      log_lambda = log(cv_obj$lambda),
      mse        = cv_obj$cvm,
      mse_lo     = cv_obj$cvlo,
      mse_hi     = cv_obj$cvup
    )
    p <- ggplot(d, aes(x = log_lambda, y = mse)) +
      geom_ribbon(aes(ymin = mse_lo, ymax = mse_hi),
                  fill = ACCENT, alpha = 0.15) +
      geom_line(color = ACCENT, linewidth = 0.9) +
      geom_point(color = ACCENT, size = 1.2, alpha = 0.6) +
      geom_vline(xintercept = log(cv_obj$lambda.min),
                 color = ACCENT2, linetype = "dashed",
                 linewidth = 0.8) +
      geom_vline(xintercept = log(cv_obj$lambda.1se),
                 color = ACCENT4, linetype = "dotted",
                 linewidth = 0.8) +
      geom_vline(xintercept = log(m$lam),
                 color = ACCENT3, linetype = "dashed",
                 linewidth = 0.9) +
      annotate("text", x = log(cv_obj$lambda.min),
               y = max(d$mse) * 0.95,
               label = "\u03bb.min", color = ACCENT2,
               size = 3, hjust = -0.2) +
      annotate("text", x = log(cv_obj$lambda.1se),
               y = max(d$mse) * 0.88,
               label = "\u03bb.1se", color = ACCENT4,
               size = 3, hjust = -0.2) +
      annotate("text", x = log(m$lam),
               y = max(d$mse) * 0.81,
               label = "Selected", color = ACCENT3,
               size = 3, hjust = -0.2) +
      labs(x = "Log(\u03bb)", y = "Mean Squared Error (CV)") +
      theme_dark_dash()
    ggplotly(p) %>% plotly_dark()
  })

  # Predicted vs Actual
  output$pred_scatter <- renderPlotly({
    m <- cur_model()
    d <- data.frame(actual = c(m$y), predicted = c(m$pr))
    if (nrow(d) > 5000) {
      set.seed(42); d <- d[sample(nrow(d), 5000), ]
    }
    p <- ggplot(d, aes(x = actual, y = predicted)) +
      geom_point(alpha = 0.12, size = 0.8, color = ACCENT) +
      geom_abline(slope = 1, intercept = 0, color = ACCENT3,
                  linetype = "dashed", linewidth = 0.7) +
      labs(x = "Actual Churn Probability",
           y = "Predicted Churn Probability") +
      theme_dark_dash()
    ggplotly(p) %>% plotly_dark()
  })

  # Residuals
  output$pred_resid <- renderPlotly({
    m <- cur_model()
    res_vec <- c(m$res)
    if (length(res_vec) > 10000) {
      set.seed(42)
      res_vec <- res_vec[sample(length(res_vec), 10000)]
    }
    d <- data.frame(residual = res_vec)
    p <- ggplot(d, aes(x = residual)) +
      geom_histogram(bins = 50, fill = ACCENT, alpha = 0.75,
                     color = BG_CARD, linewidth = 0.3) +
      geom_vline(xintercept = 0, color = ACCENT3,
                 linetype = "dashed", linewidth = 0.7) +
      labs(x = "Residual (Actual - Predicted)", y = "Frequency") +
      theme_dark_dash()
    ggplotly(p) %>% plotly_dark()
  })

  # What-If Simulator
  sim_pred <- reactive({
    m <- cur_model()
    nx <- build_sim_input(input, m$xn, m$xm)
    p  <- c(predict(m$cv, newx = nx, s = m$lam))
    max(0, min(1, p[1]))
  })

  output$sim_churn <- renderText(fmt_pct(sim_pred()))
  output$sim_risk  <- renderText(classify_risk(sim_pred()))

  # Recommendations
  output$sim_recommend <- renderUI({
    p <- sim_pred()
    risk <- classify_risk(p)

    recs <- switch(risk,
      "HIGH" = list(
        icon_el = icon("circle-exclamation"),
        title   = "High Risk \u2014 Immediate Action Needed",
        items   = list(
          paste0("Increase active products: the strongest single ",
                 "negative predictor of churn"),
          paste0("Improve satisfaction score: proactively collect ",
                 "feedback and resolve open support tickets"),
          paste0("Boost app login frequency: push personalized ",
                 "offers or highlight unused features")
        )
      ),
      "MEDIUM" = list(
        icon_el = icon("triangle-exclamation"),
        title   = "Medium Risk \u2014 Monitor & Nurture",
        items   = list(
          "Schedule a periodic check-in or net promoter survey",
          paste0("Watch credit utilization \u2014 flag and ",
                 "intervene if it approaches 70%"),
          paste0("Encourage adoption of additional products to ",
                 "deepen engagement and switching cost")
        )
      ),
      "LOW" = list(
        icon_el = icon("circle-check"),
        title   = "Low Risk \u2014 Retain & Grow",
        items   = list(
          paste0("Customer health looks good \u2014 maintain ",
                 "current service quality"),
          paste0("Consider cross-sell opportunities; loyalty ",
                 "and engagement are strong")
        )
      )
    )

    border_col <- switch(risk,
                         "HIGH" = ACCENT3,
                         "MEDIUM" = ACCENT4,
                         "LOW" = ACCENT2)

    tags$div(
      class = "recommend-box",
      style = paste0("border-left:3px solid ", border_col,
                     ";background:", BG_INPUT, ";"),
      tags$div(class = "recommend-title",
               style = paste0("color:", border_col, ";"),
               recs$icon_el, tags$span(recs$title)),
      tags$ul(lapply(recs$items, tags$li))
    )
  })
}

shinyApp(ui = ui, server = server)
