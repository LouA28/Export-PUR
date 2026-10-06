#Load libraries

library(shiny)
library(readxl)
library(writexl)
library(ggplot2)
library(plotly)
library(treemap)
library(treemapify)
library(data.table)
library(DT)
library(dplyr)
library(openxlsx)
library(tidyr)
library(stringr)
library(png)
library(scales)
library(purrr)
library(lubridate)
library(htmlwidgets)
library(htmltools)
library(shinyjs)
library(formattable)
library(sf)
library(leaflet)
library(profvis)
library(shinycssloaders)
library(countrycode)

## ============================================================================
## NOTE FOR WHOEVER MAINTAINS THIS FILE
## EU imports PUR app, ALL PARTNERS version (CN chapters 01-30).
##
## Every data file from the pipeline has a `partner` column. The app shows
## ONE partner at a time, chosen in the bar under the top navigation: each
## session filters every dataset to that partner (see `d()` in the server),
## so all the pages below work exactly as they did in the single-partner
## (UK) app. To add an "All partners" view later, the PUR-only files
## (raw_data("calculateHS2all"), agri_period, Map1, *_timeseries) would need
## Pref_Trade / Eligible_Trade columns added in the pipeline, because PURs
## can't be summed across partners.
##
## Optional data files are still wrapped in `if (file.exists(...))` guards:
## add the matching .RDS later and the relevant dropdown/graph starts working.
## ============================================================================


## ---- Custom UI components -------------------------
## Same function names as the UK app, so the same call sites work unchanged,
## but they render clean GDS-styled HTML instead of AdminLTE/shinydashboard
## widgets.

tabItems <- function(...) tabsetPanel(id = "tabs", type = "hidden", ...)
tabItem  <- function(tabName, ...) tabPanelBody(value = tabName, ...)
updateTabItems <- function(session, inputId, selected) {
  updateTabsetPanel(session, inputId, selected = selected)
}
dashboardBody <- function(...) tagList(...)

box <- function(..., title = NULL, status = NULL, solidHeader = FALSE,
                width = NULL, height = NULL, collapsible = FALSE) {
  div(class = "panel-card",
      if (!is.null(title)) div(class = "panel-card-title", title),
      ...)
}

renderValueBox <- renderUI


## ---- Load RDS files ----

## Core data (all partners). Files are read the FIRST time something needs
## them, not all at start-up, and each one gets a row index by partner so
## filtering is a lookup rather than a scan of every row. This is what keeps
## the app responsive with 200+ partners; nothing about the figures changes.

data_files <- c(
  agri_period           = "data/agri_period.RDS",
  agri_period_sector    = "data/agri_period_sector.RDS",
  summary_data_all      = "data/summary_data_all.RDS",
  preprocessed_data     = "data/preprocessed_PUR_data.RDS",
  preprocessed_agri_PUR = "data/preprocessed_agri_PUR.RDS",
  calculateHS2all       = "data/calculateHS2all.RDS",
  Totalexports          = "data/Totalexports.RDS",
  Totalexports_all      = "data/Totalexports_all.RDS",
  monthly_HS2_1         = "data/monthly_HS2.RDS",
  HS4_timeseries        = "data/HS4_timeseries.RDS",
  HS6_timeseries        = "data/HS6_timeseries.RDS",
  SITC_timeseries       = "data/SITC_timeseries.RDS",
  preprocessed_HS4      = "data/preprocessed_HS4_data.RDS",
  preprocessed_HS6      = "data/preprocessed_HS6_data.RDS",
  preprocessed_CN8      = "data/preprocessed_CN8_data.RDS",
  Map1                  = "data/Map1.RDS",
  Map2                  = "data/Map2.RDS",
  Map3                  = "data/Map3.RDS",
  treemap_eligibility   = "data/treemap_eligibility.RDS",
  treemap_use           = "data/treemap_use.RDS",
  treemap_combination_code = "data/treemap_combination_code.RDS",
  old_data_exports      = "data/old_data_exports.RDS",
  New_data_exports      = "data/New_data_exports.RDS",
  ## optional - the app works with these absent
  preprocessed_SITC     = "data/preprocessed_SITC.RDS",
  CN8_timeseries        = "data/CN8_timeseries.RDS",
  monthly_HS4_country   = "data/monthly_HS4_country.RDS",
  monthly_HS6_country   = "data/monthly_HS6_country.RDS",
  monthly_CN8_country   = "data/monthly_CN8_country.RDS",
  monthly_SITC_country  = "data/monthly_SITC_country.RDS",
  pref_breakdown_code   = "data/pref_breakdown_code.RDS")

.data_cache <- new.env(parent = emptyenv())
.idx_cache  <- new.env(parent = emptyenv())

## whole file (NULL if it doesn't exist), read once and kept
raw_data <- function(name) {
  if (!exists(name, envir = .data_cache)) {
    f <- data_files[[name]]
    assign(name, if (!is.null(f) && file.exists(f)) readRDS(f) else NULL,
           envir = .data_cache)
  }
  get(name, envir = .data_cache)
}

## rows for one partner, via a row index built on first use
partner_rows <- function(name, p) {
  x <- raw_data(name)
  if (is.null(x) || !"partner" %in% names(x)) return(x)
  if (!exists(name, envir = .idx_cache)) {
    assign(name, split(seq_len(nrow(x)), x$partner), envir = .idx_cache)
  }
  i <- get(name, envir = .idx_cache)[[p]]
  if (is.null(i)) x[0, ] else x[i, ]
}

## Preference-regime data: one file per partner, written by 04_pref_data.R.
## Opened only for the partner being viewed.
pref_dir   <- file.path("data", "pref")
pref_ready <- dir.exists(pref_dir) && file.exists("data/pref_labels.RDS")

pref_labels_all <- function() {
  if (!exists("pref_labels", envir = .data_cache)) {
    assign("pref_labels",
           if (file.exists("data/pref_labels.RDS")) readRDS("data/pref_labels.RDS") else NULL,
           envir = .data_cache)
  }
  get("pref_labels", envir = .data_cache)
}

## detail = TRUE opens the partner's full CN8 file; otherwise the much
## smaller chapter-level file, which covers All products / All agrifood / HS2
pref_for <- function(p, detail = FALSE) {
  key <- paste0("pref:", p, if (detail) ":cn8")
  if (!exists(key, envir = .data_cache)) {
    f <- file.path(pref_dir, paste0(p, if (detail) "_cn8", ".RDS"))
    assign(key, if (file.exists(f)) readRDS(f) else NULL, envir = .data_cache)
  }
  get(key, envir = .data_cache)
}

## regime colours / order, used by the bar, the chart and the table
regime_levels <- c("MFN \u2013 zero duty", "MFN \u2013 duty paid",
                   "PTA preference used", "GSP preference used",
                   "Entry regime unknown", "Not applicable")
regime_cols <- c("MFN \u2013 zero duty"    = "#0b2e4f",
                 "MFN \u2013 duty paid"    = "#1d70b8",
                 "PTA preference used"     = "#00703c",
                 "GSP preference used"     = "#4c9be8",
                 "Entry regime unknown"    = "#b1b4b6",
                 "Not applicable"          = "#d9d9d9")

map_sf_data <- function() {
  if (!exists("map_sf", envir = .data_cache)) {
    assign("map_sf", if (file.exists("data/map_sf_simplified.RDS"))
      readRDS("data/map_sf_simplified.RDS")
      else st_read("ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"),
      envir = .data_cache)
  }
  get("map_sf", envir = .data_cache)
}

## Small lookups file (written by 03_app_data.R): years, member states, code
## descriptions, partners. With it, start-up never opens the big data files.
app_lookups <- if (file.exists("data/app_lookups.RDS")) readRDS("data/app_lookups.RDS") else NULL

## ---- Partners ----
## Partner codes are Comext's (mostly ISO-2). Names come from countrycode,
## then three edits, all in one place so they're easy to change:
##   1. hidden_partners: Comext catch-all codes that aren't countries
##      (high seas, not specified, etc.) - left out of the dropdown
##   2. comext_names: Comext codes countrycode doesn't know
##   3. name_fixes: countrycode names changed to GOV.UK / FCDO style
## Any code still unrecognised shows as the code itself.

hidden_partners <- c("QP", "QQ", "QR", "QS", "QU", "QV", "QW", "QX", "QY", "QZ")

comext_names <- c(
  "XS" = "Serbia",
  "XK" = "Kosovo",
  "XC" = "Ceuta",
  "XL" = "Melilla")

name_fixes <- c(
  "Congo - Brazzaville"                        = "Congo",
  "Congo - Kinshasa"                           = "Democratic Republic of the Congo",
  "Hong Kong SAR China"                        = "Hong Kong",
  "Macao SAR China"                            = "Macao",
  "C\u00f4te d\u2019Ivoire"                    = "Ivory Coast",
  "Palestinian Territories"                    = "Occupied Palestinian Territories",
  "Micronesia (Federated States of)"           = "Micronesia",
  "United States Minor Outlying Islands (the)" = "United States Minor Outlying Islands",
  "Heard & McDonald Islands"                   = "Heard Island and McDonald Islands",
  "Caribbean Netherlands"                      = "Bonaire, Sint Eustatius and Saba",
  "U.S. Virgin Islands"                        = "United States Virgin Islands",
  "Bahamas"                                    = "The Bahamas",
  "Gambia"                                     = "The Gambia")

partner_codes <- setdiff(
  if (!is.null(app_lookups)) app_lookups$partners
  else sort(unique(raw_data("preprocessed_agri_PUR")$partner)),
  hidden_partners)
partner_names <- countrycode(partner_codes, "iso2c", "country.name", warn = FALSE)
partner_names <- ifelse(is.na(partner_names), comext_names[partner_codes], partner_names)
partner_names <- ifelse(is.na(partner_names), partner_codes, partner_names)
fix <- match(partner_names, names(name_fixes))
partner_names <- ifelse(is.na(fix), partner_names, name_fixes[fix])
partner_names <- gsub(" & ", " and ", partner_names)   # "Antigua & Barbuda"
partner_names <- sub("^St\\. ", "St ", partner_names)   # "St. Lucia" -> "St Lucia"

## alphabetical, ignoring a leading "The" (so The Bahamas sits under B)
partner_choices <- setNames(partner_codes, partner_names)[order(sub("^The ", "", partner_names))]
default_partner <- if ("GB" %in% partner_codes) "GB" else partner_codes[1]

## Revisions data (kept from the old EU app, restyled only) -- built per
## partner inside the server, from old_data_exports / New_data_exports
n_vintages_shown <- 4
make_revisions <- function(old, new) {
  d <- bind_rows(old, new)
  if (nrow(d) == 0) return(d)
  d <- mutate(d, Date = paste(year, month, sep = "-"))
  d$Date <- ymd(paste(d$Date, "01", sep = "-"))
  d$Date <- as.factor(d$Date)
  stamp_order <- unique(d$date_stamp)
  stamp_order <- stamp_order[order(as.Date(sub("Eurostat ", "", stamp_order), format = "%d-%m-%Y"))]
  stamp_order <- tail(stamp_order, n_vintages_shown)
  d <- d %>% filter(date_stamp %in% stamp_order)
  d$date_stamp <- factor(d$date_stamp, levels = stamp_order)
  d
}

## combocodes shown in the preference-type breakdown chart (only used if
## pref_breakdown_code.RDS exists)
selected_combocodes <- c("e1u11","e2u11","e2u21","e3u11","e3u31","e3u30")
combocode_colours <- c(
  "e1u11" = "#0b2e4f", "e2u11" = "#1d70b8", "e2u21" = "#4c9be8",
  "e3u11" = "#7fb8e0", "e3u31" = "#00b0d8", "e3u30" = "#003078")
combocode_labels <- c(
  "e1u11" = "MFN Only: MFN non-zero",
  "e2u11" = "GSP: MFN non-zero",
  "e2u21" = "GSP: GSP non-zero",
  "e3u11" = "TCA: MFN non-zero",
  "e3u31" = "TCA: TCA non-zero",
  "e3u30" = "TCA: TCA zero")

##################### Map preparation #############

## The EU app currently only has one map dataset (Map1 = PUR). If you later
## add MFN/FTA non-use maps in the same shape as the UK app's Map2/Map3, drop
## them in as data/Map2.RDS / data/Map3.RDS and the "maptype" selector in the
## Explorer overview will pick them up automatically.
## Basemap: Esri World Gray Canvas -- light grey, no API key needed
## (CARTO basemaps now need a key, which caused the watermarks)
map_tiles <- providers$Esri.WorldGrayCanvas

## Map view. eu_bounds is the area the map opens on: roughly mainland EU,
## from southern Cyprus/Malta up to northern Finland. Widen or narrow these
## four numbers to change the framing. (A bounding box is used rather than
## the member states' own extent, because Natural Earth includes France's
## overseas departments, which would pull the view out to South America.)
## max_bounds is how far a user can pan before the map springs back.
eu_bounds  <- list(lng1 = -11, lat1 = 34, lng2 = 32, lat2 = 71)
max_bounds <- list(lng1 = -35, lat1 = 20, lng2 = 60, lat2 = 82)

## applied to all three maps
frame_europe <- function(map) {
  map %>%
    fitBounds(eu_bounds$lng1, eu_bounds$lat1, eu_bounds$lng2, eu_bounds$lat2) %>%
    setMaxBounds(max_bounds$lng1, max_bounds$lat1, max_bounds$lng2, max_bounds$lat2)
}

createmap_data <- function(data) data %>% filter(!is.na(agri_PUR))

createmap_plot <- function(data1, partner_name) {
  scale_range <- c(0, 100)
  pal <- colorNumeric(palette = "Blues", domain = scale_range)
  leaflet(options = leafletOptions(minZoom = 3)) %>%
    addProviderTiles(map_tiles) %>%
    frame_europe() %>%
    addPolygons(data = data1, fillColor = ~pal(agri_PUR), color = "#1d70b8", weight = 1.2,
                fillOpacity = 1,
                label = ~paste("Country:", NAME, ". PUR:", agri_PUR, "%"),
                highlight = highlightOptions(weight = 2, color = "white", bringToFront = FALSE)) %>%
    addLegend("topright", pal = pal, values = scale_range, labFormat = labelFormat(suffix = "%"),
              title = paste("EU import PUR from", partner_name),
              opacity = 1)
}

createmap_data2 <- function(data) return(data)
createmap_plot2 <- function(data1, partner_name) {
  pal <- colorBin(palette = "Blues", domain = data1$nonuse,
                  bins = c(0, 50, 100, 150, 200, 250, 500, Inf))
  leaflet(options = leafletOptions(minZoom = 3)) %>%
    addProviderTiles(map_tiles) %>%
    frame_europe() %>%
    addPolygons(data = data1, fillColor = ~pal(nonuse), color = "#1d70b8", weight = 1.2,
                fillOpacity = 1,
                label = ~paste0("Country:", NAME, ". MFN Non-zero: \u20ac", nonuse, "m"),
                highlight = highlightOptions(weight = 2, color = "white", bringToFront = FALSE)) %>%
    addLegend("topright", pal = pal, values = data1$nonuse, labFormat = labelFormat(prefix = "\u20ac", suffix = "m"),
              title = paste("EU imports from", partner_name, "under MFN non-zero use"),
              opacity = 1)
}

createmap_data3 <- function(data) return(data)
createmap_plot3 <- function(data1, partner_name) {
  pal <- colorBin(palette = "Blues", domain = data1$nonuse,
                  bins = c(0, 50, 100, 150, 200, 250, 500, Inf))
  leaflet(options = leafletOptions(minZoom = 3)) %>%
    addProviderTiles(map_tiles) %>%
    frame_europe() %>%
    addPolygons(data = data1, fillColor = ~pal(nonuse), color = "#1d70b8", weight = 1.2,
                fillOpacity = 1,
                label = ~paste0("Country:", NAME, ". FTA Non-zero: \u20ac", nonuse, "m"),
                highlight = highlightOptions(weight = 2, color = "white", bringToFront = FALSE)) %>%
    addLegend("topright", pal = pal, values = data1$nonuse, labFormat = labelFormat(prefix = "\u20ac", suffix = "m"),
              title = paste("EU imports from", partner_name, "under FTA non-zero use"),
              opacity = 1)
}

years_PUR <- if (!is.null(app_lookups)) {
  app_lookups$map_years
} else {
  sort(unique(createmap_data(raw_data("Map1"))$year))
}
maptype_choices <- c("PUR" = "PUR")
if (!is.null(raw_data("Map2"))) maptype_choices <- c(maptype_choices, "MFN Non-zero use" = "MFN")
if (!is.null(raw_data("Map3"))) maptype_choices <- c(maptype_choices, "FTA Non-zero use" = "FTA")


####################################### The app  #################################################

body <- dashboardBody(
  tags$head(tags$style(HTML("
    .content-wrapper, .right-side { background-color: #ffffff !important; }
    .tile-page { padding: 26px 36px; }
    .tile-hero { display:flex; align-items:center; gap:16px; margin-bottom:20px; }
    .tile-hero-icon { width:52px; height:52px; border-radius:10px;
      background:#1d70b8; color:#ffffff; font-size:26px; font-weight:700;
      display:flex; align-items:center; justify-content:center; }
    .tile-hero h1 { font-size:24px; font-weight:700; color:#0b0c0c; margin:0; }
    .tile-hero p { font-size:14px; color:#505a5f; margin:4px 0 0; }
    .tile-row { display:grid; grid-template-columns:repeat(3, 1fr);
      gap:16px; margin-bottom:26px; }
    a.tile-link { text-decoration:none !important; }
    .tile { background:#ffffff; border:1px solid #d9d9d9; border-radius:10px;
      padding:18px 20px; min-height:180px; transition:all .12s ease-in-out; }
    a.tile-link:hover .tile { border-color:#1d70b8;
      box-shadow:0 2px 8px rgba(0,0,0,0.10); transform:translateY(-2px); }
    .tile-icon { font-size:26px; color:#1d70b8; }
    .tile h3 { font-size:17px; font-weight:700; color:#0b0c0c; margin:10px 0 4px; }
    .tile p { font-size:13px; color:#505a5f; margin:0 0 12px; line-height:1.45; }
    .tile-preview { background:#f3f2f1; border-radius:6px; padding:7px 10px;
      font-size:12px; color:#505a5f; }
    .tile-bars { display:flex; gap:4px; align-items:flex-end; height:38px; }
    .tile-bars div { flex:1; border-radius:2px; }
    .govuk-h2 { color:#0b0c0c; font-size:20px; font-weight:700;
      margin:26px 0 10px; border-top:1px solid #d9d9d9; padding-top:16px;
      max-width:820px; }
    .govuk-body, .tile-page li { color:#0b0c0c; font-size:14px;
      line-height:1.6; max-width:820px; }
    .govuk-warning { display:flex; gap:14px; align-items:flex-start;
      margin:14px 0; max-width:820px; }
    .govuk-warning-icon { min-width:32px; height:32px; border-radius:50%;
      background:#0b0c0c; color:#ffffff; font-weight:700; font-size:20px;
      display:flex; align-items:center; justify-content:center; }
    .govuk-warning-text { font-weight:700; font-size:14px; color:#0b0c0c;
      margin:4px 0 0; }
    .govuk-inset { background:#f3f2f1; border-left:8px solid #b1b4b6;
      padding:11px 15px; margin:14px 0; max-width:820px; }
    .govuk-inset p { margin:0; font-size:13.5px; color:#0b0c0c; }
    .govuk-meta { border-top:1px solid #d9d9d9; margin-top:24px;
      padding-top:12px; color:#505a5f; font-size:12.5px; }
    .content-wrapper { margin-left: 0 !important; }
    .main-sidebar, .main-header { display: none !important; }
    .formula-wrap { max-width:820px; }
    .topnav { background:#ffffff; border-bottom:2px solid #0b0c0c;
      padding: 0 36px; }
    .topnav-inner { max-width:1240px; margin:0 auto; display:flex;
      align-items:center; justify-content:space-between; height:58px; }
    .topnav-brand { font-size:17px; font-weight:700; color:#0b0c0c; }
    .topnav-ver { font-size:10px; color:#b1b4b6; margin-left:8px; }
    .topnav-tag { font-size:11px; font-weight:700; color:#ffffff;
      background:#1d70b8; border-radius:4px; padding:2px 8px;
      margin-left:10px; vertical-align:2px; }
    .topnav-links a { font-size:14.5px; color:#505a5f; margin-left:26px;
      text-decoration:none !important; padding-bottom:18px; }
    .topnav-links a:hover { color:#0b0c0c; }
    .topnav-links a.active { color:#1d70b8; font-weight:700;
      border-bottom:3px solid #1d70b8; }
    .topnav-hidden-link { font-size:11.5px; color:#8a9299; margin-left:22px;
      text-decoration:none !important; }
    .topnav-hidden-link:hover { color:#505a5f; text-decoration:underline !important; }
    .partner-bar { background:#f0f6fb; border-bottom:1px solid #cfe0f0;
      padding:8px 36px; }
    .partner-bar-inner { max-width:1240px; margin:0 auto; display:flex;
      align-items:center; gap:12px; }
    .partner-bar-label { font-size:14px; font-weight:700; color:#0b0c0c;
      white-space:nowrap; }
    .partner-bar .form-group { margin:0; min-width:300px; }
    body { background:#ffffff; }
    .container-fluid { padding:0; }
    .panel-card { background:#ffffff; border:1px solid #d9d9d9;
      border-radius:10px; padding:18px 20px; margin-bottom:16px; }
    .panel-card-title { font-size:16px; font-weight:700; color:#0b0c0c;
      border-bottom:2px solid #1d70b8; padding-bottom:8px; margin-bottom:14px; }
    .stat-card { background:#f0f6fb; border:1px solid #cfe0f0;
      border-top:4px solid #1d70b8;
      border-radius:8px; padding:16px 18px 14px; margin-bottom:12px; }
    .stat-card .stat-value { font-size:30px; font-weight:700;
      color:#1d70b8; margin:0; line-height:1.1; }
    .stat-card .stat-label { font-size:12px; color:#505a5f;
      margin:6px 0 0; line-height:1.35; }
    .tab-page { max-width:1240px; margin:0 auto; padding:24px 36px; }
    .info-grid { display:grid; grid-template-columns:1fr 1fr;
      gap:0 44px; align-items:start; }
    .caveat-panel { border:1px solid #d9d9d9; border-left:6px solid #0b0c0c;
      background:#fbfbfa; padding:18px 24px; margin:28px 0 0; }
    .caveat-panel .govuk-h2 { border-top:none; padding-top:0; margin-top:0; }
    .caveat-panel li, .caveat-panel p { max-width:none; }
    .tab-content > .tab-pane { max-width:1800px; margin:0 auto;
      padding:24px 36px; }
    .tile-page { max-width:1240px; margin:0 auto; }
    .tab-content > .tab-pane .tab-pane { max-width:none; padding:0; }
    .tile-page { padding:0; }
    .stat-card { min-height:118px; display:flex; flex-direction:column;
      justify-content:center; }
    .stat-grid { display:grid; grid-template-columns:repeat(4, 1fr);
      gap:14px; margin:0 0 16px; }
    .stat-grid .stat-card { margin-bottom:0; }
    .page-head { margin:14px 0 14px; }
    .page-head h2 { font-size:22px; font-weight:700; color:#0b0c0c;
      margin:4px 0 2px; }
    .page-head p { font-size:13px; color:#505a5f; margin:0; }
    .kpi-strip { background:#ffffff; border:2px solid #1d70b8;
      border-radius:10px; display:grid; grid-template-columns:repeat(3, 1fr);
      margin-bottom:14px; }
    .kpi-cell { padding:16px 22px; display:flex; align-items:center; gap:14px;
      border-right:1px solid #cfe0f0; }
    .kpi-cell:last-child { border-right:none; }
    .kpi-icon { width:42px; height:42px; min-width:42px; border-radius:50%;
      background:#eaf2fa; color:#1d70b8; font-size:17px;
      display:flex; align-items:center; justify-content:center; }
    .kpi-cell .kpi-value { font-size:27px; font-weight:700; color:#1d70b8;
      margin:0; line-height:1.1; }
    .kpi-cell .kpi-label { font-size:12px; color:#505a5f; margin:3px 0 0; }
    .builder-cta { background:#ffffff; border:1px solid #cfe0f0;
      border-radius:10px; padding:14px 20px;
      margin-bottom:16px; display:flex; align-items:center;
      justify-content:space-between; gap:20px; }
    .cta-title { font-size:16px; font-weight:700; color:#0b0c0c; margin:0; }
    .cta-sub { font-size:12.5px; color:#505a5f; margin:3px 0 0; max-width:640px; }
    .btn-cta { background:#1d70b8; color:#ffffff; border:none; font-weight:700;
      padding:10px 22px; border-radius:6px; white-space:nowrap; }
    .btn-cta:hover, .btn-cta:focus { background:#003078; color:#ffffff; }
    .back-link { color:#1d70b8; font-weight:600; font-size:13.5px; }
    .equal-row .row { display:flex; flex-wrap:wrap; }
    .equal-row .row > [class*='col-'] { display:flex; }
    .equal-row .panel-card { flex:1; width:100%; }
    .card-note { display:block; font-size:11.5px; font-weight:400;
      color:#6f777b; margin-top:2px; }
    .btn-build { background:#1d70b8; color:#ffffff; border:none;
      font-weight:700; margin-right:8px; }
    .btn-build:hover, .btn-build:focus { background:#003078; color:#ffffff; }
    .btn-dl { margin-top:0; }
    /* grouped header on the regime table: each block gets its own colour,
       carried down into the body cells so the columns stay distinguishable */
    table.dataTable thead th.grp-top, table.dataTable thead th.grp-total,
    table.dataTable thead th.grp-mfn, table.dataTable thead th.grp-pref,
    table.dataTable thead th.grp-pct {
      text-align: center; font-size: 12px; padding: 5px 8px; }
    table.dataTable thead th.grp-top {
      background: #f0f6fb; font-weight: 700; color: #0b0c0c; }
    table.dataTable thead tr:nth-child(2) th.grp-mfn,
    table.dataTable thead tr:nth-child(2) th.grp-pref,
    table.dataTable thead th.grp-total {
      font-weight: 700; color: #0b0c0c; }
    table.dataTable thead tr:nth-child(2) th.grp-mfn {
      background: #d9e8f5; border-bottom: 3px solid #1d70b8; }
    table.dataTable thead tr:nth-child(2) th.grp-pref {
      background: #d9efe2; border-bottom: 3px solid #00703c; }
    table.dataTable thead th.grp-pct {
      background: #ffffff; border-bottom: 3px solid #505a5f;
      font-weight: 700; color: #0b0c0c; }
    table.dataTable thead tr:last-child th.grp-mfn  { background: #eef3f9; }
    table.dataTable thead tr:last-child th.grp-pref { background: #edf7f0; }
    table.dataTable thead tr:last-child th.grp-pct  {
      background: #ffffff; font-weight: 400; }
    #b_output .radio { margin-bottom: 10px; }
    #b_output .card-note { max-width: 235px; line-height: 1.35; }
  "))),
  uiOutput("topnav"),
  
  ## Partner selector -- applies to every page
  div(class = "partner-bar", div(class = "partner-bar-inner",
                                 span(class = "partner-bar-label", "EU imports from:"),
                                 selectizeInput("partner", NULL, choices = partner_choices,
                                                selected = default_partner)
  )),
  
  tabItems(
    
    tabItem(tabName = "Overview",
            div(class = "tile-page",
                div(class = "tile-hero",
                    div(class = "tile-hero-icon", "%"),
                    div(
                      h1(textOutput("hero_title", inline = TRUE)),
                      p(textOutput("hero_line", inline = TRUE))
                    )
                ),
                div(class = "tile-row",
                    actionLink("go_map", class = "tile-link", label = div(class = "tile",
                                                                          icon("map", class = "tile-icon"),
                                                                          h3("Compare member states"),
                                                                          p("Map of PURs across EU member states"),
                                                                          div(class = "tile-bars",
                                                                              div(style = "height:55%; background:#b8d8f2;"),
                                                                              div(style = "height:85%; background:#5694c9;"),
                                                                              div(style = "height:40%; background:#d6e8f7;"),
                                                                              div(style = "height:95%; background:#1d70b8;"),
                                                                              div(style = "height:70%; background:#5694c9;"),
                                                                              div(style = "height:60%; background:#b8d8f2;")
                                                                          )
                    )),
                    actionLink("go_ts", class = "tile-link", label = div(class = "tile",
                                                                         icon("chart-line", class = "tile-icon"),
                                                                         h3("Track trends"),
                                                                         p("PURs over the full publication series, by chapter or SITC"),
                                                                         plotOutput("tile_spark", height = "38px")
                    )),
                    actionLink("go_app", class = "tile-link", label = div(class = "tile",
                                                                          icon("magnifying-glass", class = "tile-icon"),
                                                                          h3("Look up a PUR"),
                                                                          p("Pick a member state and product, drill down to HS4, HS6 or CN8 level"),
                                                                          div(class = "tile-preview", textOutput("tile_example", inline = TRUE))
                    ))
                ),
                div(class = "info-grid",
                    div(
                      h2(class = "govuk-h2", "What the Preference Utilisation Rate measures"),
                      p(class = "govuk-body",
                        "Trade agreements and unilateral schemes give tariff preferences to imports
               from many of the EU's trading partners, but a preference only applies where the
               goods meet the relevant conditions, such as Rules of Origin, so not all eligible
               trade uses it."),
                      p(class = "govuk-body",
                        "The Preference Utilisation Rate reflects the value of EU imports from a partner
               entering under trade preferences as a share of the total value of imports that were
               eligible for preference. Understanding where preferences are being used - and where
               they are not - can inform work to improve how trade agreements are used."),
                      div(class = "formula-wrap",
                          withMathJax(
                            helpText('$$PUR = \\frac{\\textit{Value of preferential imports}}
                       {\\textit{Value of imports eligible for preferences}}
                       \\times 100$$')))
                    ),
                    div(
                      h2(class = "govuk-h2", "Methodology"),
                      p(class = "govuk-body", strong("Eligibility (the denominator). "),
                        "Imports are eligible for a preference if one or more preferential tariffs were
               available for that good from the partner in the month of reporting, at a rate below
               the MFN tariff that would otherwise apply."),
                      p(class = "govuk-body", strong("Utilisation (the numerator). "),
                        "Imports are recorded as using their preference if they were imported
               under a preferential regime."),
                      p(class = "govuk-body", strong("Exclusions. "),
                        "Imports are excluded from the eligibility total if a preferential tariff
               wouldn't reasonably be used:"),
                      tags$ul(
                        tags$li("Special processing procedures permitting duty-free or
                       reduced-rate entry (inward or outward processing)"),
                        tags$li("Preference-eligible goods that entered duty-free under
                       MFN terms, e.g. suspensions or non-preferential TRQs"),
                        tags$li("Imports where the entry regime is unknown, e.g. due to
                       insufficient customs declaration information")
                      )
                    )
                ),
                div(class = "caveat-panel",
                    h2(class = "govuk-h2", "Key limitations and caveats"),
                    div(class = "govuk-warning",
                        div(class = "govuk-warning-icon", "!"),
                        p(class = "govuk-warning-text",
                          "This tool shows EU imports from one partner at a time - choose the
               partner at the top of the page. Check these notes before citing figures.")
                    ),
                    tags$ul(
                      tags$li("Eurostat publishes data with suppressions, so some values are
                     not reported; this may account for some missing figures."),
                      tags$li("Some member states and chapters show no PUR because no imports
                     were eligible for preferential tariffs in that period."),
                      tags$li("Trade data is revised by Eurostat frequently; figures for
                     recent months may change in later publications - see the Data
                     revisions page.")
                    ),
                    div(class = "govuk-inset",
                        p(textOutput("coverage_note", inline = TRUE))
                    )
                ),
                div(class = "govuk-meta",
                    HTML("App built by Louise Anokye, October 2026 &middot;
                Quality assured: Katie Earl &middot;
                Questions or queries: louise.anokye@defra.gov.uk or
                katie.earl@defra.gov.uk &middot;
                Data: Eurostat, updated monthly"))
            )
    ),
    
    tabItem(tabName = "PUR",
            tabsetPanel(id = "explorer_view", type = "hidden",
                        
                        tabPanelBody(value = "overview",
                                     div(class = "page-head",
                                         h2("Explorer"),
                                         p("All member states and all years at a glance - use the builder below
               for specific countries, periods or products")
                                     ),
                                     uiOutput("kpi_strip"),
                                     div(class = "builder-cta",
                                         div(
                                           p(class = "cta-title", "Create a custom PUR table"),
                                           p(class = "cta-sub", "Choose member states, years and code level
                 (HS2, HS4, HS6 or CN8) to create a custom PUR table you can
                 search and download")
                                         ),
                                         actionButton("open_builder", "Get started \u2192",
                                                      class = "btn-cta")
                                     ),
                                     div(class = "equal-row",
                                         fluidRow(
                                           column(6, div(class = "panel-card",
                                                         div(class = "panel-card-title", "EU average agrifood import PUR over time",
                                                             span(class = "card-note", "agrifood chapters 01\u201324; each point is one
                     year's overall PUR across every member state combined")),
                                                         withSpinner(plotlyOutput("ov_ts", height = "645px"),
                                                                     type = 4, color = "#1d70b8")
                                           )),
                                           column(6, div(class = "panel-card",
                                                         div(class = "panel-card-title", "Average agrifood PUR by HS2 chapter",
                                                             span(class = "card-note", "agrifood chapters 01\u201324, all member
                     states, all years combined")),
                                                         uiOutput("ov_hs2_text"),
                                                         withSpinner(plotlyOutput("ov_hs2", height = "540px"),
                                                                     type = 4, color = "#1d70b8")
                                           ))
                                         )
                                     ),
                                     div(class = "panel-card",
                                         div(class = "panel-card-title", "PUR by member state"),
                                         fluidRow(
                                           column(3, selectInput("maptype", NULL, choices = maptype_choices)),
                                           column(4, sliderInput("yearmap", NULL, min = min(as.numeric(years_PUR)),
                                                                 max = max(as.numeric(years_PUR)),
                                                                 value = max(as.numeric(years_PUR)), sep = ""))
                                         ),
                                         uiOutput("map_text"),
                                         withSpinner(leafletOutput("map", height = "580px"),
                                                     type = 4, color = "#1d70b8")
                                     )
                        ),
                        
                        tabPanelBody(value = "builder",
                                     div(class = "page-head",
                                         actionLink("back_overview",
                                                    "\u2190 Back to Explorer overview", class = "back-link"),
                                         h2("Explorer \u00B7 Custom tables and graphs"),
                                         p("Leave a selector empty to include everything")
                                     ),
                                     fluidRow(
                                       column(3, div(class = "panel-card",
                                                     div(class = "panel-card-title", "Choices"),
                                                     radioButtons("b_output", "What would you like to create?",
                                                                  choiceNames = list(
                                                                    HTML("<strong>Table</strong>
            <span class='card-note'>Trade values you can search,
            filter and download to Excel</span>"),
                                                                    HTML("<strong>Graph</strong>
            <span class='card-note'>Trends over time, downloadable
            as a PNG image</span>")),
                                                                  choiceValues = c("Table", "Graph")),
                                                     radioButtons("b_level", "Code level",
                                                                  choices = c("All agrifood", "All products", "HS section", "HS4", "HS6", "CN8"),
                                                                  selected = "All agrifood", inline = TRUE),
                                                     uiOutput("b_help"),
                                                     selectizeInput("b_country", "Member state",
                                                                    choices = NULL, multiple = TRUE,
                                                                    options = list(placeholder = "All member states \u2013 total (leave empty)")),
                                                     conditionalPanel(
                                                       condition = "input.b_output == 'Table'",
                                                       checkboxInput("b_combined",
                                                                     "Show every individual member state", FALSE)),
                                                     selectizeInput("b_years", "Years",
                                                                    choices = NULL, multiple = TRUE,
                                                                    options = list(placeholder = "Leave empty for default")),
                                                     conditionalPanel(
                                                       condition = "input.b_output == 'Table'",
                                                       checkboxInput("b_byyear", "Show each year separately", FALSE)),
                                                     conditionalPanel(
                                                       condition = "input.b_output == 'Table' &&
               input.b_mode == 'Preference'",
                                                       checkboxInput("p_expand",
                                                                     "Expand duty free / duty paid", FALSE)),
                                                     conditionalPanel(
                                                       condition = "input.b_level == 'HS2' || input.b_level == 'HS4' ||
               input.b_level == 'HS6' ||
               input.b_level == 'CN8' || input.b_level == 'SITC'",
                                                       selectizeInput("b_codes", "Code(s)",
                                                                      choices = NULL, multiple = TRUE,
                                                                      options = list(placeholder = "Type to search codes",
                                                                                     maxOptions = 50))),
                                                     conditionalPanel(
                                                       condition = "input.b_output == 'Graph' &&
                 (input.b_mode == 'Preference' ||
                  input.b_level == 'All agrifood' ||
                  input.b_level == 'All products' ||
                  input.b_level == 'HS section' ||
                  input.b_level == 'HS2')",
                                                       radioButtons("b_freq2", "Frequency",
                                                                    choices = c("Annual", "Monthly"), inline = TRUE)),
                                                     actionButton("b_go", "Create", class = "btn-build"),
                                                     conditionalPanel(
                                                       condition = "input.b_output == 'Table'",
                                                       style = "display:inline-block;",
                                                       downloadButton("b_dl", "Excel", class = "btn-dl")),
                                                     conditionalPanel(
                                                       condition = "input.b_output == 'Graph'",
                                                       style = "display:inline-block;",
                                                       downloadButton("b_png", "PNG", class = "btn-dl"))
                                       )),
                                       column(9,
                                              tabsetPanel(
                                                id = "b_mode", type = "tabs",
                                                
                                                tabPanel(
                                                  "Preference utilisation (PUR)", value = "PUR",
                                                  div(class = "panel-card", style = "border-top-left-radius:0;",
                                                      div(class = "panel-card-title",
                                                          textOutput("b_caption", inline = TRUE)),
                                                      uiOutput("b_explain"),
                                                      conditionalPanel(
                                                        condition = "input.b_output == 'Table'",
                                                        DT::dataTableOutput("b_table")),
                                                      conditionalPanel(
                                                        condition = "input.b_output == 'Graph'",
                                                        p(class = "card-note", "Use the PNG button on the left to download
    this graph with a full title, source and date included."),
                                                        withSpinner(plotlyOutput("b_graph", height = "470px"),
                                                                    type = 4, color = "#1d70b8"),
                                                        uiOutput("b_ctry_note"),
                                                        uiOutput("b_seckey"),
                                                        uiOutput("b_coderegime")))),
                                                
                                                tabPanel(
                                                  "Preference regimes", value = "Preference",
                                                  div(class = "panel-card", style = "border-top-left-radius:0;",
                                                      div(class = "panel-card-title",
                                                          textOutput("p_caption", inline = TRUE)),
                                                      uiOutput("p_sentence"),
                                                      uiOutput("p_strip"),
                                                      uiOutput("p_explain"),
                                                      conditionalPanel(
                                                        condition = "input.b_output == 'Table'",
                                                        DT::dataTableOutput("p_table")),
                                                      conditionalPanel(
                                                        condition = "input.b_output == 'Graph'",
                                                        p(class = "card-note", "Use the PNG button on the left to download
    this graph with a full title, source and date included."),
                                                        withSpinner(plotlyOutput("p_graph", height = "470px"),
                                                                    type = 4, color = "#1d70b8"))))
                                              ))
                                     )
                        )
            )
    ),
    
    ## Revisions - kept, but reached via a small, low-key link in the top
    ## nav rather than a main tile/tab (per your request to keep it "hidden").
    tabItem(tabName = "Revisions",
            div(class = "tab-page",
                div(class = "page-head",
                    actionLink("back_overview_rev", "\u2190 Back to Overview", class = "back-link"),
                    h2("Data revisions"),
                    p(textOutput("rev_sub", inline = TRUE))
                ),
                div(class = "panel-card",
                    p(class = "govuk-body",
                      "Trade data is revised frequently, depending on the national statistical
             authorities' practices. Every month, Eurostat publishes reports showing the
             revisions applied to the monthly EU aggregates as well as to EU countries' data.
             However this includes all their trading partners and so it doesn't show the
             impact of revisions on trade with a particular partner. This page shows how
             the totals of EU imports from the selected partner have changed between
             Eurostat's publications."),
                    withSpinner(plotlyOutput("Revision_graph", height = "480px"),
                                type = 4, color = "#1d70b8")
                )
            )
    )
    
  )
)

server <- function(input, output, session){
  
  ## ---- Partner filtering ----
  ## Every dataset filtered to the chosen partner. All outputs below read
  ## via d(), so each page behaves exactly like the single-partner app.
  partner_sel <- reactive({ req(input$partner); input$partner })
  pname <- reactive(names(partner_choices)[match(partner_sel(), partner_choices)])
  
  ## d("name") gives that dataset for the chosen partner. Filtered once per
  ## partner and kept, so several outputs using the same data cost one filter.
  sess_cache <- new.env(parent = emptyenv())
  observeEvent(input$partner, {
    rm(list = ls(sess_cache), envir = sess_cache)   # drop the previous partner
  }, ignoreInit = TRUE)
  
  d <- function(name) {
    p <- partner_sel()
    if (!exists(name, envir = sess_cache)) {
      assign(name, partner_rows(name, p), envir = sess_cache)
    }
    get(name, envir = sess_cache)
  }
  
  revisions_data <- reactive({
    make_revisions(d("old_data_exports"), d("New_data_exports"))
  })
  
  ## Friendly message for partners with no eligible trade at all
  no_data_msg <- reactive(paste0("No preference-eligible EU imports from ", pname(),
                                 " for this view."))
  
  ## ---- Tiles Overview page ----
  latest_year <- max(as.numeric(raw_data("calculateHS2all")$year), na.rm = TRUE)
  
  output$hero_title <- renderText(paste0("PUR explorer - EU imports from ", pname()))
  
  output$hero_line <- renderText({
    pur <- d("calculateHS2all")$agri_PUR[d("calculateHS2all")$year == latest_year]
    pur <- suppressWarnings(round(as.numeric(pur[1]), 1))
    if (length(pur) == 0 || is.na(pur))
      return(paste0("No preference-eligible trade in ", latest_year,
                    " \u00B7 what would you like to do?"))
    paste0(pur, "% of eligible trade used its preference in ", latest_year,
           " \u00B7 what would you like to do?")
  })
  
  output$tile_example <- renderText({
    ## HS4 rather than CN8: same example, a much smaller file to open
    ex <- d("preprocessed_HS4") %>%
      ungroup() %>%
      filter(year == latest_year, Eligible_Trade > 0, country_name != "EU") %>%
      arrange(desc(Eligible_Trade)) %>% slice(1)
    if (nrow(ex) == 0) return("")
    paste0(ex$country_name, " \u00B7 Ch. ", substr(ex$HS4, 1, 2), " \u2192 ",
           round(ex$Pref_Trade / ex$Eligible_Trade * 100, 1), "%")
  })
  
  output$tile_spark <- renderPlot({
    df <- d("agri_period") %>% filter(!is.na(agri_PUR))
    req(nrow(df) > 0)
    df$perref <- as.Date(as.character(df$period))
    ggplot(df, aes(x = perref, y = agri_PUR)) +
      geom_line(colour = "#1d70b8", linewidth = 1) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
  }, bg = "transparent")
  
  output$coverage_note <- renderText({
    yrs <- sort(unique(as.numeric(raw_data("monthly_HS2_1")$year)))
    last_month <- max(as.numeric(raw_data("monthly_HS2_1")$month[raw_data("monthly_HS2_1")$year == max(yrs)]))
    paste0("This tool currently contains PUR data for January ", min(yrs),
           " to ", month.name[last_month], " ", max(yrs),
           ", CN chapters 01\u201330. The most recent months are provisional and subject to revision.")
  })
  
  observeEvent(input$go_app, {
    updateTabItems(session, "tabs", "PUR")
    updateTabsetPanel(session, "explorer_view", "builder")
    updateRadioButtons(session, "b_output", selected = "Table")
  })
  observeEvent(input$go_ts, {
    updateTabItems(session, "tabs", "PUR")
    updateTabsetPanel(session, "explorer_view", "builder")
    updateRadioButtons(session, "b_output", selected = "Graph")
  })
  observeEvent(input$go_map, {
    updateTabItems(session, "tabs", "PUR")
    updateTabsetPanel(session, "explorer_view", "overview")
  })
  observeEvent(input$open_builder, updateTabsetPanel(session, "explorer_view", "builder"))
  observeEvent(input$back_overview, updateTabsetPanel(session, "explorer_view", "overview"))
  observeEvent(input$back_overview_rev, updateTabItems(session, "tabs", "Overview"))
  
  ## ---- Top navigation ----
  output$topnav <- renderUI({
    cur <- input$tabs
    navlink <- function(id, label, tab) {
      cls <- if (identical(cur, tab)) "active" else ""
      actionLink(id, label, class = cls)
    }
    div(class = "topnav", div(class = "topnav-inner",
                              span(class = "topnav-brand", "PUR explorer",
                                   span(class = "topnav-tag", "EU IMPORTS")),
                              div(class = "topnav-links",
                                  navlink("nav_ov",  "Overview",    "Overview"),
                                  navlink("nav_app", "Explorer",    "PUR"),
                                  actionLink("nav_rev", "Data revisions", class = "topnav-hidden-link"))
    ))
  })
  
  observeEvent(input$nav_ov,  updateTabItems(session, "tabs", "Overview"))
  observeEvent(input$nav_app, updateTabItems(session, "tabs", "PUR"))
  observeEvent(input$nav_rev, updateTabItems(session, "tabs", "Revisions"))
  
  ## ============ EXPLORER: overview + list builder ============
  
  ## Lookups that don't depend on the partner come from the full data
  bloc_names <- c("EU")
  hs2_desc_lookup <- if (!is.null(app_lookups)) app_lookups$hs2_desc
  else raw_data("preprocessed_data") %>% ungroup() %>% distinct(HS2, HS2_desc)
  b_years_all <- if (!is.null(app_lookups)) app_lookups$years
  else sort(unique(raw_data("preprocessed_HS4")$year))
  b_countries_all <- if (!is.null(app_lookups)) app_lookups$countries
  else sort(setdiff(unique(raw_data("preprocessed_HS4")$country_name), bloc_names))
  hs2_levels <- sprintf("%02d", 1:30)
  agrifood_max <- 24   # agrifood = chapters 01-24
  
  b_full_years <- if (!is.null(app_lookups)) app_lookups$full_years
  else raw_data("monthly_HS2_1") %>% ungroup() %>% distinct(year, month) %>%
    count(year) %>% filter(n == 12) %>% pull(year) %>% sort()
  b_years_default <- tail(b_full_years, 3)
  if (length(b_years_default) == 0) b_years_default <- tail(b_years_all, 3)
  
  updateSelectizeInput(session, "b_country", choices = b_countries_all, server = TRUE)
  
  b_levels <- list(
    PUR        = c("All agrifood", "All products", "HS section", "SITC",
                   "HS2", "HS4", "HS6", "CN8"),
    Preference = c("All agrifood", "All products", "HS2", "HS4", "HS6", "CN8"))
  
  observeEvent(list(input$b_output, input$b_mode), {
    req(input$b_output, input$b_mode)
    lvls <- b_levels[[if (isTruthy(input$b_mode)) input$b_mode else "PUR"]]
    keep <- if (isTruthy(input$b_level) && input$b_level %in% lvls) input$b_level
    else "All agrifood"
    updateRadioButtons(session, "b_level", choices = lvls,
                       selected = keep, inline = TRUE)
    ph <- if (input$b_output == "Table")
      paste0("Latest ", length(b_years_default), " full years \u2013 ",
             min(b_years_default), "\u2013", max(b_years_default), " (leave empty)")
    else "All years (leave empty)"
    updateSelectizeInput(session, "b_years", choices = b_years_all,
                         selected = input$b_years, options = list(placeholder = ph))
  })
  
  ## Code choices follow the partner too (only codes traded with that partner)
  code_choices <- function(level, graph_mode) {
    lk <- app_lookups
    nm <- function(x, code, desc) setNames(x[[code]], paste(x[[code]], x[[desc]], sep = " \u2013 "))
    if (graph_mode && level %in% c("HS4", "HS6", "SITC") && !is.null(lk)) {
      return(switch(level,
                    "HS4"  = lk$hs4_combined,
                    "HS6"  = lk$hs6_combined,
                    "SITC" = lk$sitc_combined))
    }
    if (!is.null(lk)) {
      return(switch(level,
                    "HS2" = nm(lk$hs2_desc, "HS2", "HS2_desc"),
                    "HS4" = nm(lk$hs4_desc, "HS4", "HS4_desc"),
                    "HS6" = nm(lk$hs6_desc, "HS6", "HS6_desc"),
                    "CN8" = nm(lk$cn8_desc, "CN8", "CN8_desc"),
                    "SITC" = if (!is.null(d("preprocessed_SITC")))
                      sort(unique(d("preprocessed_SITC")$`SITC combined`)) else NULL,
                    NULL))
    }
    ## fallback if the lookups file hasn't been built yet
    switch(level,
           "HS2" = nm(hs2_desc_lookup, "HS2", "HS2_desc"),
           "HS4" = nm(d("preprocessed_HS4") %>% ungroup() %>% distinct(HS4, HS4_desc) %>%
                        arrange(HS4), "HS4", "HS4_desc"),
           "HS6" = nm(d("preprocessed_HS6") %>% ungroup() %>% distinct(HS6, HS6_desc) %>%
                        arrange(HS6), "HS6", "HS6_desc"),
           "CN8" = nm(d("preprocessed_CN8") %>% ungroup() %>% distinct(CN8, CN8_desc) %>%
                        arrange(CN8), "CN8", "CN8_desc"),
           NULL)
  }
  
  observeEvent(list(input$b_level, input$b_output, input$b_mode), {
    req(input$b_level, input$b_output)
    graph_mode <- input$b_output == "Graph" && !identical(input$b_mode, "Preference")
    ch <- code_choices(input$b_level, graph_mode)
    ph <- if (graph_mode) "Choose one or more codes to plot" else "All codes (leave empty)"
    updateSelectizeInput(session, "b_codes", choices = ch, selected = character(0),
                         server = TRUE, options = list(placeholder = ph, maxOptions = 50))
  })
  
  output$b_mode_note <- renderUI({
    p(class = "card-note", style = "margin:0 0 10px;",
      if (identical(input$b_mode, "Preference"))
        "What the goods actually entered under: MFN, GSP or an agreement, duty free or duty paid."
      else "Preference utilisation rates and trade values.")
  })
  
  output$b_help <- renderUI({
    msg <- switch(input$b_level,
                  "All agrifood" =
                    "One overall figure/series covering every agrifood import
       (chapters 01\u201324). Member-state splits available; graphs can be
       annual or monthly.",
                  "All products" =
                    "One overall figure/series covering every import in the data
       (chapters 01\u201330). Member-state splits available; graphs can be
       annual or monthly.",
                  "HS section" =
                    "The broad HS sections. Member-state splits available;
       graphs can be annual or monthly.",
                  "HS2" =
                    "Chapters 01\u201330. Member-state splits available; graphs
       can be annual or monthly - pick chapter(s) or leave empty for all.",
                  "HS4" =
                    "Detailed product headings. Tables give full member-state detail;
       graphs are monthly, all states combined - pick the code(s) to plot.",
                  "HS6" =
                    "Fine product subheadings. Tables give full member-state detail;
       graphs are monthly, all states combined - pick the code(s) to plot.",
                  "CN8" =
                    "The most detailed product codes. Tables give full member-state
       detail; graphs are monthly - pick the code(s) to plot.",
                  "SITC" =
                    "Standard International Trade Classification groups.
       Tables give member-state detail; graphs are monthly - pick the
       code(s) to plot.")
    p(class = "card-note", style = "margin-bottom:12px;", msg)
  })
  
  ## HS sections by chapter (standard HS: I 01-05, II 06-14, III 15,
  ## IV 16-24, V 25-27, VI 28-38). Labels are taken from the data where
  ## they start "Section <numeral>"; otherwise "Section <numeral>" is shown.
  sec_labels <- sort(unique(raw_data("agri_period_sector")$HS_Section))
  chapter_to_section <- function(hs2) {
    ch <- as.numeric(hs2)
    roman <- dplyr::case_when(ch <= 5 ~ "I", ch <= 14 ~ "II", ch == 15 ~ "III",
                              ch <= 24 ~ "IV", ch <= 27 ~ "V", ch <= 38 ~ "VI",
                              TRUE ~ NA_character_)
    code <- paste("Section", roman)
    lab <- sec_labels[match(code, trimws(sub(":.*$", "", sec_labels)))]
    ifelse(is.na(lab), code, lab)
  }
  
  ## ---- KPI band ----
  output$kpi_strip <- renderUI({
    core <- d("preprocessed_agri_PUR") %>% filter(!country_name %in% bloc_names)
    n_countries <- length(unique(core$country_name))
    
    if ("year" %in% names(core)) {
      core_p <- core %>% filter(year %in% b_years_default)
      pur_lab <- paste0(min(b_years_default), "\u2013", max(b_years_default))
    } else {
      core_p <- core
      pur_lab <- paste0(min(b_years_all), "\u2013", max(b_years_all))
    }
    elig <- sum(core_p$Eligible_Trade, na.rm = TRUE)
    overall <- if (elig > 0)
      paste0(round(sum(core_p$Pref_Trade, na.rm = TRUE) / elig * 100, 1), "%") else "n/a"
    
    ti <- d("Totalexports") %>% filter(!country_name %in% bloc_names)
    if ("year" %in% names(ti)) {
      ti <- ti %>% filter(year %in% b_years_default)
      tot_imp <- round(sum(ti$Total, na.rm = TRUE) / length(b_years_default), 0)
      imp_txt <- paste0("Avg annual imports, ",
                        min(b_years_default), "\u2013", max(b_years_default))
    } else {
      tot_imp <- round(sum(ti$Total, na.rm = TRUE), 0)
      imp_txt <- paste0("Total imports, ", min(b_years_all), "\u2013", max(b_years_all))
    }
    div(class = "kpi-strip",
        div(class = "kpi-cell",
            div(class = "kpi-icon", icon("globe")),
            div(p(class = "kpi-value", n_countries),
                p(class = "kpi-label", "Member states in the data"))),
        div(class = "kpi-cell",
            div(class = "kpi-icon", icon("percent")),
            div(p(class = "kpi-value", overall),
                p(class = "kpi-label", paste0("Overall PUR, ", pur_lab)))),
        div(class = "kpi-cell",
            div(class = "kpi-icon", icon("euro-sign")),
            div(p(class = "kpi-value", paste0("\u20ac", tot_imp, "bn")),
                p(class = "kpi-label", imp_txt)))
    )
  })
  
  hs2_summary <- reactive({
    d("summary_data_all") %>%
      filter(as.numeric(HS2) <= agrifood_max) %>%
      group_by(HS2) %>%
      summarise(Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
                Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE), .groups = "drop") %>%
      filter(Eligible_Trade > 0)
  })
  
  output$ov_hs2_text <- renderUI({
    df <- hs2_summary() %>%
      mutate(PUR = round(Pref_Trade / Eligible_Trade * 100, 2)) %>%
      left_join(hs2_desc_lookup, by = "HS2")
    if (nrow(df) == 0) return(p(class = "card-note", no_data_msg()))
    lowest <- df[which.min(df$PUR), ]
    HTML(paste0(
      "The graph below presents the average PUR for each agrifood chapter
       (HS2, chapters 01\u201324) across all member states and all years in
       the data. <strong>",
      lowest$HS2_desc, " (Chapter ", sprintf("%02d", as.numeric(lowest$HS2)),
      ")</strong> has the lowest PUR of <strong>", lowest$PUR, "%</strong>.
       Note: this graph shows the import preference used (blue) as a share
       of the imports eligible for preference (grey). There are some HS
       chapters which are all MFN zero and therefore no imports would be
       eligible for a preference - these appear as gaps."))
  })
  
  output$ov_hs2 <- renderPlotly({
    validate(need(nrow(hs2_summary()) > 0, no_data_msg()))
    df <- hs2_summary() %>%
      mutate(HS2 = sprintf("%02d", as.numeric(HS2)),
             PUR = round(Pref_Trade / Eligible_Trade * 100, 1),
             NonPUR = 100 - PUR) %>%
      left_join(hs2_desc_lookup %>%
                  mutate(HS2 = sprintf("%02d", as.numeric(HS2))), by = "HS2") %>%
      mutate(desc_short = ifelse(nchar(HS2_desc) > 45,
                                 paste0(sub("[;,].*$", "", HS2_desc)), HS2_desc),
             desc_wrap = gsub("\\n", "<br>", str_wrap(desc_short, 38)),
             tip = paste0("<b>Ch. ", HS2, "</b><br>", desc_wrap, "<br>PUR: ", PUR, "%")) %>%
      tidyr::pivot_longer(c(NonPUR, PUR), names_to = "PUR_Type", values_to = "share") %>%
      mutate(HS2 = factor(HS2, levels = sprintf("%02d", 1:agrifood_max)),
             PUR_Type = factor(PUR_Type, levels = c("NonPUR", "PUR")))
    ggplotly(
      ggplot(df, aes(x = HS2, y = share, fill = PUR_Type, text = tip)) +
        geom_col(position = "stack", width = 0.82) +
        scale_x_discrete(drop = FALSE) +
        scale_fill_manual(values = c("NonPUR" = "#b1b4b6", "PUR" = "#1d70b8"),
                          labels = c("Eligible, preference not used", "Preference used")) +
        theme_classic() +
        theme(axis.title = element_blank(),
              axis.text.x = element_text(angle = 90),
              legend.title = element_blank(),
              legend.position = "bottom"),
      tooltip = "text") %>%
      plotly::layout(legend = list(orientation = "h", y = -0.12), margin = list(b = 30, t = 10))
  })
  
  output$ov_ts <- renderPlotly({
    df <- d("monthly_HS2_1") %>%
      filter(!country_name %in% bloc_names,
             as.numeric(HS2) <= agrifood_max) %>%
      group_by(year) %>%
      summarise(P = sum(Pref_Trade, na.rm = TRUE),
                E = sum(Eligible_Trade, na.rm = TRUE), .groups = "drop") %>%
      filter(E > 0) %>%
      mutate(year = as.numeric(as.character(year)), PUR = round(P / E * 100, 1))
    validate(need(nrow(df) > 0, no_data_msg()))
    last_pt <- df[which.max(df$year), ]
    ggplotly(
      ggplot(df, aes(x = year, y = PUR, group = 1, text = paste0(year, ": ", PUR, "%"))) +
        geom_line(colour = "#1d70b8", linewidth = 0.9) +
        geom_point(colour = "#1d70b8", size = 2) +
        geom_point(data = last_pt, colour = "#003078", size = 2.8) +
        scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
        scale_x_continuous(breaks = sort(unique(df$year))) +
        theme_minimal() +
        theme(axis.title = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank()),
      tooltip = "text") %>%
      plotly::layout(margin = list(b = 30, t = 10, l = 10, r = 10))
  })
  
  ## ---- Map (folded into Explorer overview) ----
  ## Joined on demand for the chosen partner and year
  Map_data <- reactive({
    req(input$maptype, input$yearmap)
    src <- switch(input$maptype,
                  "PUR" = createmap_data(d("Map1")),
                  "MFN" = createmap_data2(d("Map2")),
                  "FTA" = createmap_data3(d("Map3")))
    yr <- as.character(input$yearmap)
    left_join(map_sf_data(), src[as.character(src$year) == yr, , drop = FALSE],
              by = c("ISO_A2_EH" = "cooalpha"))
  })
  
  output$map <- renderLeaflet({
    if (input$maptype == "PUR") {
      createmap_plot(Map_data(), pname())
    } else if (input$maptype == "MFN") {
      createmap_plot2(Map_data(), pname())
    } else {
      createmap_plot3(Map_data(), pname())
    }
  })
  
  output$map_text <- renderUI({
    base_text <- switch(input$maptype,
                        "PUR" = paste0("<strong>Currently showing: PUR.</strong> Each member state is
        shaded by its Preference Utilisation Rate for the selected year -
        the share of its preference-eligible imports from ", pname(), " that actually
        entered under a preferential tariff. Darker blue = higher
        utilisation. Grey states have no eligible trade in that year."),
                        "MFN" = paste0("<strong>Currently showing: MFN Non-zero use.</strong>
        Each member state is shaded by the value (\u20acm) of its imports from ",
                                       pname(), " that entered under the standard MFN regime while paying a
        non-zero tariff in the selected year - trade that paid full duty.
        Darker blue = higher value."),
                        "FTA" = paste0("<strong>Currently showing: FTA Non-zero use.</strong>
        Each member state is shaded by the value (\u20acm) of its imports from ",
                                       pname(), " that entered under a preferential regime but still at
        a non-zero reduced tariff in the selected year. Darker blue =
        higher value."))
    views_text <- if (length(maptype_choices) > 1)
      "Use the dropdown to switch views and the slider to change year.
      Hover over a country for its figure."
    else "Use the slider to change year. Hover over a country for its figure."
    HTML(paste0(base_text, "<br><br>", views_text,
                '<div style="margin-bottom: 18px;"></div>'))
  })
  
  ## ---- List builder ----
  builder_source <- function(level) {
    if (level %in% c("All agrifood", "All products")) {
      src <- d("preprocessed_HS4")
      if (level == "All agrifood") src <- src %>% filter(as.numeric(HS2) <= agrifood_max)
      src %>%
        group_by(country_name, year) %>%
        summarise(Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
                  Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
                  Total_imp = sum(Total_ex, na.rm = TRUE), .groups = "drop") %>%
        mutate(HS2 = NA_character_,
               Code = if (level == "All agrifood") "AGRI" else "ALL",
               Description = if (level == "All agrifood") "All agrifood products (chapters 01\u201324)"
               else "All products (chapters 01\u201330)")
    } else if (level == "HS section") {
      d("preprocessed_HS4") %>%
        mutate(sec = chapter_to_section(HS2)) %>%
        group_by(country_name, year, sec) %>%
        summarise(Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
                  Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
                  Total_imp = sum(Total_ex, na.rm = TRUE), .groups = "drop") %>%
        mutate(HS2 = NA_character_, Code = sub(":.*$", "", sec), Description = sec) %>%
        select(-sec)
    } else if (level == "HS4") {
      d("preprocessed_HS4") %>% mutate(Code = HS4, Description = HS4_desc, Total_imp = Total_ex)
    } else if (level == "HS6") {
      d("preprocessed_HS6") %>% mutate(Code = HS6, Description = HS6_desc, Total_imp = Total_ex)
    } else if (level == "HS2") {
      d("preprocessed_HS4") %>%
        ungroup() %>%
        filter(!country_name %in% bloc_names) %>%
        group_by(country_name, year, HS2) %>%
        summarise(Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
                  Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
                  Total_imp = sum(Total_ex, na.rm = TRUE), .groups = "drop") %>%
        left_join(hs2_desc_lookup, by = "HS2") %>%
        mutate(Code = HS2, Description = coalesce(HS2_desc, ""))
    } else if (level == "SITC") {
      validate(need(!is.null(d("preprocessed_SITC")),
                    "SITC tables need data/preprocessed_SITC.RDS - rerun the pipeline"))
      d("preprocessed_SITC") %>%
        mutate(Code = `SITC combined`, Description = `SITC combined`,
               HS2 = NA_character_, Total_imp = Total_ex)
    } else {
      d("preprocessed_CN8") %>% mutate(Code = CN8, Description = CN8_desc, Total_imp = Total_ex)
    }
  }
  
  b_built <- eventReactive(input$b_go, {
    df <- builder_source(input$b_level) %>% ungroup() %>%
      select(-any_of("partner")) %>%
      filter(!country_name %in% bloc_names)
    yrs <- if (length(input$b_years)) input$b_years else b_years_default
    df <- df %>% filter(year %in% yrs)
    if (input$b_level %in% c("HS2", "HS4", "HS6", "CN8", "SITC") && length(input$b_codes)) {
      df <- df %>% filter(Code %in% input$b_codes)
    }
    
    grp <- c("country_name", "Code", "Description", if (isTRUE(input$b_byyear)) "year")
    missing <- character(0)
    
    if (length(input$b_country)) {
      out <- df %>%
        filter(country_name %in% input$b_country) %>%
        group_by(across(all_of(grp))) %>%
        summarise(Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
                  Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
                  Total_imp = sum(Total_imp, na.rm = TRUE), .groups = "drop")
      missing <- setdiff(input$b_country, unique(out$country_name))
    } else {
      comb <- df %>%
        group_by(across(all_of(setdiff(grp, "country_name")))) %>%
        summarise(Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
                  Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
                  Total_imp = sum(Total_imp, na.rm = TRUE), .groups = "drop") %>%
        mutate(country_name = "All member states \u2013 total") %>%
        select(country_name, everything())
      if (isTRUE(input$b_combined)) {
        rows <- df %>%
          group_by(across(all_of(grp))) %>%
          summarise(Pref_Trade = sum(Pref_Trade, na.rm = TRUE),
                    Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
                    Total_imp = sum(Total_imp, na.rm = TRUE), .groups = "drop")
        out <- bind_rows(rows, comb[, names(rows)])
      } else {
        out <- comb
      }
    }
    if (!isTRUE(input$b_byyear)) {
      n_yrs <- max(1, length(unique(df$year)))
      out <- out %>%
        mutate(across(c(Pref_Trade, Eligible_Trade, Total_imp), ~ round(.x / n_yrs)))
    }
    out %>%
      mutate(`PUR (%)` = ifelse(Eligible_Trade > 0,
                                round(Pref_Trade / Eligible_Trade * 100, 1), NA)) %>%
      mutate(across(c(Pref_Trade, Eligible_Trade, Total_imp), ~ round(.x / 1e6, 1))) %>%
      arrange(Code, across(any_of("year")),
              country_name != "All member states \u2013 total", country_name) %>%
      rename(`Member state` = country_name,
             `Preferential imports (€m)` = Pref_Trade,
             `Eligible imports (€m)` = Eligible_Trade,
             `Total imports (€m)` = Total_imp) %>%
      select(`Member state`, any_of("year"), Code, Description,
             `Preferential imports (€m)`, `Eligible imports (€m)`,
             `Total imports (€m)`, `PUR (%)`) -> out2
    
    if (!isTRUE(input$b_byyear)) {
      names(out2) <- sub("^Preferential imports", "Avg preferential imports",
                         sub("^Eligible imports", "Avg eligible imports",
                             sub("^Total imports", "Avg total imports", names(out2))))
    }
    attr(out2, "missing") <- missing
    out2
  })
  
  b_graph_data <- eventReactive(input$b_go, {
    yrs <- if (length(input$b_years)) input$b_years else b_years_all
    
    if (input$b_level %in% c("All agrifood", "All products", "HS section", "HS2")) {
      df <- d("monthly_HS2_1") %>% ungroup() %>%
        filter(!country_name %in% bloc_names, year %in% yrs)
      if (input$b_level == "All agrifood") df <- df %>% filter(as.numeric(HS2) <= agrifood_max)
      if (input$b_level == "HS section") df <- df %>%
          mutate(code = sub(":.*$", "", chapter_to_section(HS2)))
      if (input$b_level == "HS2") {
        df <- df %>% mutate(code = HS2)
        if (length(input$b_codes)) df <- df %>% filter(HS2 %in% input$b_codes)
      }
      use_code <- !input$b_level %in% c("All agrifood", "All products")
      use_ctry <- length(input$b_country) > 0
      if (use_ctry) df <- df %>% filter(country_name %in% input$b_country)
      
      grp <- c(if (use_ctry) "country_name", if (use_code) "code", "year",
               if (input$b_freq2 == "Monthly") "month")
      out <- df %>%
        group_by(across(all_of(grp))) %>%
        summarise(P = sum(Pref_Trade, na.rm = TRUE),
                  E = sum(Eligible_Trade, na.rm = TRUE), .groups = "drop") %>%
        filter(E > 0) %>%
        mutate(PUR = round(P / E * 100, 1))
      if (input$b_freq2 == "Monthly") {
        ## guard against non-month periods (e.g. Comext's annual "52" rows)
        out <- out %>% filter(month %in% sprintf("%02d", 1:12))
        validate(need(nrow(out) > 0, "No monthly data for this selection."))
        out <- out %>% mutate(x = ym_date(year, month), lbl = format(x, "%b %Y"))
      } else {
        out <- out %>% mutate(x = as.numeric(as.character(year)), lbl = as.character(year))
      }
      out$series <-
        if (use_ctry && use_code) paste0(out$country_name, " \u00B7 ", out$code)
      else if (use_ctry) out$country_name
      else if (use_code) out$code
      else if (input$b_level == "All agrifood") "All member states \u2013 total, agrifood"
      else "All member states \u2013 total, all products"
      b_ctry_missing(if (use_ctry) setdiff(input$b_country, unique(out$country_name)) else character(0))
      
    } else {
      validate(need(length(input$b_codes) > 0,
                    "Pick at least one code to plot - the full list is too large to draw."))
      ctry_src <- switch(input$b_level,
                         "HS4"  = d("monthly_HS4_country"),
                         "HS6"  = d("monthly_HS6_country"),
                         "CN8"  = d("monthly_CN8_country"),
                         "SITC" = d("monthly_SITC_country"), NULL)
      
      if (is.null(ctry_src) && pref_ready && input$b_level %in% c("HS4", "HS6", "CN8")) {
        ## no monthly-by-country file in the pipeline, so use the preference
        ## detail file: it has CN8 x member state x month, which gives the
        ## same PUR and lets a member-state selection work at these levels
        px <- as.data.table(pref_for(partner_sel(), detail = TRUE))
        validate(need(!is.null(px) && nrow(px) > 0,
                      paste0("No data for ", pname(), " at this level.")))
        nchar_lvl <- c("HS4" = 4, "HS6" = 6, "CN8" = 8)[[input$b_level]]
        picked <- substr(input$b_codes, 1, nchar_lvl)
        px <- px[substr(CN8, 1, nchar_lvl) %in% picked & year %in% yrs]
        if (length(input$b_country)) px <- px[country_name %in% input$b_country]
        px <- px[month %in% sprintf("%02d", 1:12)]
        px[, ctry := if (length(input$b_country)) country_name else "all"]
        px[, code := substr(CN8, 1, nchar_lvl)]
        out <- px[, .(P = sum(statvalue[pref_used & eligible], na.rm = TRUE),
                      E = sum(statvalue[eligible], na.rm = TRUE)),
                  by = .(code, year, month, country_name = ctry)]
        out <- out[E > 0]
        b_ctry_missing(if (length(input$b_country))
          setdiff(input$b_country, unique(out$country_name)) else character(0))
        ## stop here when nothing is left, before any date is built
        validate(need(nrow(out) > 0,
                      paste0("No eligible trade for this code",
                             if (length(input$b_country))
                               paste0(" from ", paste(input$b_country, collapse = ", ")),
                             " \u2013 it may not be traded on this route, or may have",
                             " no preference eligibility (e.g. zero MFN tariff).")))
        out[, `:=`(PUR = round(100 * P / E, 1), x = ym_date(year, month))]
        out[, lbl := format(x, "%b %Y")]
        out[, series := if (length(input$b_country) > 1)
          paste0(country_name, " \u00B7 ", code) else code]
        out <- as.data.frame(out)
        
      } else if (!is.null(ctry_src) && length(input$b_country) > 0) {
        code_col <- input$b_level
        picked <- sub("\\D.*$", "", input$b_codes)
        out <- ctry_src %>%
          filter(country_name %in% input$b_country, .data[[code_col]] %in% picked, year %in% yrs) %>%
          group_by(country_name, code = .data[[code_col]], year, sort_date) %>%
          summarise(P = sum(Pref_Trade, na.rm = TRUE), E = sum(Eligible_Trade, na.rm = TRUE),
                    .groups = "drop") %>%
          filter(E > 0) %>%
          mutate(PUR = round(P / E * 100, 1), x = sort_date, lbl = format(x, "%b %Y"),
                 series = if (length(input$b_country) > 1)
                   paste0(country_name, " \u00B7 ", code) else code)
        b_ctry_missing(setdiff(input$b_country, unique(out$country_name)))
        validate(need(nrow(out) > 0,
                      paste0("No eligible trade for this code from ",
                             paste(input$b_country, collapse = ", "),
                             " \u2013 it may not be traded on this route, or may have",
                             " no preference eligibility (e.g. zero MFN tariff).")))
      } else {
        b_ctry_missing(character(0))
        src <- switch(input$b_level,
                      "HS4"  = d("HS4_timeseries") %>% dplyr::rename(code = `HS4 combined`),
                      "HS6"  = d("HS6_timeseries") %>% dplyr::rename(code = `HS6 combined`),
                      "SITC" = d("SITC_timeseries") %>% dplyr::rename(code = `SITC combined`),
                      "CN8"  = { validate(need(!is.null(d("CN8_timeseries")),
                                               paste0("CN8 graphs need the preference detail file ",
                                                      "data/pref/", partner_sel(), "_cn8.RDS ",
                                                      "(run 04_pref_data.R), or data/CN8_timeseries.RDS. ",
                                                      "Neither was found.")))
                        d("CN8_timeseries") %>% dplyr::rename(code = `CN8 combined`) })
        ## the pipeline stores the month as `period`; older builds called it `perref`
        if (!"perref" %in% names(src)) src <- src %>% dplyr::rename(perref = period)
        out <- src %>%
          filter(code %in% input$b_codes, !is.na(agri_PUR)) %>%
          mutate(x = as.Date(as.character(perref)), year = format(x, "%Y"),
                 PUR = agri_PUR, lbl = format(x, "%b %Y"), series = as.character(code)) %>%
          filter(year %in% yrs)
      }
    }
    validate(need(nrow(out) > 0, "No eligible trade for this selection - try different options"))
    out
  })
  
  b_meta <- eventReactive(input$b_go, {
    ctry <- if (length(input$b_country)) paste(input$b_country, collapse = ", ")
    else "All member states"
    yrs <- if (length(input$b_years)) paste(sort(input$b_years), collapse = ", ")
    else paste0(min(b_years_all), "\u2013", max(b_years_all))
    freq <- if (input$b_level %in% c("All agrifood", "All products", "HS section", "HS2"))
      tolower(input$b_freq2) else "monthly"
    list(
      title = if (pref_mode())
        paste0("Entry regimes \u2013 EU imports from ", pname())
      else paste0("Preference Utilisation Rate \u2013 EU imports from ", pname()),
      subtitle = paste0(input$b_level, " level \u00B7 ", ctry, " \u00B7 ", yrs, " \u00B7 ", freq),
      caption = paste0(
        if (input$b_level == "HS section")
          paste0(paste(strwrap(paste(sec_labels, collapse = " \u00B7 "), width = 165), collapse = "\n"), "\n")
        else "",
        "PUR = preferential imports as % of preference-eligible imports. ",
        "Source: Eurostat \u00B7 Defra PUR explorer \u00B7 Downloaded ",
        format(Sys.Date(), "%d %B %Y")))
  })
  
  ## ---------------- preference regimes (same controls) ----------------
  
  ## year + month -> date, safe when there are no rows to convert
  ym_date <- function(year, month) {
    if (length(year) == 0) return(as.Date(character(0)))
    as.Date(paste(year, month, "01", sep = "-"))
  }
  
  ## NULL-safe: the mode radio may not have reported in yet
  pref_mode <- function() identical(isolate(input$b_mode), "Preference")
  
  ## code descriptions for the preference table, from the lookups file
  pref_descriptions <- function(code, level) {
    lk <- app_lookups
    tab <- if (is.null(lk)) NULL else switch(level,
                                             "HS2" = lk$hs2_desc, "HS4" = lk$hs4_desc,
                                             "HS6" = lk$hs6_desc, "CN8" = lk$cn8_desc, NULL)
    if (is.null(tab)) {
      return(if (level == "All agrifood") "All agrifood products (chapters 01\u201324)"
             else if (level == "All products") "All products (chapters 01\u201330)" else "")
    }
    as.character(tab[[2]])[match(code, tab[[1]])]
  }
  
  pref_rows <- eventReactive(input$b_go, {
    validate(need(pref_ready,
                  "Preference data hasn't been built yet - run 04_pref_data.R in the pipeline."))
    lvl  <- input$b_level
    deep <- lvl %in% c("HS4", "HS6", "CN8")     # only these need the CN8 file
    x <- pref_for(partner_sel(), detail = deep)
    validate(need(!is.null(x) && nrow(x) > 0, paste0("No preference data for ", pname(), ".")))
    x <- as.data.table(x)
    
    if (length(input$b_country)) x <- x[country_name %in% input$b_country]
    yrs <- if (length(input$b_years)) input$b_years
    else if (input$b_output == "Table") b_years_default else b_years_all
    x <- x[year %in% yrs]
    
    if (lvl == "All agrifood")
      x <- x[suppressWarnings(as.numeric(if (deep) substr(CN8, 1, 2) else HS2)) <= agrifood_max]
    n <- c("HS2" = 2, "HS4" = 4, "HS6" = 6, "CN8" = 8)[lvl]
    if (!is.na(n) && length(input$b_codes))
      x <- x[substr(if (deep) CN8 else HS2, 1, n) %in% substr(input$b_codes, 1, n)]
    validate(need(nrow(x) > 0, "No trade for this selection - try different options."))
    
    ## the code shown in the table, at the chosen level
    x[, Code := if (is.na(n)) (if (lvl == "All agrifood") "AGRI" else "ALL")
      else substr(if (deep) CN8 else HS2, 1, n)]
    x
  })
  
  pref_totals <- reactive({
    x <- pref_rows()
    tot <- sum(x$statvalue, na.rm = TRUE)
    pc <- function(cond) if (tot > 0) 100 * sum(x$statvalue[cond], na.rm = TRUE) / tot else NA
    elig <- sum(x$statvalue[x$eligible], na.rm = TRUE)
    list(duty_free    = pc(x$duty_free),
         mfn_zero     = pc(x$regime == "MFN \u2013 zero duty"),
         mfn_duty     = pc(x$regime == "MFN \u2013 duty paid"),
         pref_zero    = pc(x$duty_free & x$pref_used),
         pref_nonzero = pc(!x$duty_free & x$pref_used),
         not_used     = pc(x$eligible & !x$pref_used),
         unknown      = pc(x$regime == "Entry regime unknown"),
         pur          = if (elig > 0)
           100 * sum(x$statvalue[x$pref_used & x$eligible], na.rm = TRUE) / elig else NA)
  })
  
  fmt1 <- function(x) if (is.na(x)) "n/a" else paste0(formatC(x, format = "f", digits = 1), "%")
  
  output$b_page_title <- renderText({
    if (identical(input$b_mode, "Preference"))
      "Explorer \u00B7 Preference regimes" else "Explorer \u00B7 Custom PUR table"
  })
  
  output$p_sentence <- renderUI({
    req(input$b_go > 0, pref_mode(), b_fresh())
    t <- pref_totals()
    yrs <- isolate(if (length(input$b_years)) input$b_years
                   else if (input$b_output == "Table") b_years_default else b_years_all)
    per <- if (length(yrs) == 1) paste("in", yrs)
    else paste0("in ", min(yrs), "\u2013", max(yrs))
    extra <- c(
      if (!is.na(t$not_used) && t$not_used >= 0.05)
        paste0(fmt1(t$not_used), " had a preference available but came in under MFN terms anyway."),
      if (!is.na(t$unknown) && t$unknown >= 0.05)
        paste0(fmt1(t$unknown), " had an entry regime that wasn't recorded."))
    tagList(
      h3(style = "font-size:17px; font-weight:700; color:#0b0c0c; margin:2px 0 8px;",
         paste0(fmt1(t$duty_free), " of these EU imports from ", pname(),
                " entered duty free ", per)),
      p(class = "card-note", style = "max-width:none; font-size:13px;",
        paste0(fmt1(t$mfn_zero), " entered at a zero MFN tariff, ", fmt1(t$pref_zero),
               " used a preference at zero duty, ", fmt1(t$pref_nonzero),
               " used a preference but still paid duty, and ", fmt1(t$mfn_duty),
               " paid full MFN duty. ", paste(extra, collapse = " "),
               " PUR for this selection: ", fmt1(t$pur), ".")))
  })
  
  output$p_strip <- renderUI({
    req(input$b_go > 0, pref_mode(), b_fresh())
    seg <- pref_rows()[, .(v = sum(statvalue, na.rm = TRUE)), by = regime]
    seg[, share := 100 * v / sum(v)]
    seg <- seg[order(match(regime, regime_levels))]
    bars <- lapply(seq_len(nrow(seg)), function(i)
      div(style = paste0("width:", seg$share[i], "%; background:",
                         regime_cols[[seg$regime[i]]], "; height:20px;"),
          title = paste0(seg$regime[i], ": ", fmt1(seg$share[i]))))
    key <- lapply(seq_len(nrow(seg)), function(i)
      span(style = "margin-right:14px; font-size:11.5px; color:#505a5f; white-space:nowrap;",
           span(style = paste0("display:inline-block; width:10px; height:10px; margin-right:5px;",
                               "background:", regime_cols[[seg$regime[i]]], ";")),
           paste0(seg$regime[i], " ", fmt1(seg$share[i]))))
    tagList(div(style = "display:flex; width:100%; border-radius:4px; overflow:hidden; margin:4px 0 6px;", bars),
            div(style = "display:flex; flex-wrap:wrap; margin-bottom:10px;", key))
  })
  
  ## table: total imports, then MFN, then preference broken into PTA and GSP.
  ## "Expand duty free / duty paid" splits each of MFN, PTA and GSP into the
  ## part that entered at zero duty and the part that paid some duty.
  pref_built <- reactive({
    x <- copy(pref_rows())
    by_year  <- isTRUE(isolate(input$b_byyear))
    split_ms <- length(isolate(input$b_country)) > 0 || isTRUE(isolate(input$b_combined))
    expand   <- isTRUE(isolate(input$p_expand))
    x[, `Member state` := if (split_ms) country_name else "All member states \u2013 total"]
    x[, Description := pref_descriptions(Code, isolate(input$b_level))]
    grp <- c("Member state", "Code", "Description", if (by_year) "year")
    
    r_mfn_free <- "MFN \u2013 zero duty"
    r_mfn_paid <- "MFN \u2013 duty paid"
    r_pta      <- "PTA preference used"
    r_gsp      <- "GSP preference used"
    
    out <- x[, .(total     = sum(statvalue, na.rm = TRUE),
                 pref      = sum(statvalue[pref_used], na.rm = TRUE),
                 mfn_free  = sum(statvalue[regime == r_mfn_free], na.rm = TRUE),
                 mfn_paid  = sum(statvalue[regime == r_mfn_paid], na.rm = TRUE),
                 pta_free  = sum(statvalue[regime == r_pta &  duty_free], na.rm = TRUE),
                 pta_paid  = sum(statvalue[regime == r_pta & !duty_free], na.rm = TRUE),
                 gsp_free  = sum(statvalue[regime == r_gsp &  duty_free], na.rm = TRUE),
                 gsp_paid  = sum(statvalue[regime == r_gsp & !duty_free], na.rm = TRUE)),
             by = grp]
    ## shares of total imports, from the unrounded values
    out[, `% under preference` := round(100 * pref / total, 1)]
    out[, `% MFN duty free`    := round(100 * mfn_free / total, 1)]
    out[, `% duty paid`        := round(100 * (mfn_paid + pta_paid + gsp_paid) / total, 1)]
    
    ## money columns in EUR millions; average per year when years are combined.
    ## The smallest parts are rounded first and the subtotals built from them,
    ## so every "of which" adds up to its parent on screen. (Total imports is
    ## rounded on its own and is legitimately larger, since it also covers
    ## trade with an unrecorded regime or no eligibility information.)
    n_yrs <- if (by_year) 1 else max(1, length(unique(x$year)))
    leaves <- c("total", "mfn_free", "mfn_paid", "pta_free", "pta_paid",
                "gsp_free", "gsp_paid")
    out[, (leaves) := lapply(.SD, function(v) round(v / 1e6 / n_yrs, 1)), .SDcols = leaves]
    out[, `:=`(mfn = mfn_free + mfn_paid,
               pta = pta_free + pta_paid,
               gsp = gsp_free + gsp_paid)]
    out[, pref := pta + gsp]
    
    ## which money columns to show, in order
    keep <- if (expand)
      c("total",
        "mfn", "mfn_free", "mfn_paid",
        "pref",
        "pta", "pta_free", "pta_paid",
        "gsp", "gsp_free", "gsp_paid")
    else c("total", "mfn", "pref", "pta", "gsp")
    
    labs <- c(total    = "Total imports",
              mfn      = "MFN",
              mfn_free = "MFN duty free",
              mfn_paid = "MFN duty paid",
              pref     = "Preference used",
              pta      = "of which PTA",
              pta_free = "PTA duty free",
              pta_paid = "PTA duty paid",
              gsp      = "of which GSP",
              gsp_free = "GSP duty free",
              gsp_paid = "GSP duty paid")
    pfx <- if (by_year) "" else "Avg "
    
    out <- out[, c(grp, keep, "% under preference", "% MFN duty free",
                   "% duty paid"), with = FALSE]
    setnames(out, keep, paste0(pfx, labs[keep], " (\u20acm)"))
    if (by_year) setnames(out, "year", "Year")
    out[order(Code, `Member state`)]
  })
  
  ## graph: each regime's share of these imports over time
  pref_graph_data <- reactive({
    x <- pref_rows()
    monthly <- isolate(input$b_freq2) == "Monthly"
    grp <- c("year", if (monthly) "month", "regime")
    out <- x[, .(value = sum(statvalue, na.rm = TRUE)), by = grp]
    out[, share := round(100 * value / sum(value), 1),
        by = c("year", if (monthly) "month")]
    if (monthly) {
      out <- out[month %in% sprintf("%02d", 1:12)]
      validate(need(nrow(out) > 0, "No monthly data for this selection."))
      out[, x := ym_date(year, month)]
      out[, lbl := format(x, "%b %Y")]
    } else {
      out[, x := as.numeric(year)]
      out[, lbl := year]
    }
    out[, regime := factor(regime, levels = intersect(regime_levels, unique(regime)))]
    out
  })
  
  b_gg <- reactiveVal(NULL)
  b_ctry_missing <- reactiveVal(character(0))
  b_built_type <- reactiveVal(NULL)
  b_built_partner <- reactiveVal(NULL)
  b_built_mode <- reactiveVal(NULL)
  observeEvent(input$b_go, {
    b_built_type(input$b_output)
    b_built_partner(input$partner)
    b_built_mode(input$b_mode)
  })
  ## A built table/graph is "stale" if the output type OR the partner changed
  b_fresh <- reactive(identical(input$b_output, b_built_type()) &&
                        identical(input$partner, b_built_partner()) &&
                        identical(input$b_mode, b_built_mode()))
  
  output$b_graph <- renderPlotly({
    req(input$b_go > 0, input$b_output == "Graph", b_fresh())
    dg <- b_graph_data()
    one_series <- length(unique(dg$series)) == 1
    g <- ggplot(dg, aes(x = x, y = PUR, colour = series, group = series,
                        text = paste0(series, " (", lbl, "): ", PUR, "%"))) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.3) +
      scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      theme_classic() +
      theme(axis.title = element_blank(), legend.title = element_blank())
    if (one_series) {
      g <- g + scale_colour_manual(values = "#1d70b8") + theme(legend.position = "none")
    }
    if (inherits(dg$x, "Date")) {
      g <- g + scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
        theme(axis.text.x = element_text(angle = 45))
    } else {
      g <- g + scale_x_continuous(breaks = sort(unique(dg$x)))
    }
    b_gg(g)
    p <- ggplotly(g, tooltip = "text") %>%
      plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = list("toImage"))
    if (!one_series) {
      p <- p %>% plotly::layout(
        legend = list(orientation = "h", x = 0, y = -0.22, xanchor = "left", yanchor = "top"),
        margin = list(b = 90))
    }
    p
  })
  
  ## ---- preference tab outputs ----
  output$p_graph <- renderPlotly({
    req(input$b_go > 0, input$b_output == "Graph", pref_mode(), b_fresh())
    pref_plot()
  })
  
  output$p_table <- DT::renderDataTable({
    req(input$b_go > 0, pref_mode(), b_fresh())
    dat <- pref_built()
    pct <- intersect(c("% under preference", "% MFN duty free", "% duty paid"), names(dat))
    money  <- grep("\u20acm", names(dat), value = TRUE)
    labels <- setdiff(names(dat), c(money, pct))
    
    ## which money columns belong to which block (order set in pref_built)
    has <- function(pat) grep(pat, money, value = TRUE)
    col_total <- has("Total imports")
    col_mfn   <- setdiff(has("MFN"), col_total)
    col_pref  <- setdiff(money, c(col_total, col_mfn))
    
    ## short names for the bottom header row; the block band above supplies
    ## the context, and a dash marks a column as part of the one above it
    short <- function(n) {
      n <- sub(" \\(\u20acm\\)$", "", sub("^Avg ", "", n))
      n <- sub("^of which ", "", n)
      n <- sub("^(MFN|PTA|GSP) duty", "\u2013 duty", n)
      n <- sub("^% ", "", n)
      if (n %in% c("MFN", "Preference used")) "Total" else n
    }
    
    ## three-row header: block bands, then the columns inside each block
    bandc <- function(x) switch(x, mfn = "grp-mfn", pref = "grp-pref", "grp-pct")
    th_list <- function(cols, cls) lapply(cols, function(n)
      htmltools::tags$th(class = cls, short(n)))
    
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(lapply(labels, function(n) th(rowspan = 3, n)),
           th(colspan = length(money), class = "grp-top",
              if (grepl("^Avg", money[1])) "Average annual imports (\u20acm)"
              else "Imports (\u20acm)"),
           th(colspan = length(pct), rowspan = 2, class = "grp-pct",
              HTML("Share of total imports&nbsp;*"))),
        tr(th(rowspan = 2, class = "grp-total", "Total imports"),
           th(colspan = length(col_mfn), class = "grp-mfn", "MFN"),
           th(colspan = length(col_pref), class = "grp-pref", "Preference used")),
        tr(th_list(col_mfn, "grp-mfn"), th_list(col_pref, "grp-pref"),
           th_list(pct, "grp-pct"))
      )))
    
    DT::datatable(dat, rownames = FALSE, container = sketch, filter = "top",
                  options = list(pageLength = 15, scrollX = TRUE,
                                 columnDefs = money_defs(dat, money))) %>%
      DT::formatRound(pct, digits = 1) %>%
      DT::formatStyle(col_mfn,  backgroundColor = "#eef3f9") %>%
      DT::formatStyle(col_pref, backgroundColor = "#edf7f0") %>%
      DT::formatStyle(pct,      backgroundColor = "#ffffff")
  })
  
  output$p_caption <- renderText({
    if (input$b_go == 0) return("Choose options on the left, then press Create")
    if (!b_fresh()) return("Options changed \u2013 press Create to build this view")
    sc <- isolate(paste0(pname(), " \u00B7 ", input$b_output, " \u00B7 ",
                         input$b_level, " level"))
    sc
  })
  
  output$p_explain <- renderUI({
    req(input$b_go > 0, pref_mode(), b_fresh())
    p(class = "card-note", style = "max-width:none;",
      paste0("Columns are the value of imports in \u20ac millions entering under each regime.
             '% MFN duty free' is the share that entered at a zero MFN tariff, needing no
             preference. '% under preference' is the share that used a GSP or agreement
             rate, at zero or reduced duty. '% duty paid' is the share that paid some duty,
             under MFN or a non-zero preferential rate.
             'MFN' is trade entering on standard terms; 'Preference used' is trade using a
             GSP or agreement rate, split into PTA and GSP. Tick 'Expand duty free / duty
             paid' on the left to split each of those into the part entering at zero duty
             and the part that paid some duty. The PUR itself is on the Preference
             utilisation tab. * Those three are shares of total imports and may not sum to
             100%, because trade with an unrecorded entry regime or no eligibility
             information is in the total but in none of them."))
  })
  
  ## preference graph: regime shares over time
  pref_plot <- function() {
    dfp <- copy(pref_graph_data())
    dfp[, value_m := round(value / 1e6, 2)]
    g <- ggplot(dfp, aes(x = x, y = value_m, colour = regime, group = regime,
                         text = paste0(regime, " (", lbl, "): \u20ac",
                                       value_m, "m  (", share, "% of imports)"))) +
      geom_line(linewidth = 0.9) + geom_point(size = 1.3) +
      scale_y_continuous(labels = scales::label_comma(accuracy = 1)) +
      scale_colour_manual(values = regime_cols) +
      labs(y = "\u20ac million") +
      theme_classic() +
      theme(axis.title.x = element_blank(), legend.title = element_blank())
    if (inherits(dfp$x, "Date"))
      g <- g + scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      theme(axis.text.x = element_text(angle = 45))
    else g <- g + scale_x_continuous(breaks = sort(unique(dfp$x)))
    b_gg(g)
    ggplotly(g, tooltip = "text") %>%
      plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = list("toImage")) %>%
      plotly::layout(legend = list(orientation = "h", x = 0, y = -0.22,
                                   xanchor = "left", yanchor = "top"),
                     margin = list(b = 90))
  }
  
  output$b_explain <- renderUI({
    req(input$b_go > 0, b_fresh())
    p(class = "card-note", style = "max-width:none;",
      paste0("'Total imports' reflects the EU's imports of these codes from ", pname(), ",
                 regardless of the eligibility or use of a preference regime, e.g. all imports.
                 'Eligible imports' reflects the value of imports that were eligible for a tariff
                 preference. 'Preferential imports' reflects the value of imports that were
                 actually imported under a tariff preference (e.g. used the preference).
                 'PUR (%)' reflects the Preference Utilisation Rate, e.g. value of imports using
                 preference divided by the value of imports that was eligible for a preference.
                 When years are combined in a table, values shown are the average per year across
                 the included years. Trade values are shown in \u20ac millions; a value
                 under \u20ac0.05m shows as '<0.1'."))
  })
  
  output$b_ctry_note <- renderUI({
    req(input$b_go > 0, b_fresh())
    if (isolate(input$b_output) != "Graph") return(NULL)
    m <- b_ctry_missing()
    if (!length(m)) return(NULL)
    p(class = "card-note", style = "color:#d4351c; margin-top:6px;",
      paste0("Not plotted \u2013 no eligible trade for this selection: ", paste(m, collapse = ", "), "."))
  })
  
  output$b_seckey <- renderUI({
    req(input$b_go > 0, b_fresh())
    if (isolate(input$b_level) != "HS section" || isolate(input$b_output) != "Graph") return(NULL)
    p(class = "card-note", style = "margin-top:6px;", HTML(paste(sec_labels, collapse = " &nbsp;\u00B7&nbsp; ")))
  })
  
  ## ---- optional import-regime breakdown (only if pref_breakdown_code.RDS exists) ----
  b_regime_group <- function(u) {
    dplyr::case_when(
      grepl("Unknown", u, ignore.case = TRUE) ~ "Entry regime unknown",
      grepl("TCA", u)                          ~ "TCA preference used",
      grepl("GSP", u)                          ~ "GSP/DCTS preference used",
      grepl("MFN", u) & grepl("non-zero", u)   ~ "MFN \u2013 duty payable",
      grepl("MFN", u)                          ~ "MFN \u2013 zero duty",
      TRUE                                     ~ "Other")
  }
  b_regime_cols <- c("MFN \u2013 zero duty"      = "#0b2e4f",
                     "MFN \u2013 duty payable"   = "#1d70b8",
                     "TCA preference used"        = "#00b0d8",
                     "GSP/DCTS preference used"   = "#7fb8e0",
                     "Entry regime unknown"       = "#999999",
                     "Other"                      = "#e74c3c")
  
  output$b_coderegime <- renderUI({
    req(input$b_go > 0, b_fresh())
    if (is.null(d("pref_breakdown_code")) ||
        !isolate(input$b_level) %in% c("HS4", "HS6", "CN8") ||
        length(isolate(input$b_codes)) != 1) return(NULL)
    div(class = "panel-card", style = "margin-top:16px;",
        p(class = "card-note",
          "Regime breakdown available once data/pref_breakdown_code.RDS is added to the pipeline."))
  })
  
  output$b_png <- downloadHandler(
    filename = function() paste0("PUR_trend_", input$partner, "_", input$b_level, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(b_gg())
      m <- b_meta()
      g_out <- b_gg()
      if (inherits(g_out$data$x, "Date"))
        suppressMessages(g_out <- g_out +
                           scale_x_date(date_labels = "%b %Y", date_breaks = "4 months",
                                        expand = ggplot2::expansion(mult = c(0.01, 0.03))))
      suppressWarnings(
        ggsave(file,
               plot = g_out +
                 labs(title = m$title, subtitle = m$subtitle, caption = m$caption,
                      y = if (pref_mode()) "\u20ac million" else "PUR (%)") +
                 theme(legend.position = "bottom",
                       plot.title = element_text(size = 15, face = "bold", colour = "#0b0c0c"),
                       plot.subtitle = element_text(size = 11, colour = "#505a5f", margin = margin(b = 10)),
                       plot.caption = element_text(size = 8.5, colour = "#6f777b", hjust = 0, margin = margin(t = 10)),
                       axis.title.y = element_text(size = 10, colour = "#505a5f"),
                       axis.text.x = element_text(angle = 45, hjust = 1, size = 8)),
               width = 12, height = 7, dpi = 200, bg = "white"))
    })
  
  output$b_caption <- renderText({
    if (input$b_go == 0) return("Choose options on the left, then press Create")
    if (!b_fresh()) return("Options changed \u2013 press Create to build this view")
    tbl <- if (isolate(input$b_output) == "Table") b_built() else NULL
    isolate({
      base <- paste0(
        pname(), " \u00B7 ",
        if (pref_mode()) "Preference regimes" else "PUR", " \u00B7 ",
        input$b_output, " \u00B7 ", input$b_level, " level \u00B7 ",
        if (input$b_output == "Graph" &&
            input$b_level %in% c("HS4", "HS6", "CN8", "SITC") &&
            is.null(switch(input$b_level,
                           "HS4"  = d("monthly_HS4_country"),
                           "HS6"  = d("monthly_HS6_country"),
                           "CN8"  = d("monthly_CN8_country"),
                           "SITC" = d("monthly_SITC_country"))))
          "all member states (state selection not applied at this level)"
        else if (length(input$b_country)) paste(input$b_country, collapse = ", ")
        else if (isTRUE(input$b_combined)) "all member states \u2013 total and individually"
        else "all member states \u2013 total", " \u00B7 ",
        if (length(input$b_years)) paste(input$b_years, collapse = ", ")
        else if (input$b_output == "Table")
          paste0(min(b_years_default), "\u2013", max(b_years_default), " \u00B7 latest full years")
        else "all years",
        if (isTRUE(input$b_byyear)) " (shown per year)"
        else if (input$b_output == "Table") " (annual average)"
        else " (combined)")
      miss <- attr(tbl, "missing")
      if (length(miss) > 0) {
        base <- paste0(base, " \u00B7 no recorded trade for this selection: ", paste(miss, collapse = ", "))
      }
      base
    })
  })
  
  ## money columns: 1 dp in millions, but a value under EUR 0.05m shows as
  ## "<0.1" rather than 0.0, so small trade isn't mistaken for none.
  ## Sorting and filtering still use the underlying number.
  money_render <- DT::JS(
    "function(data, type, row) {",
    "  if (type !== 'display') return data;",
    "  if (data === null || data === '') return '';",
    "  var v = parseFloat(data);",
    "  if (isNaN(v)) return data;",
    "  if (v > 0 && v < 0.05) return '<0.1';",
    "  if (v < 0 && v > -0.05) return '>-0.1';",
    "  return v.toLocaleString('en-GB', {minimumFractionDigits: 1, maximumFractionDigits: 1});",
    "}")
  
  money_defs <- function(dat, cols) {
    idx <- match(cols, names(dat)) - 1
    idx <- idx[!is.na(idx)]
    if (!length(idx)) return(list())
    list(list(targets = idx, render = money_render))
  }
  
  output$b_table <- DT::renderDataTable({
    req(input$b_go > 0, b_fresh())
    dat <- b_built()
    money <- grep("imports \\(\u20acm\\)", names(dat), value = TRUE)
    tbl <- DT::datatable(dat, rownames = FALSE, filter = "top",
                         options = list(pageLength = 15, scrollX = TRUE,
                                        columnDefs = money_defs(dat, money))) %>%
      DT::formatRound("PUR (%)", digits = 1)
    total_lbl <- "All member states \u2013 total"
    if (total_lbl %in% dat$`Member state` && any(dat$`Member state` != total_lbl)) {
      tbl <- tbl %>% DT::formatStyle("Member state", target = "row",
                                     fontWeight = DT::styleEqual(total_lbl, "bold"))
    }
    tbl
  })
  
  output$b_dl <- downloadHandler(
    filename = function() paste0("PUR_table_", input$partner, "_", input$b_level, "_", Sys.Date(), ".xlsx"),
    content = function(file) {
      out <- if (pref_mode()) pref_built() else b_built()
      out <- cbind(Partner = pname(), as.data.frame(out))
      writexl::write_xlsx(out, file)
    }
  )
  
  ## ---- Revisions ----
  output$rev_sub <- renderText(paste0("How EU-published totals for imports from ",
                                      pname(), " have changed between publications"))
  
  output$Revision_graph <- renderPlotly({
    rev <- revisions_data()
    validate(need(nrow(rev) > 0, paste0("No revisions data yet for ", pname(),
                                        " - it builds up with each monthly update.")))
    ggplotly(
      ggplot(rev, aes(x = Date, y = Total_exports, fill = date_stamp,
                      text = paste0(comma(Total_exports)))) +
        geom_bar(position = "dodge", stat = "identity") +
        scale_x_discrete(labels = function(x) gsub("-\\d{2}$", "", x)) +
        scale_fill_manual(name = "Data difference",
                          values = c("#0b2e4f", "#1d70b8", "#5694c9", "#b8d8f2")) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.position = "right", axis.title.x = element_blank()) +
        ylab(paste0("EU imports from ", pname(), " (\u20ac)")) +
        scale_y_continuous(labels = scales::label_comma()),
      tooltip = "text")
  })
  
}

ui <- fluidPage(body)

shinyApp(ui = ui, server = server) 