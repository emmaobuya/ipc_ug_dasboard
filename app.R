# ==============================================================================
# Ebola IPC Assessment Analysis Dashboard -- full 3-tab version
# ==============================================================================
# Built to run as a shinylive (WebAssembly) app: everything happens in the
# browser. File upload always works fully offline/client-side. The ODK
# Central live-connect option performs a browser-side HTTP request to your
# server -- see the CORS caveat in odk_fetch_submissions_csv() in
# ipc_helpers.R and in README.md if that option doesn't work for you.
#
# Tabs: Outbreak Response, Summary View, Facility Deep Dive.
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(DT)
library(plotly)
library(leaflet)

source("ipc_helpers.R")

EMPTY_MSG <- function(type = "bar", msg = "No data") {
  plotly_empty(type = type) |> layout(title = msg)
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- page_navbar(
  title = "Ebola IPC Assessment Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = TRUE,

  sidebar = sidebar(
    width = 320,

    h5("\U0001F4C2 Data Source"),
    radioButtons("data_source_mode", NULL,
                 choices = c("\U0001F4C4 File Upload" = "file", "\U0001F310 ODK Central API" = "odk"),
                 selected = "file"),

    conditionalPanel(
      condition = "input.data_source_mode == 'file'",
      fileInput("data_file", "Upload ODK Central CSV export", accept = ".csv"),
      p(class = "text-muted small", "Processed entirely in your browser; never uploaded anywhere.")
    ),
    conditionalPanel(
      condition = "input.data_source_mode == 'odk'",
      textInput("odk_url", "Server URL", placeholder = "https://your-central-server.org"),
      textInput("odk_project", "Project ID", placeholder = "e.g. 4"),
      textInput("odk_form", "Form ID (xmlFormId)", placeholder = "e.g. uganda_evd_ipc_scorecard"),
      textInput("odk_email", "Email"),
      passwordInput("odk_password", "Password"),
      actionButton("odk_fetch", "\U0001F680 Fetch Data", class = "btn-primary"),
      p(class = "text-muted small",
        "Credentials are used only for this fetch, in your browser, and are never stored or saved. If this fails with a network error (not an auth error), your ODK Central server likely needs CORS enabled for this dashboard's URL -- ask your admin, or use File Upload instead.")
    ),

    hr(),
    h5("\U0001F50D View Controls"),
    dateInput("min_date", "Include data on/after", value = as.Date("2026-05-15")),
    selectizeInput("f_region", "Region", choices = NULL, multiple = TRUE),
    selectizeInput("f_district", "District", choices = NULL, multiple = TRUE),
    selectizeInput("f_subcounty", "Subcounty", choices = NULL, multiple = TRUE),
    selectizeInput("f_level", "Facility Level", choices = NULL, multiple = TRUE),
    selectizeInput("f_facility", "Facilities", choices = NULL, multiple = TRUE),

    checkboxInput("use_custom_baseline", "Use custom baseline anchor", value = FALSE),
    conditionalPanel(
      condition = "input.use_custom_baseline == true",
      dateInput("baseline_target_date", "Target baseline date", value = NA),
      numericInput("baseline_buffer_weeks", "\u00B1 weeks buffer", value = 2, min = 0, max = 12)
    ),
    p(class = "text-muted small",
      "Off (default): each facility's own earliest assessment is its baseline. On: pick a common date + buffer -- useful for comparing all facilities to a shared event like outbreak start."),

    radioButtons("time_axis", "Time Axis (Summary View trajectory chart)",
                 choices = c("Reporting Date", "Days Since Baseline", "Assessment #"),
                 selected = "Reporting Date"),

    hr(),
    h5("\U0001F6A8 Outbreak Settings"),
    p(class = "text-muted small",
      "Limits which domains feed the domain-level charts/tables below. Overall-score outputs (Readiness Index, indicator boxes) always use all domains."),
    selectizeInput("f_domains", NULL, choices = setNames(DOMAINS$id, DOMAINS$label),
                    selected = DOMAINS$id, multiple = TRUE)
  ),

  # ============================================================================
  # TAB 1: Outbreak Response
  # ============================================================================
  nav_panel(
    "\U0001F6A8 Outbreak Response",
    uiOutput("no_data_banner"),

    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "\U0001F534 Critical", value = textOutput("kpi_critical"), theme = "danger"),
      value_box(title = "\U0001F7E1 At Risk", value = textOutput("kpi_at_risk"), theme = "warning"),
      value_box(title = "\U0001F7E2 Ready", value = textOutput("kpi_ready"), theme = "success")
    ),

    card(card_header("\U0001F6A6 Outbreak Readiness Index"), plotlyOutput("readiness_chart", height = "500px")),
    card(card_header("\U0001F7E2\U0001F7E1\U0001F534 Assessment Domain Scores"), plotlyOutput("domain_heatmap", height = "500px")),
    card(
      card_header("\U0001F5FA\uFE0F Facility Map"),
      p(class = "text-muted small", "Requires latitude/longitude in the uploaded data."),
      leafletOutput("facility_map", height = "500px")
    ),
    card(card_header("\U0001F4CB Dispatch Decision Table"), DTOutput("dispatch_table"))
  ),

  # ============================================================================
  # TAB 2: Summary View
  # ============================================================================
  nav_panel(
    "\U0001F310 Summary View",
    uiOutput("no_data_banner_2"),

    layout_columns(
      col_widths = c(4, 4, 4, 4, 4, 4),
      value_box(title = "Facilities Tracked", value = textOutput("sv_kpi_facilities"), showcase = icon("hospital")),
      value_box(title = "Avg. Assessments / Facility", value = textOutput("sv_kpi_avg_assess"), showcase = icon("clipboard-list")),
      value_box(title = "Median \u0394 Total Score (since baseline)", value = textOutput("sv_kpi_median_delta"), showcase = icon("arrow-trend-up")),
      value_box(title = "Median Follow-up (days)", value = textOutput("sv_kpi_median_followup"), showcase = icon("calendar-days")),
      value_box(title = "Total Assessments", value = textOutput("sv_kpi_total_assess"), showcase = icon("list-check")),
      value_box(title = "Last 30 / Last 7 Days", value = textOutput("sv_kpi_recent"), showcase = icon("clock"))
    ),

    card(
      card_header("\U0001F4C8 Total Score Trajectory"),
      radioButtons("trend_filter", NULL, inline = TRUE,
                   choices = c("All", "Increasing", "Decreasing", "Static"), selected = "All"),
      plotlyOutput("trajectory_chart", height = "550px")
    ),

    card(
      card_header("\U0001F7E5\U0001F7E9 Change from Baseline (by domain)"),
      plotlyOutput("change_heatmap", height = "500px")
    ),

    card(
      card_header("\U0001F4CB Facility Progress Summary"),
      DTOutput("progress_table")
    )
  ),

  # ============================================================================
  # TAB 3: Facility Deep Dive
  # ============================================================================
  nav_panel(
    "\U0001F3E5 Facility Deep Dive",
    uiOutput("no_data_banner_3"),

    layout_columns(
      col_widths = c(6, 6),
      selectInput("dd_facility", "\U0001F3E5 Facility to Inspect", choices = NULL),
      selectInput("dd_assessment", "\U0001F4CB Specific Assessment", choices = NULL)
    ),

    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Assessments Since Baseline", value = textOutput("dd_kpi_since_baseline"), showcase = icon("chart-line")),
      value_box(title = "Assessments (Last 30 Days)", value = textOutput("dd_kpi_last30"), showcase = icon("calendar-days")),
      value_box(title = "Assessments (Last 7 Days)", value = textOutput("dd_kpi_last7"), showcase = icon("clock"))
    ),

    layout_columns(
      col_widths = c(7, 5),
      card(card_header("\U0001F4CA Single Assessment Snapshot"), plotlyOutput("snapshot_chart", height = "500px")),
      card(card_header("Domain Detail"), DTOutput("snapshot_table"))
    ),

    card(
      card_header("\U0001F4CA Domain Change Since Baseline (Diverging View)"),
      plotlyOutput("dd_diverging_chart", height = "500px")
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  # ==== DATA LOADING ====
  odk_data <- reactiveVal(NULL)

  observeEvent(input$odk_fetch, {
    req(input$odk_url, input$odk_project, input$odk_form, input$odk_email, input$odk_password)
    tryCatch({
      d <- odk_fetch_submissions_csv(input$odk_url, input$odk_project, input$odk_form,
                                      input$odk_email, input$odk_password)
      odk_data(d)
      showNotification(paste0("Loaded ", nrow(d), " submissions from ODK Central."), type = "message")
    }, error = function(e) {
      showNotification(paste("ODK Central fetch failed:", conditionMessage(e)), type = "error", duration = NULL)
    })
  })

  raw_data <- reactive({
    if (input$data_source_mode == "odk") {
      req(odk_data())
      odk_data()
    } else {
      req(input$data_file)
      read_csv(input$data_file$datapath, col_types = cols(.default = "c"), show_col_types = FALSE)
    }
  })

  clean_data <- reactive({
    req(raw_data())
    clean_ipc_data(raw_data())
  })

  has_data <- reactive({
    (input$data_source_mode == "file" && !is.null(input$data_file)) ||
      (input$data_source_mode == "odk" && !is.null(odk_data()))
  })

  no_data_banner_ui <- function() {
    if (!has_data()) {
      div(class = "alert alert-info", "Load data via the sidebar's \U0001F4C2 Data Source panel to get started.")
    }
  }
  output$no_data_banner <- renderUI(no_data_banner_ui())
  output$no_data_banner_2 <- renderUI(no_data_banner_ui())
  output$no_data_banner_3 <- renderUI(no_data_banner_ui())

  # ==== FILTER CHOICES ====
  observeEvent(clean_data(), {
    df <- clean_data()
    if (nrow(df) == 0) return()
    updateSelectizeInput(session, "f_region", choices = sort(unique(na.omit(df$region))), server = FALSE)
    updateSelectizeInput(session, "f_district", choices = sort(unique(na.omit(df$district))), server = FALSE)
    updateSelectizeInput(session, "f_subcounty", choices = sort(unique(na.omit(df$subcounty))), server = FALSE)
    updateSelectizeInput(session, "f_level", choices = sort(unique(na.omit(df$facility_level))), server = FALSE)
    updateSelectizeInput(session, "f_facility", choices = sort(unique(na.omit(df$facility))), server = FALSE)
    if (any(!is.na(df$date_of_assessment))) {
      updateDateInput(session, "baseline_target_date", value = min(df$date_of_assessment, na.rm = TRUE))
    }
  })

  # ==== VIEW CONTROLS FILTER ====
  filtered_data <- reactive({
    df <- clean_data()
    if (nrow(df) == 0) return(df)
    if (!is.na(input$min_date)) df <- df %>% filter(is.na(date_of_assessment) | date_of_assessment >= input$min_date)
    if (length(input$f_region) > 0) df <- df %>% filter(region %in% input$f_region)
    if (length(input$f_district) > 0) df <- df %>% filter(district %in% input$f_district)
    if (length(input$f_subcounty) > 0) df <- df %>% filter(subcounty %in% input$f_subcounty)
    if (length(input$f_level) > 0) df <- df %>% filter(facility_level %in% input$f_level)
    if (length(input$f_facility) > 0) df <- df %>% filter(facility %in% input$f_facility)
    df
  })

  selected_domain_ids <- reactive({
    ids <- as.integer(input$f_domains)
    if (length(ids) == 0) DOMAINS$id else ids
  })

  # ==== BASELINE-SCOPED DATA (Summary View + Facility Deep Dive) ====
  baseline_scope <- reactive({
    compute_baseline_scoped(
      filtered_data(),
      use_custom = isTRUE(input$use_custom_baseline),
      target_date = input$baseline_target_date,
      buffer_weeks = if (is.null(input$baseline_buffer_weeks)) 2 else input$baseline_buffer_weeks
    )
  })

  facility_summary <- reactive({
    df <- baseline_scope()
    if (nrow(df) == 0) return(df)

    baseline_rows <- df %>%
      filter(date_of_assessment == baseline_date) %>%
      group_by(facility) %>% slice_head(n = 1) %>% ungroup() %>%
      select(facility, region, district, subcounty, baseline_date, baseline_total = overall_pct)

    latest_rows <- df %>%
      group_by(facility) %>% slice_max(date_of_assessment, n = 1, with_ties = FALSE) %>% ungroup() %>%
      select(facility, latest_date = date_of_assessment, latest_total = overall_pct, latest_category = overall_category)

    n_assessments <- df %>% count(facility, name = "n_assessments")

    baseline_rows %>%
      left_join(latest_rows, by = "facility") %>%
      left_join(n_assessments, by = "facility") %>%
      mutate(
        delta_total = round(latest_total - baseline_total, 1),
        followup_days = as.numeric(latest_date - baseline_date)
      )
  })

  # ============================================================================
  # TAB 1: Outbreak Response
  # ============================================================================
  latest_snapshot <- reactive({
    df <- filtered_data()
    if (nrow(df) == 0) return(df)
    latest_per_facility(df)
  })

  domain_long_selected <- reactive({
    snap <- latest_snapshot()
    if (nrow(snap) == 0) return(snap)
    build_domain_long(snap) %>% filter(domain_id %in% selected_domain_ids())
  })

  output$kpi_critical <- renderText({ snap <- latest_snapshot(); if (nrow(snap) == 0) "--" else sum(snap$overall_category == "Critical", na.rm = TRUE) })
  output$kpi_at_risk  <- renderText({ snap <- latest_snapshot(); if (nrow(snap) == 0) "--" else sum(snap$overall_category == "At Risk", na.rm = TRUE) })
  output$kpi_ready    <- renderText({ snap <- latest_snapshot(); if (nrow(snap) == 0) "--" else sum(snap$overall_category == "Ready", na.rm = TRUE) })

  output$readiness_chart <- renderPlotly({
    snap <- latest_snapshot()
    if (nrow(snap) == 0) return(EMPTY_MSG())
    snap <- snap %>% arrange(overall_pct)
    plot_ly(snap, y = ~factor(facility, levels = facility), x = ~overall_pct,
            type = "bar", orientation = "h",
            marker = list(color = CATEGORY_COLORS[snap$overall_category]),
            text = ~paste0(round(overall_pct, 1), "%"), textposition = "outside") |>
      layout(yaxis = list(title = "", tickfont = list(size = 9)),
             xaxis = list(title = "Overall IPC Score (%)", range = c(0, 105)),
             height = max(500, nrow(snap) * 18))
  })

  output$domain_heatmap <- renderPlotly({
    dl <- domain_long_selected()
    if (nrow(dl) == 0) return(EMPTY_MSG("heatmap"))
    plot_ly(dl, x = ~label, y = ~facility, z = ~pct, type = "heatmap",
            colorscale = list(c(0, "#dc3545"), c(0.5, "#ffc107"), c(1, "#28a745")),
            zmin = 0, zmax = 100,
            text = ~paste0(facility, " / ", label, ": ", pct, "%"), hoverinfo = "text") |>
      layout(xaxis = list(title = "", tickangle = -35), yaxis = list(title = "", tickfont = list(size = 9)),
             height = max(500, n_distinct(dl$facility) * 18))
  })

  output$facility_map <- renderLeaflet({
    snap <- latest_snapshot()
    if (nrow(snap) == 0 || all(is.na(snap$lat))) {
      return(leaflet() %>% addTiles() %>% setView(lng = 32.5, lat = 1.4, zoom = 6))
    }
    snap <- snap %>% filter(!is.na(lat), !is.na(lon))
    leaflet(snap) %>% addTiles() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat, radius = ~pmax(4, overall_pct / 8),
        color = ~CATEGORY_COLORS[overall_category], fillOpacity = 0.8, stroke = FALSE,
        popup = ~paste0("<b>", facility, "</b><br>Status: ", overall_category,
                         "<br>Score: ", overall_pct, "%<br>Last assessed: ", format(date_of_assessment, "%d %b %Y"))
      )
  })

  output$dispatch_table <- renderDT({
    snap <- latest_snapshot()
    if (nrow(snap) == 0) return(datatable(data.frame(Message = "No data"), rownames = FALSE))
    dl <- domain_long_selected()
    critical_gaps <- dl %>% filter(category == "Critical") %>%
      group_by(facility) %>% summarise(critical_domains = paste(label, collapse = "; "), .groups = "drop")
    out <- snap %>%
      left_join(critical_gaps, by = "facility") %>%
      transmute(Facility = facility, Readiness = overall_pct,
                `Domains with Critical Gaps` = ifelse(is.na(critical_domains), "None", critical_domains),
                `Last Assessed` = format(date_of_assessment, "%d %b %Y"), Status = overall_category) %>%
      arrange(Readiness)
    datatable(out, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE, filter = "top") %>%
      formatStyle("Status", backgroundColor = styleEqual(names(CATEGORY_COLORS), unname(CATEGORY_COLORS)),
                  color = styleEqual(c("Critical", "At Risk", "Ready"), c("white", "black", "white")), fontWeight = "bold")
  })

  # ============================================================================
  # TAB 2: Summary View
  # ============================================================================
  output$sv_kpi_facilities <- renderText({ nrow(facility_summary()) })
  output$sv_kpi_avg_assess <- renderText({
    fsum <- facility_summary(); if (nrow(fsum) == 0) return("--")
    round(mean(fsum$n_assessments, na.rm = TRUE), 1)
  })
  output$sv_kpi_median_delta <- renderText({
    fsum <- facility_summary(); if (nrow(fsum) == 0) return("--")
    d <- median(fsum$delta_total, na.rm = TRUE)
    paste0(ifelse(d >= 0, "+", ""), round(d, 1), " pts")
  })
  output$sv_kpi_median_followup <- renderText({
    fsum <- facility_summary(); if (nrow(fsum) == 0) return("--")
    round(median(fsum$followup_days, na.rm = TRUE), 0)
  })
  output$sv_kpi_total_assess <- renderText({ nrow(baseline_scope()) })
  output$sv_kpi_recent <- renderText({
    bs <- baseline_scope(); if (nrow(bs) == 0) return("--")
    d30 <- sum(bs$date_of_assessment >= Sys.Date() - 30, na.rm = TRUE)
    d7  <- sum(bs$date_of_assessment >= Sys.Date() - 7, na.rm = TRUE)
    paste0(d30, " / ", d7)
  })

  facility_trend <- reactive({
    fsum <- facility_summary()
    if (nrow(fsum) == 0) return(fsum)
    fsum %>% mutate(trend = case_when(
      is.na(delta_total) ~ "Static",
      delta_total > 1  ~ "Increasing",
      delta_total < -1 ~ "Decreasing",
      TRUE ~ "Static"
    )) %>% select(facility, trend)
  })

  output$trajectory_chart <- renderPlotly({
    df <- baseline_scope()
    if (nrow(df) == 0) return(EMPTY_MSG("scatter"))

    plot_df <- df %>% left_join(facility_trend(), by = "facility")
    if (!is.null(input$trend_filter) && input$trend_filter != "All") {
      plot_df <- plot_df %>% filter(trend == input$trend_filter)
    }
    if (nrow(plot_df) == 0) return(EMPTY_MSG("scatter", "No facilities match this trend filter"))

    plot_df <- plot_df %>%
      group_by(facility) %>% arrange(date_of_assessment) %>%
      mutate(days_since_baseline = as.numeric(date_of_assessment - baseline_date),
             assessment_number = row_number()) %>%
      ungroup()

    x_vals <- switch(input$time_axis,
                      "Reporting Date" = plot_df$date_of_assessment,
                      "Days Since Baseline" = plot_df$days_since_baseline,
                      "Assessment #" = plot_df$assessment_number)

    plot_ly(plot_df, x = x_vals, y = ~overall_pct, color = ~facility,
            type = "scatter", mode = "lines+markers",
            text = ~facility, hoverinfo = "text+y", showlegend = FALSE) |>
      layout(
        shapes = list(
          list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 50, y1 = 50, line = list(dash = "dash", color = "gray")),
          list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 80, y1 = 80, line = list(dash = "dash", color = "gray"))
        ),
        xaxis = list(title = input$time_axis),
        yaxis = list(title = "Overall IPC Score (%)", range = c(0, 105))
      )
  })

  output$change_heatmap <- renderPlotly({
    df <- baseline_scope()
    if (nrow(df) == 0) return(EMPTY_MSG("heatmap"))

    baseline_domains <- df %>% filter(date_of_assessment == baseline_date) %>%
      group_by(facility) %>% slice_head(n = 1) %>% ungroup() %>%
      build_domain_long() %>% select(facility, domain_id, label, baseline_pct = pct)

    latest_domains <- df %>% group_by(facility) %>% slice_max(date_of_assessment, n = 1, with_ties = FALSE) %>% ungroup() %>%
      build_domain_long() %>% select(facility, domain_id, label, latest_pct = pct)

    change_df <- baseline_domains %>%
      inner_join(latest_domains, by = c("facility", "domain_id", "label")) %>%
      mutate(delta = round(latest_pct - baseline_pct, 1)) %>%
      filter(domain_id %in% selected_domain_ids())

    if (nrow(change_df) == 0) return(EMPTY_MSG("heatmap"))

    plot_ly(change_df, x = ~label, y = ~facility, z = ~delta, type = "heatmap",
            colorscale = list(c(0, "#dc3545"), c(0.5, "#ffc107"), c(1, "#28a745")),
            zmid = 0, zmin = -100, zmax = 100,
            text = ~paste0(facility, " / ", label, ": ", ifelse(delta >= 0, "+", ""), delta), hoverinfo = "text") |>
      layout(xaxis = list(title = "", tickangle = -35), yaxis = list(title = "", tickfont = list(size = 9)),
             height = max(500, n_distinct(change_df$facility) * 18))
  })

  output$progress_table <- renderDT({
    fsum <- facility_summary()
    if (nrow(fsum) == 0) return(datatable(data.frame(Message = "No data"), rownames = FALSE))
    out <- fsum %>%
      transmute(Facility = facility, Assessments = n_assessments,
                `Baseline Date` = format(baseline_date, "%d %b %Y"),
                `Latest Date` = format(latest_date, "%d %b %Y"),
                `Follow-up (days)` = followup_days,
                `Baseline Total` = baseline_total, `Latest Total` = latest_total,
                `Change (pts)` = delta_total, `Latest Status` = latest_category)
    datatable(out, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE, filter = "top") %>%
      formatStyle("Latest Status", backgroundColor = styleEqual(names(CATEGORY_COLORS), unname(CATEGORY_COLORS)),
                  color = styleEqual(c("Critical", "At Risk", "Ready"), c("white", "black", "white")), fontWeight = "bold")
  })

  # ============================================================================
  # TAB 3: Facility Deep Dive
  # ============================================================================
  observeEvent(filtered_data(), {
    df <- filtered_data()
    if (nrow(df) == 0) return()
    updateSelectInput(session, "dd_facility", choices = sort(unique(df$facility)))
  })

  dd_facility_all <- reactive({
    req(input$dd_facility)
    filtered_data() %>% filter(facility == input$dd_facility) %>% arrange(desc(date_of_assessment))
  })

  observeEvent(list(input$dd_facility, filtered_data()), {
    df <- dd_facility_all()
    if (nrow(df) == 0) return()
    labels <- paste0(format(df$date_of_assessment, "%d %b %Y"), " (", df$assessment_round, ")")
    choices <- setNames(as.character(df$date_of_assessment), labels)
    updateSelectInput(session, "dd_assessment", choices = choices, selected = choices[1])
  })

  dd_selected_row <- reactive({
    req(input$dd_assessment)
    dd_facility_all() %>% filter(as.character(date_of_assessment) == input$dd_assessment) %>% slice_head(n = 1)
  })

  dd_facility_scoped <- reactive({
    req(input$dd_facility)
    baseline_scope() %>% filter(facility == input$dd_facility)
  })

  dd_baseline_row <- reactive({
    df <- dd_facility_scoped()
    if (nrow(df) == 0) return(df)
    df %>% filter(date_of_assessment == baseline_date) %>% slice_head(n = 1)
  })

  output$dd_kpi_since_baseline <- renderText({ nrow(dd_facility_scoped()) })
  output$dd_kpi_last30 <- renderText({
    df <- dd_facility_scoped(); if (nrow(df) == 0) return("--")
    sum(df$date_of_assessment >= Sys.Date() - 30, na.rm = TRUE)
  })
  output$dd_kpi_last7 <- renderText({
    df <- dd_facility_scoped(); if (nrow(df) == 0) return("--")
    sum(df$date_of_assessment >= Sys.Date() - 7, na.rm = TRUE)
  })

  output$snapshot_chart <- renderPlotly({
    row <- dd_selected_row()
    if (nrow(row) == 0) return(EMPTY_MSG())
    dl <- build_domain_long(row) %>% filter(domain_id %in% selected_domain_ids())
    plot_ly(dl, x = ~label, y = ~pct, type = "bar", marker = list(color = CATEGORY_COLORS[dl$category])) |>
      layout(
        shapes = list(
          list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 50, y1 = 50, line = list(dash = "dash", color = "gray")),
          list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 80, y1 = 80, line = list(dash = "dash", color = "gray"))
        ),
        xaxis = list(title = "", tickangle = -35), yaxis = list(title = "Score (%)", range = c(0, 105))
      )
  })

  output$snapshot_table <- renderDT({
    row <- dd_selected_row(); brow <- dd_baseline_row()
    if (nrow(row) == 0) return(datatable(data.frame(Message = "No data"), rownames = FALSE))
    dl <- build_domain_long(row) %>% select(domain_id, label, score = pct, category)
    if (nrow(brow) > 0) {
      bl <- build_domain_long(brow) %>% select(domain_id, baseline_pct = pct)
      dl <- dl %>% left_join(bl, by = "domain_id") %>% mutate(change = round(score - baseline_pct, 1))
    } else {
      dl$change <- NA
    }
    out <- dl %>% transmute(Domain = label, Score = score, Change = change, Status = category)
    datatable(out, options = list(pageLength = 15, dom = "t"), rownames = FALSE) %>%
      formatStyle("Status", backgroundColor = styleEqual(names(CATEGORY_COLORS), unname(CATEGORY_COLORS)),
                  color = styleEqual(c("Critical", "At Risk", "Ready"), c("white", "black", "white")), fontWeight = "bold")
  })

  output$dd_diverging_chart <- renderPlotly({
    row <- dd_selected_row(); brow <- dd_baseline_row()
    if (nrow(row) == 0 || nrow(brow) == 0) return(EMPTY_MSG("bar", "No baseline available for this facility under the current settings"))
    dl <- build_domain_long(row) %>% select(domain_id, label, pct)
    bl <- build_domain_long(brow) %>% select(domain_id, baseline_pct = pct)
    change_df <- dl %>% inner_join(bl, by = "domain_id") %>%
      mutate(delta = round(pct - baseline_pct, 1)) %>%
      filter(domain_id %in% selected_domain_ids()) %>%
      arrange(delta)
    if (nrow(change_df) == 0) return(EMPTY_MSG("bar"))

    bar_colors <- ifelse(change_df$delta < 0, "#dc3545", ifelse(change_df$delta > 0, "#28a745", "#ffc107"))
    plot_ly(change_df, y = ~factor(label, levels = label), x = ~delta, type = "bar", orientation = "h",
            marker = list(color = bar_colors)) |>
      layout(xaxis = list(title = "Change in domain score (percentage points) since baseline"), yaxis = list(title = ""))
  })
}

shinyApp(ui, server)
